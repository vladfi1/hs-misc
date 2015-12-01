{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}

module GrammarNNDAG where

import Control.Monad (foldM)
import Data.Function (fix)

import Generics.SOP hiding (SingI)
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.Constraint
import Generics.SOP.Dict
import Data.Vinyl hiding (Dict)
import Data.Singletons.Prelude hiding (All, And)
import Data.Singletons.Prelude.List (Length)

import Utils
import List
import Grammar
import TensorHMatrix
import DAGIO
import VinylUtils
import Random

import Data.Default
import DefaultM

import Numeric.LinearAlgebra (Numeric)
import qualified Data.Vector.Storable as Vector

type family Size (s :: k -> *) t :: k
type IntegralS (s :: k -> *) = IntegralK ('KProxy :: KProxy k)

class SingI (Size s t) => KnownSize s t
instance SingI (Size s t) => KnownSize s t

class All2 (KnownSize s) (Code t) => KnownSizes s t
instance All2 (KnownSize s) (Code t) => KnownSizes s t

newtype Repr a dim = Repr { runRepr :: Node (Tensor a '[dim]) }

newtype ReprT a s t = ReprT { runReprT :: Node (Tensor a '[Size s t]) }

instance (Default a, Usable a, IntegralS s, KnownSize s t) => DefaultM IO (ReprT a s t) where
  defM = ReprT <$> defM

newtype Linear a outDim inDim = Linear (Node (Tensor a '[outDim, inDim]))
newtype LinearT a s tOut tIn = LinearT (Node (Tensor a '[Size s tOut, Size s tIn]))

instance (Default a, Usable a, IntegralN outDim, SingI outDim, SingI inDim) => DefaultM IO (Linear a outDim inDim) where
  defM = Linear <$> defM

instance (Default a, Usable a, IntegralS s, KnownSize s tIn, KnownSize s tOut) => DefaultM IO (LinearT a s tOut tIn) where
  defM = LinearT <$> defM

linear :: Numeric a => Linear a outDim inDim -> Repr a inDim -> IO (Node (Tensor a '[outDim]))
linear (Linear m) (Repr v) = makeBinary mv m v

linearT :: Numeric a => LinearT a s tOut tIn -> ReprT a s tIn -> IO (ReprT a s tOut)
linearT (LinearT m) (ReprT v) = ReprT <$> makeBinary mv m v

data EncodeParams' a s parent children = EncodeParams' (ReprT a s parent) (NP (LinearT a s parent) children)

instance (Default a, Usable a, IntegralS s, KnownSize s parent, SListI children, All (KnownSize s) children) => DefaultM IO (EncodeParams' a s parent children) where
  defM = EncodeParams' <$> defM <*> sequence'_NP (cpure_NP (Proxy::Proxy (KnownSize s)) (Comp defM))

encodeParent' :: (Usable a, Floating a, IntegralS s, KnownSize s parent) =>
  EncodeParams' a s parent children -> NP (ReprT a s) children -> IO (ReprT a s parent)
encodeParent' (EncodeParams' (ReprT bias) weights) inputs = do
  let linearT' m v = Comp $ K <$> linearT m v
  children <- map runReprT . collapse_NP <$> (sequence'_NP $ liftA2_NP' linearT' weights inputs)
  parent <- foldM (makeBinary (+)) bias children
  ReprT <$> makeUnary (tmap tanh) parent

newtype EncodeParams a s t = EncodeParams { runEncodeParams :: NP (EncodeParams' a s t) (Code t) }

encodeParent :: (Floating a, Usable a, IntegralS s, KnownSize s t) =>
  EncodeParams a s t -> SOP (ReprT a s) (Code t) -> IO (ReprT a s t)
encodeParent (EncodeParams params) (SOP sop) = collapse_NS $ liftA2_NS' encode params sop
  where encode p cs = K $ encodeParent' p cs

data Encoding a s t where
  Primitive :: ReprT a s t -> Encoding a s t
  Generic :: ReprT a s t -> SOP (Encoding a s) (Code t) -> Encoding a s t

getRepr :: Encoding a s t -> ReprT a s t
getRepr (Primitive repr) = repr
getRepr (Generic repr _) = repr

instance (Default a, Usable a, IntegralS s, KnownSize s t, KnownSizes s t) => DefaultM IO (EncodeParams a s t) where
  defM = EncodeParams <$> sequence'_NP (cpure_NP (Proxy::Proxy (All (KnownSize s))) (Comp defM))

newtype Encoder a s t = Encoder { runEncoder :: t -> IO (Encoding a s t) }

encodeRec :: forall ts a s t. (Floating a, Usable a, KnownSize s t, IntegralS s) =>
  Rec (Encoder a s) ts -> Dict (Contained ts) t -> EncodeParams a s t -> Encoder a s t

encodeRec encoders Dict params = Encoder f where
  encoders' = cpure_POP (Proxy::Proxy (Find ts)) (Fn $ Comp . runEncoder (index find encoders) . unI)
  f t = do
    children <- sequence_SOP' (ap_SOP encoders' (from t))
    let childReprs = liftA_SOP' getRepr children
    parent <- encodeParent params childReprs
    return $ Generic parent children

makeEncoders :: forall p g a s. (Floating a, Usable a, IntegralS s, All (KnownSize s) g) =>
  Complete p g -> NP (EncodeParams a s) g -> Rec (Encoder a s) p -> Rec (Encoder a s) (p :++ g)
makeEncoders complete params prim = fix f where
  f encoders = rAppend prim $ np2Rec (cliftA2_NP (Proxy::Proxy (KnownSize s)) (encodeRec encoders) (unAll_NP complete) params)

newtype AnyEncoder a s ts = AnyEncoder { runAnyEncoder :: forall t. Find ts t => t -> IO (Encoding a s t) }

makeEncoder :: forall p g a s. (Floating a, Usable a, IntegralS s, All (KnownSize s) g) =>
  Complete p g -> NP (EncodeParams a s) g -> Rec (Encoder a s) p -> AnyEncoder a s (p :++ g)
makeEncoder complete params prim = AnyEncoder f where
  encoders = makeEncoders complete params prim
  f :: forall t. Find (p :++ g) t => t -> IO (Encoding a s t)
  f t = runEncoder (index find encoders) t

newtype LinearIn a s t t' = LinearIn (Node (Tensor a '[Size s t', Size s t]))

instance (Default a, Usable a, IntegralS s, KnownSize s t, KnownSize s t') => DefaultM IO (LinearIn a s t t') where
  defM = LinearIn <$> defM

linearIn :: Numeric a => LinearIn a s t t' -> ReprT a s t -> IO (ReprT a s t')
linearIn (LinearIn m) (ReprT v) = ReprT <$> makeBinary mv m v

data AffineIn a s t t' = AffineIn (ReprT a s t') (LinearIn a s t t')

instance (Default a, Usable a, IntegralS s, KnownSize s t, KnownSize s t') => DefaultM IO (AffineIn a s t t') where
  defM = AffineIn <$> defM <*> defM

affineIn :: (Floating a, Usable a, IntegralS s, KnownSize s t') => AffineIn a s t t' -> ReprT a s t -> IO (ReprT a s t')
affineIn (AffineIn (ReprT bias) weights) input = do
  ReprT l <- linearIn weights input
  b <- makeBinary (+) bias l
  t <- makeUnary (tmap tanh) b
  return $ ReprT t

data Affine' a outDim inDim =
  Affine' (Node (Tensor a '[outDim])) (Node (Tensor a '[outDim, inDim]))

instance (Default a, Usable a, IntegralN outDim, SingI outDim, SingI inDim) => DefaultM IO (Affine' a outDim inDim) where
  defM = Affine' <$> defM <*> defM

affine' :: (Floating a, Usable a, IntegralN outDim, SingI outDim) => Affine' a outDim inDim -> Repr a inDim -> IO (Repr a outDim)
affine' (Affine' bias weight) (Repr input) = do
  l <- makeBinary mv weight input
  b <- makeBinary (+) bias l
  Repr <$> makeUnary (tmap tanh) b

data DecodeParams a s t =
  DecodeParams (Affine' a (Length (Code t)) (Size s t)) (POP (AffineIn a s t) (Code t))

instance (Default a, Usable a, KnownCode s t, KnownSize s t, KnownSizes s t) => DefaultM IO (DecodeParams a s t) where
  defM = DecodeParams <$> defM <*> sequence'_POP (cpure_POP (Proxy::Proxy (KnownSize s)) (Comp defM))

decodeParent :: forall a s t. (Real a, Floating a, Usable a, All2 (KnownSize s) (Code t), KnownCode s t) =>
  DecodeParams a s t -> ReprT a s t -> IO (SOP (ReprT a s) (Code t))
decodeParent (DecodeParams aff params) parent = do
  Repr node <- affine' aff (Repr $ runReprT parent)
  Vector v <- evalNode node
  let weights = map toRational $ Vector.toList v
  
  let children = np2ns . unPOP $ cliftA_POP (Proxy::Proxy (KnownSize s)) (Comp . flip affineIn parent) params
  
  child <- sample $ zip children weights
  sequence'_SOP $ SOP child

newtype Decoder a s t = Decoder { runDecoder :: ReprT a s t -> IO t }

class SingI (FromInteger (Length (Code t)) :: k) => KnownCode (s :: k -> *) t
instance SingI (FromInteger (Length (Code t)) :: k) => KnownCode (s :: k -> *) t

decodeRec :: forall ts a s t. (Real a, Floating a, Usable a, KnownSizes s t, KnownCode s t) =>
  Rec (Decoder a s) ts -> Dict (Contained ts) t -> DecodeParams a s t -> Decoder a s t

decodeRec decoders Dict params = Decoder f where
  decoders' = cpure_POP (Proxy::Proxy (Find ts)) (Fn $ Comp . (I <$>) . runDecoder (index find decoders))
  f repr = do
    childReprs <- decodeParent params repr
    children <- sequence_SOP' (ap_SOP decoders' childReprs)
    return $ to children

makeDecoders :: forall p g a s. (Real a, Floating a, Usable a, All (And (KnownSizes s) (KnownCode s)) g) =>
  Complete p g -> NP (DecodeParams a s) g -> Rec (Decoder a s) p -> Rec (Decoder a s) (p :++ g)
makeDecoders complete params prim = fix f where
  f decoders = rAppend prim $ np2Rec (cliftA2_NP (Proxy::Proxy (And (KnownSizes s) (KnownCode s))) (decodeRec decoders) (unAll_NP complete) params)

newtype AnyDecoder a s ts = AnyDecoder { runAnyDecoder :: forall t. Find ts t => ReprT a s t -> IO t }

makeDecoder :: forall p g a s. (Real a, Floating a, Usable a, All (And (KnownSizes s) (KnownCode s)) g) =>
  Complete p g -> NP (DecodeParams a s) g -> Rec (Decoder a s) p -> AnyDecoder a s (p :++ g)
makeDecoder complete params prim = AnyDecoder f where
  decoders = makeDecoders complete params prim
  f :: forall t. Find (p :++ g) t => ReprT a s t -> IO t
  f t = runDecoder (index find decoders) t

--newtype AutoEncoder a s t = AutoEncoder { runAutoEncoder :: t -> IO (Node 

