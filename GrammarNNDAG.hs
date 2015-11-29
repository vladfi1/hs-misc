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

newtype Repr a dim = Repr { runRepr :: Node (Tensor a '[dim]) }
  --deriving (Show)

newtype Linear a outDim inDim = Linear (Node (Tensor a '[outDim, inDim]))
  --deriving (Show)

instance (Default a, Usable a, IntegralN outDim, SingI outDim, SingI inDim) => DefaultM IO (Linear a outDim inDim) where
  defM = Linear <$> defM

linear :: Numeric a => Linear a outDim inDim -> Repr a inDim -> IO (Node (Tensor a '[outDim]))
linear (Linear m) (Repr v) = makeBinary mv m v

data Affine a outDim inDims = Affine (Node (Tensor a '[outDim])) (NP (Linear a outDim) inDims)

instance (Default a, Usable a, IntegralN outDim, SingI outDim, SListI inDims, All SingI inDims) => DefaultM IO (Affine a outDim inDims) where
  defM = Affine <$> defM <*> sequence'_NP (cpure_NP (Proxy::Proxy SingI) (Comp defM))

affine :: (Usable a, IntegralN outDim, SingI outDim) => Affine a outDim inDims -> NP (Repr a) inDims -> IO (Node (Tensor a '[outDim]))
affine (Affine bias weights) inputs =
  foldM (makeBinary (+)) bias =<< (map runRepr . collapse_NP <$> (sequence'_NP $ liftA2_NP' linear' weights inputs))
  where linear' m v = Comp $ K . Repr <$> linear m v

type family MapSize s ts where
  MapSize s '[] = '[]
  MapSize s (t ': ts) = Size s t ': MapSize s ts

type family MapSize2 s (tss :: [[*]]) where
  MapSize2 s '[] = '[]
  MapSize2 s (ts ': tss) = MapSize s ts ': MapSize2 s tss

encodeParent :: (Floating a, Usable a, IntegralN outDim, SingI outDim) =>
  NP (Affine a outDim) inDims -> SOP (Repr a) inDims -> IO (Repr a outDim)
encodeParent params (SOP sop) = Repr <$> (makeUnary (tmap tanh) =<< (collapse_NS $ liftA2_NS' affine' params sop))
  where affine' p i = K $ affine p i

data Encoding s a t where
  Primitive :: Repr a (Size s t) -> Encoding s a t
  Generic :: (Generic t) => Repr a (Size s t) -> SOP (Encoding s a) (Code t) -> Encoding s a t

getRepr :: Encoding s a t -> Repr a (Size s t)
getRepr (Primitive repr) = repr
getRepr (Generic repr _) = repr

-- no way to write these with combinators?
getReprNP :: NP (Encoding s a) ts -> NP (Repr a) (MapSize s ts)
getReprNP Nil = Nil
getReprNP (e :* es) = getRepr e :* getReprNP es

getReprSOP' :: NS (NP (Encoding s a)) ts -> NS (NP (Repr a)) (MapSize2 s ts)
getReprSOP' (S es) = S $ getReprSOP' es
getReprSOP' (Z es) = Z $ getReprNP es

getReprSOP :: SOP (Encoding s a) ts -> SOP (Repr a) (MapSize2 s ts)
getReprSOP (SOP sop) = SOP $ getReprSOP' sop

newtype EncodeParams s a t =
  EncodeParams { runEncodeParams :: NP (Affine a (Size s t)) (MapSize2 s (Code t)) }

--deriving instance (SListI (MapSize2 (Code t))) => Show (EncodeParams t a)

class (Generic t, KnownSize s t, All2 SingI (MapSize2 s (Code t))) => HasParams s t
instance (Generic t, KnownSize s t, All2 SingI (MapSize2 s (Code t))) => HasParams s t

instance (Default a, Usable a, HasParams s t, IntegralS s) => DefaultM IO (EncodeParams s a t) where
  defM = EncodeParams <$> sequence'_NP (cpure_NP (Proxy::Proxy (All SingI)) (Comp defM))

newtype Encoder s a t = Encoder { runEncoder :: t -> IO (Encoding s a t) }

encodeRec :: forall ts s a t. (Floating a, Usable a, KnownSize s t, IntegralS s) =>
  Rec (Encoder s a) ts -> Dict (Contained ts) t -> EncodeParams s a t -> Encoder s a t

encodeRec encoders Dict (EncodeParams params) = Encoder f where
  encoders' = cpure_POP (Proxy::Proxy (Find ts)) (Fn $ Comp . runEncoder (index find encoders) . unI)
  f t = do
    children <- sequence_SOP' (ap_SOP encoders' (from t))
    let childReprs = getReprSOP children
    parent <- encodeParent params childReprs
    return $ Generic parent children

makeEncoders :: forall p g s a. (Floating a, Usable a, IntegralS s, All (KnownSize s) g) =>
  Complete p g -> NP (EncodeParams s a) g -> Rec (Encoder s a) p -> Rec (Encoder s a) (p :++ g)
makeEncoders complete params prim = fix f where
  f encoders = rAppend prim $ np2Rec (cliftA2_NP (Proxy::Proxy (KnownSize s)) (encodeRec encoders) (unAll_NP complete) params)

makeEncoder complete params prim = f where
  encoders = makeEncoders complete params prim
  f t = runEncoder (index find encoders) t

newtype Repr' s a t = Repr' { runRepr' :: Node (Tensor a '[Size s t]) }

instance (Default a, Usable a, KnownSize s t, IntegralS s) => DefaultM IO (Repr' s a t) where
  defM = Repr' <$> defM

newtype LinearIn s a t t' = LinearIn (Node (Tensor a '[Size s t', Size s t]))

instance (Default a, Usable a, IntegralS s, KnownSize s t, KnownSize s t') => DefaultM IO (LinearIn s a t t') where
  defM = LinearIn <$> defM

linearIn :: Numeric a => LinearIn s a t t' -> Repr' s a t -> IO (Repr' s a t')
linearIn (LinearIn m) (Repr' v) = Repr' <$> makeBinary mv m v

data AffineIn s a t t' = AffineIn (Repr' s a t') (LinearIn s a t t')

instance (Default a, Usable a, IntegralS s, KnownSize s t, KnownSize s t') => DefaultM IO (AffineIn s a t t') where
  defM = AffineIn <$> defM <*> defM

affineIn :: (Floating a, Usable a, IntegralS s, KnownSize s t') => AffineIn s a t t' -> Repr' s a t -> IO (Repr' s a t')
affineIn (AffineIn (Repr' bias) weights) input = do
  Repr' l <- linearIn weights input
  b <- makeBinary (+) bias l
  t <- makeUnary (tmap tanh) b
  return $ Repr' t

data Affine' a outDim inDim =
  Affine' (Node (Tensor a '[outDim])) (Node (Tensor a '[outDim, inDim]))

instance (Default a, Usable a, IntegralN outDim, SingI outDim, SingI inDim) => DefaultM IO (Affine' a outDim inDim) where
  defM = Affine' <$> defM <*> defM

affine' :: (Floating a, Usable a, IntegralN outDim, SingI outDim) => Affine' a outDim inDim -> Repr a inDim -> IO (Repr a outDim)
affine' (Affine' bias weight) (Repr input) = do
  l <- makeBinary mv weight input
  b <- makeBinary (+) bias l
  Repr <$> makeUnary (tmap tanh) b

data DecodeParams s a t =
  DecodeParams (Affine' a (Length (Code t)) (Size s t)) (POP (AffineIn s a t) (Code t))

instance (Default a, Usable a, KnownCode s t, KnownSize s t, KnownSizes s t) => DefaultM IO (DecodeParams s a t) where
  defM = DecodeParams <$> defM <*> sequence'_POP (cpure_POP (Proxy::Proxy (KnownSize s)) (Comp defM))

decodeParent :: forall s a t. (Real a, Floating a, Usable a, All2 (KnownSize s) (Code t), KnownCode s t) =>
  DecodeParams s a t -> Repr' s a t -> IO (SOP (Repr' s a) (Code t))
decodeParent (DecodeParams aff params) parent = do
  Repr node <- affine' aff (Repr $ runRepr' parent)
  Vector v <- evalNode node
  let weights = map toRational $ Vector.toList v
  
  let children = np2ns . unPOP $ cliftA_POP (Proxy::Proxy (KnownSize s)) (Comp . flip affineIn parent) params
  
  child <- sample $ zip children weights
  sequence'_SOP $ SOP child

newtype Decoder s a t = Decoder { runDecoder :: Repr' s a t -> IO t }

class All2 (KnownSize s) (Code t) => KnownSizes s t
instance All2 (KnownSize s) (Code t) => KnownSizes s t

class SingI (FromInteger (Length (Code t)) :: k) => KnownCode (s :: k -> *) t
instance SingI (FromInteger (Length (Code t)) :: k) => KnownCode (s :: k -> *) t

decodeRec :: forall ts s a t. (Real a, Floating a, Usable a, KnownSizes s t, KnownCode s t) =>
  Rec (Decoder s a) ts -> Dict (Contained ts) t -> DecodeParams s a t -> Decoder s a t

decodeRec decoders Dict params = Decoder f where
  decoders' = cpure_POP (Proxy::Proxy (Find ts)) (Fn $ Comp . (I <$>) . runDecoder (index find decoders))
  f repr = do
    childReprs <- decodeParent params repr
    children <- sequence_SOP' (ap_SOP decoders' childReprs)
    return $ to children

makeDecoders :: forall p g s a. (Real a, Floating a, Usable a, All (And (KnownSizes s) (KnownCode s)) g) =>
  Complete p g -> NP (DecodeParams s a) g -> Rec (Decoder s a) p -> Rec (Decoder s a) (p :++ g)
makeDecoders complete params prim = fix f where
  f decoders = rAppend prim $ np2Rec (cliftA2_NP (Proxy::Proxy (And (KnownSizes s) (KnownCode s))) (decodeRec decoders) (unAll_NP complete) params)

newtype AnyDecoder s a ts = AnyDecoder { runAnyDecoder :: forall t. Find ts t => Repr' s a t -> IO t }

makeDecoder :: forall p g s a. (Real a, Floating a, Usable a, All (And (KnownSizes s) (KnownCode s)) g) =>
  Complete p g -> NP (DecodeParams s a) g -> Rec (Decoder s a) p -> AnyDecoder s a (p :++ g)
makeDecoder complete params prim = AnyDecoder f where
  decoders = makeDecoders complete params prim
  f :: forall t. Find (p :++ g) t => Repr' s a t -> IO t
  f t = runDecoder (index find decoders) t
