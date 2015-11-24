{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}

module GrammarNNDAG where

import Control.Monad (foldM)
import Data.Function (fix)

import Generics.SOP
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.Constraint
import Generics.SOP.Dict
import Data.Vinyl hiding (Dict)
import Data.Singletons.Prelude ((:++))
import Utils
import List
import Grammar
import TensorHMatrix
import DAGIO
import VinylUtils

import Data.Default
import DefaultM

import Numeric.LinearAlgebra (Numeric)
import GHC.TypeLits

type family Size s t :: Nat

newtype Repr a dim = Repr { runRepr :: Node (Tensor a '[dim]) }
  --deriving (Show)

newtype Linear a outDim inDim = Linear (Node (Tensor a '[outDim, inDim]))
  --deriving (Show)

instance (Default a, Usable a, KnownNat outDim, KnownNat inDim) => DefaultM IO (Linear a outDim inDim) where
  defM = Linear <$> defM

linear :: Numeric a => Linear a outDim inDim -> Repr a inDim -> IO (Node (Tensor a '[outDim]))
linear (Linear m) (Repr v) = makeBinary mv m v

data Affine a outDim inDims = Affine (Node (Tensor a '[outDim])) (NP (Linear a outDim) inDims)

instance (Default a, Usable a, KnownNat outDim, SListI inDims, All KnownNat inDims) => DefaultM IO (Affine a outDim inDims) where
  defM = Affine <$> defM <*> sequence'_NP (cpure_NP (Proxy::Proxy KnownNat) (Comp defM))

affine :: (Usable a, KnownNat outDim) => Affine a outDim inDims -> NP (Repr a) inDims -> IO (Node (Tensor a '[outDim]))
affine (Affine bias weights) inputs =
  foldM (makeBinary (+)) bias =<< (map runRepr . collapse_NP <$> (sequence'_NP $ liftA2_NP' linear' weights inputs))
  where linear' m v = Comp $ K . Repr <$> linear m v

type family MapSize s ts where
  MapSize s '[] = '[]
  MapSize s (t ': ts) = Size s t ': MapSize s ts

type family MapSize2 s (tss :: [[*]]) :: [[Nat]] where
  MapSize2 s '[] = '[]
  MapSize2 s (ts ': tss) = MapSize s ts ': MapSize2 s tss

encodeParent :: (Floating a, Usable a, KnownNat outDim) =>
  NP (Affine a outDim) inDims -> SOP (Repr a) inDims -> IO (Repr a outDim)

encodeParent params (SOP sop) = Repr <$> (makeUnary (tmap tanh) =<< (collapse_NS $ liftA2_NS' affine' params sop))
  where affine' p i = K $ affine p i

data Encoding s a t where
  Primitive :: Repr a (Size s t) -> Encoding s a t
  Generic :: (Generic t) => Repr a (Size s t) -> SOP (Encoding s a) (Code t) -> Encoding s a t

{-
instance Show a => Show (Encoding t a) where
  show (Primitive repr) = show repr
  show (Generic repr children) = showAsList . map unK . collapse_FSOP $ liftA_FSOP (FK . K . show) children
-}

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

class (Generic t, KnownNat (Size s t), All2 KnownNat (MapSize2 s (Code t))) => HasParams s t
instance (Generic t, KnownNat (Size s t), All2 KnownNat (MapSize2 s (Code t))) => HasParams s t

instance (Default a, Usable a, HasParams s t) => DefaultM IO (EncodeParams s a t) where
  defM = EncodeParams <$> sequence'_NP (cpure_NP (Proxy::Proxy (All KnownNat)) (Comp defM))

newtype Encoder s a t = Encoder { runEncoder :: t -> IO (Encoding s a t) }

encodeRec :: forall ts s a t. (Floating a, Usable a, KnownNat (Size s t)) =>
  Rec (Encoder s a) ts -> Dict (Contained ts) t -> EncodeParams s a t -> Encoder s a t

encodeRec encoders Dict (EncodeParams params) = Encoder f where
  encoders' = cpure_POP (Proxy::Proxy (Find ts)) (Fn $ Comp . runEncoder (index find encoders) . unI)
  f t = do
    children <- sequence_SOP' (ap_SOP encoders' (from t))
    let childReprs = getReprSOP children
    parent <- encodeParent params childReprs
    return $ Generic parent children

class KnownNat (Size s t) => KnownSize s t
instance KnownNat (Size s t) => KnownSize s t

makeEncoders :: forall p g s a. (Floating a, Usable a, All (KnownSize s) g) =>
  Complete p g -> NP (EncodeParams s a) g -> Rec (Encoder s a) p -> Rec (Encoder s a) (p :++ g)
makeEncoders complete params prim = fix f where
  f encoders = rAppend prim $ np2Rec (cliftA2_NP (Proxy::Proxy (KnownSize s)) (encodeRec encoders) (unAll_NP complete) params)

makeEncoder complete params prim = f where
  encoders = makeEncoders complete params prim
  f t = runEncoder (index find encoders) t

{-
type family Sum s ts where
  Sum s '[] = 0
  Sum s (t ': ts) = Size s t + Sum s ts

type family MapSum s (tss :: [[*]]) where
  MapSum s '[] = '[]
  MapSum s (ts ': tss) = Sum s ts ': MapSum s tss
-}

type family Length (ts :: [k]) :: Nat where
  Length '[] = 0
  Length (t ': ts) = 1 + Length ts


newtype Repr' s a t = Repr' { runRepr' :: Node (Tensor a '[Size s t]) }

newtype LinearIn s a t t' = LinearIn (Node (Tensor a '[Size s t', Size s t]))

linearIn :: Numeric a => LinearIn s a t t' -> Repr' s a t -> IO (Repr' s a t')
linearIn (LinearIn m) (Repr' v) = Repr' <$> makeBinary mv m v

data AffineIn s a t t' = AffineIn (Repr' s a t') (LinearIn s a t t')

affineIn :: (Floating a, Usable a, KnownNat (Size s t')) => AffineIn s a t t' -> Repr' s a t -> IO (Repr' s a t')
affineIn (AffineIn (Repr' bias) weights) input = do
  Repr' l <- linearIn weights input
  b <- makeBinary (+) bias l
  t <- makeUnary (tmap tanh) b
  return $ Repr' t

data DecodeParams s a t = DecodeParams (POP (AffineIn s a t) (Code t))

decodeParent :: POP (AffineIn s a t) (Code t) -> Repr' s a t -> IO (SOP (Repr' s a) (Code t))
decodeParent = undefined

{- exactly the same as an Encoding
data Decoding s a t where
  PrimitiveDecoding :: Repr a (Size s t) -> Decoding s a t
  GenericDecoding :: Generic t => Repr a (Size s t) ->
-}

newtype Decoder s a t = Decoder { runDecoder :: Repr' s a t -> IO t }

class DecodeRec ts ts' s t where
  decodeRec :: (Floating a, Usable a) => Rec (DecodeParams s a) ts -> Rec (Decoder s a) ts' -> Decoder s a t

instance {-# OVERLAPPABLE #-}
  (Generic t, KnownNat (Size s t), Find ts t, All2 (Find ts') (Code t))
  => DecodeRec ts ts' s t where
    decodeRec params decoders = Decoder f where
      DecodeParams params' = index find params :: DecodeParams s _ t
      decoders' = cpure_POP (Proxy::Proxy (Find ts')) (Fn $ Comp . (I <$>) . runDecoder (index find decoders))
      f repr = do
        childReprs <- decodeParent params' repr
        children <- sequence_SOP' (ap_SOP decoders' childReprs)
        return $ to children
