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
import Data.Vinyl

import Utils
import List
import TensorHMatrix
import GHC.TypeLits
import DAGIO

import Data.Default
import DefaultM

import Numeric.LinearAlgebra (Numeric)

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

--deriving instance (AllC (CompC Show (Flip (Linear outDim) a)) inDims) => Show (Affine outDim inDims a)

{-
instance (Default a, Usable a, SListI inDims, All KnownNat inDims, KnownNat outDim) => Default (Affine a outDim inDims) where
  def = Affine def (cpure_NP (Proxy::Proxy KnownNat) def)
-}

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

-- should be able to just use SListI here?
-- would have to write out the proof SListI dims -> All ReifyNat dims
class (SListI dims, All KnownNat dims) => Blah dims
instance (SListI dims, All KnownNat dims) => Blah dims

class (Generic t, KnownNat (Size s t), SListI (MapSize2 s (Code t)), AllF Blah (MapSize2 s (Code t))) => HasParams s t
instance (Generic t, KnownNat (Size s t), SListI (MapSize2 s (Code t)), AllF Blah (MapSize2 s (Code t))) => HasParams s t

instance (Default a, Usable a, HasParams s t) => DefaultM IO (EncodeParams s a t) where
  defM = EncodeParams <$> sequence'_NP (cpure_NP (Proxy::Proxy Blah) (Comp defM))

newtype Encoder s a t = Encoder { runEncoder :: t -> IO (Encoding s a t) }

class EncodeRec ts ts' s t where
  encodeRec :: (Floating a, Usable a) => Rec (EncodeParams s a) ts -> Rec (Encoder s a) ts' -> Encoder s a t

instance {-# OVERLAPPABLE #-}
  (Generic t, KnownNat (Size s t), Find ts t, All2 (Find ts') (Code t))
  => EncodeRec ts ts' s t where
    encodeRec params encoders = Encoder f where
      EncodeParams params' = index find params :: EncodeParams s _ t
      encoders' = cpure_POP (Proxy::Proxy (Find ts')) (Fn $ Comp . runEncoder (index find encoders) . unI)
      f t = do
        children <- sequence_SOP' (ap_SOP encoders' (from t))
        let childReprs = getReprSOP children
        parent <- encodeParent params' childReprs
        return $ Generic parent children

makeEncoders :: forall ts ts' s a. (Floating a, Usable a, SListI ts', All (EncodeRec ts ts' s) ts') =>
  Rec (EncodeParams s a) ts -> Rec (Encoder s a) ts'
makeEncoders params = fix (np2Rec . f) where
  f encoders = cpure_NP (Proxy::Proxy (EncodeRec ts ts' s)) (encodeRec params encoders)

makeEncoder :: forall proxy ts ts' s a. (Floating a, Usable a, SListI ts', All (EncodeRec ts ts' s) ts') =>
  Rec (EncodeParams s a) ts -> proxy ts' -> (forall t. Find ts' t => t -> IO (Encoding s a t))
makeEncoder params _ = f where
  encoders :: Rec (Encoder s a) ts'
  encoders = makeEncoders params
  f t = runEncoder (index find encoders) t

--data DecodeParams a t = DecodeParams (Affine a (Len
