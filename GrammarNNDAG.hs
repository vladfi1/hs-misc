{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GrammarNNDAG where

import Control.Monad (foldM)

import Generics.SOP
import Generics.SOP.NP
import Generics.SOP.NS

import Utils
import List
import TensorHMatrix
import GHC.TypeLits
import DAGIO

import Data.Default
import DefaultM

import Numeric.LinearAlgebra (Numeric)

import Prelude hiding (zipWith)

type family Codes xs where
  Codes '[] = '[]
  Codes (x ': xs) = Code x ': Codes xs

class Neural a where
  type Size a :: Nat

newtype Repr a dim = Repr { runRepr :: Node (Tensor a '[dim]) }
  --deriving (Show)

newtype Linear a outDim inDim = Linear (Node (Tensor a '[outDim, inDim]))
  --deriving (Show)

instance (Default a, Usable a, KnownNat outDim, KnownNat inDim) => DefaultM IO (Linear a outDim inDim) where
  defM = Linear <$> defM

linear :: Numeric a => Linear a outDim inDim -> Repr a inDim -> IO (Node (Tensor a '[outDim]))
linear (Linear m) (Repr v) = makeBinary mv m v

data Affine a outDim inDims = Affine (Node (Tensor a '[outDim])) (NP (Linear a outDim) inDims)

instance (Default a, Usable a, KnownNat outDim, SingI inDims, All KnownNat inDims) => DefaultM IO (Affine a outDim inDims) where
  defM = Affine <$> defM <*> sequence'_NP (cpure_NP (Proxy::Proxy KnownNat) (Comp defM))

--deriving instance (AllC (CompC Show (Flip (Linear outDim) a)) inDims) => Show (Affine outDim inDims a)

{-
instance (Default a, Usable a, SingI inDims, All KnownNat inDims, KnownNat outDim) => Default (Affine a outDim inDims) where
  def = Affine def (cpure_NP (Proxy::Proxy KnownNat) def)
-}

affine :: (Usable a, KnownNat outDim) => Affine a outDim inDims -> NP (Repr a) inDims -> IO (Node (Tensor a '[outDim]))
affine (Affine bias weights) inputs =
  foldM (makeBinary (+)) bias =<< (map runRepr . collapse_NP <$> (sequence'_NP $ liftA2_NP' linear' weights inputs))
  where linear' m v = Comp $ K . Repr <$> linear m v

type family MapSize ts where
  MapSize '[] = '[]
  MapSize (t ': ts) = Size t ': MapSize ts

type family MapSize2 (tss :: [[*]]) :: [[Nat]] where
  MapSize2 '[] = '[]
  MapSize2 (ts ': tss) = MapSize ts ': MapSize2 tss

encodeParent :: (Floating a, Usable a, KnownNat outDim) =>
  NP (Affine a outDim) inDims -> SOP (Repr a) inDims -> IO (Repr a outDim)

encodeParent params (SOP sop) = Repr <$> (makeUnary (tmap tanh) =<< (collapse_NS $ liftA2_NS' affine' params sop))
  where affine' p i = K $ affine p i

data Encoding a t where
  Primitive :: Neural t => Repr a (Size t) -> Encoding a t
  Generic :: (Neural t, Generic t) => Repr a (Size t) -> SOP (Encoding a) (Code t) -> Encoding a t

{-
instance Show a => Show (Encoding t a) where
  show (Primitive repr) = show repr
  show (Generic repr children) = showAsList . map unK . collapse_FSOP $ liftA_FSOP (FK . K . show) children
-}

getRepr :: Encoding a t -> Repr a (Size t)
getRepr (Primitive repr) = repr
getRepr (Generic repr _) = repr

-- no way to write these with combinators?
getReprNP :: NP (Encoding a) ts -> NP (Repr a) (MapSize ts)
getReprNP Nil = Nil
getReprNP (e :* es) = getRepr e :* getReprNP es

getReprSOP' :: NS (NP (Encoding a)) ts -> NS (NP (Repr a)) (MapSize2 ts)
getReprSOP' (S es) = S $ getReprSOP' es
getReprSOP' (Z es) = Z $ getReprNP es

getReprSOP :: SOP (Encoding a) ts -> SOP (Repr a) (MapSize2 ts)
getReprSOP (SOP sop) = SOP $ getReprSOP' sop

newtype EncodeParams a t =
  EncodeParams { runEncodeParams :: NP (Affine a (Size t)) (MapSize2 (Code t)) }

--deriving instance (SingI (MapSize2 (Code t))) => Show (EncodeParams t a)

-- should be able to just use SingI here?
-- would have to write out the proof SingI dims -> All ReifyNat dims
class (SingI dims, All KnownNat dims) => Blah dims
instance (SingI dims, All KnownNat dims) => Blah dims

class (Neural t, Generic t, KnownNat (Size t), SingI (MapSize2 (Code t)), All Blah (MapSize2 (Code t))) => HasParams t
instance (Neural t, Generic t, KnownNat (Size t), SingI (MapSize2 (Code t)), All Blah (MapSize2 (Code t))) => HasParams t

instance (Default a, Usable a, HasParams t) => DefaultM IO (EncodeParams a t) where
  defM = EncodeParams <$> sequence'_NP (cpure_NP (Proxy::Proxy Blah) (Comp defM))

class Encode ts t where
  encode :: (Floating a, Usable a) => NP (EncodeParams a) ts -> t -> IO (Encoding a t)

instance {-# OVERLAPPABLE #-}
  (SingI (Code t), All2 (Encode ts) (Code t), KnownNat (Size t), Generic t, Neural t, Find ts t)
  => Encode ts t where
  encode :: forall a. (Floating a, Usable a) => NP (EncodeParams a) ts -> t -> IO (Encoding a t)
  encode params t = Generic <$> repr <*> children
    where
      {-
      cliftA_SOP' :: forall f g. (forall t'. Encode t' => f t' -> g t') ->
        SOP f (Code t) -> SOP g (Code t)
      -}

      encode' :: forall t'. (Encode ts t') => I t' -> (IO :.: Encoding a) t'
      encode' (I t') = Comp $ encode params t'

      cliftA_SOP' = cliftA_SOP (Proxy :: Proxy (Encode ts))

      children :: IO (SOP (Encoding a) (Code t))
      children = sequence'_SOP $ cliftA_SOP' encode' (from t)

      EncodeParams param = index (find :: Index ts t) (np2Rec params)
      repr = encodeParent param =<< (getReprSOP <$> children)
      --childReprs = liftA_SOP getRepr

--data DecodeParams a t = DecodeParams (Affine a (Len
