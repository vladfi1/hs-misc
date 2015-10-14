{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Tensor where

import Data.Vinyl
import Nats
import Vec
import Zippable

type Dim = Rec SNat

class ReifyDim dim where
  dim :: Dim dim

instance ReifyDim '[] where
  dim = RNil

instance (ReifyDim dim, ReifyNat n) => ReifyDim (n ': dim) where
  dim = nat :& dim

data Tensor dim a where
  ZTensor :: a -> Tensor '[] a
  STensor :: Vec n (Tensor dim a) -> Tensor (n ': dim) a

instance Functor (Tensor dim) where
  fmap f (ZTensor a) = ZTensor (f a)
  fmap f (STensor l) = STensor (fmap (fmap f) l)

instance Zippable (Tensor dim) where
  ZTensor f <**> ZTensor a = ZTensor (f a)
  STensor fs <**> STensor as = STensor ((<**>) <$> fs <**> as)

fill :: Dim dim -> a -> Tensor dim a
fill RNil = ZTensor
fill (n :& dims) = STensor . Vec.replicate n . fill dims

instance ReifyDim dim => Applicative (Tensor dim) where
  pure = fill dim
  (<*>) = (<**>)



