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
  fmap f (STensor v) = STensor (fmap (fmap f) v)

instance Foldable (Tensor dim) where
  foldr f b (ZTensor a) = f a b
  foldr f b (STensor v) = foldr (flip $ foldr f) b v
  
  foldl f b (ZTensor a) = f b a
  foldl f b (STensor v) = foldl (foldl f) b v

instance Traversable (Tensor dim) where
  sequenceA (ZTensor a) = ZTensor <$> a
  sequenceA (STensor v) = STensor <$> traverse sequenceA v

instance Zippable (Tensor dim) where
  ZTensor f <**> ZTensor a = ZTensor (f a)
  STensor fs <**> STensor as = STensor ((<**>) <$> fs <**> as)

fill :: Dim dim -> a -> Tensor dim a
fill RNil = ZTensor
fill (n :& dims) = STensor . Vec.replicate n . fill dims

instance ReifyDim dim => Applicative (Tensor dim) where
  pure = fill dim
  (<*>) = (<**>)

instance Show a => Show (Tensor dim a) where
  show (ZTensor a) = show a
  show (STensor v) = show v

fromVec :: Vec n a -> Tensor '[n] a
fromVec v = STensor $ fmap ZTensor v

toVec :: Tensor '[n] a -> Vec n a
toVec (STensor v) = fmap (\(ZTensor a) -> a) v

append :: Tensor (n ': dim) a -> Tensor (m ': dim) a -> Tensor (n :+: m ': dim) a
append (STensor n) (STensor m) = STensor $ Vec.append n m

dot a b = foldl (+) 0 $ (*) <$> a <**> b

mv :: Num a => Tensor (n ': dim) a -> Tensor dim a -> Tensor '[n] a
mv (STensor m) v = fromVec $ dot v <$> m

