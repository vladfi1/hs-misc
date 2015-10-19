{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Vec where

import Prelude hiding (replicate, reverse, concat)
import Zippable
import Nats

data Vec n a where
  VNil :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

index :: LT n -> Vec n a -> a
index ZLT (VCons a _) = a
index (SLT n) (VCons _ l) = index n l

instance Functor (Vec n) where
  fmap f VNil = VNil
  fmap f (VCons a l) = VCons (f a) (fmap f l)

instance Foldable (Vec n) where
  foldr f b VNil = b
  foldr f b (VCons a l) = f a (foldr f b l)
  
  foldl f b VNil = b
  foldl f b (VCons a l) = foldl f (f b a) l

instance Traversable (Vec n) where
  sequenceA VNil = pure VNil
  sequenceA (VCons a l) = VCons <$> a <*> sequenceA l

replicate :: SNat n -> a -> Vec n a
replicate SZ _ = VNil
replicate (SS n) a = VCons a (replicate n a)

instance Zippable (Vec n) where
  VNil <**> VNil = VNil
  VCons f fs <**> VCons a as = VCons (f a) (fs <**> as)

-- ZipVec semantics
instance (ReifyNat n) => Applicative (Vec n) where
  pure = replicate nat
  (<*>) = (<**>)

appendVec :: Vec n a -> Vec m a -> Vec (n :+: m) a
appendVec VNil v = v
appendVec (VCons a v1) v2 = VCons a (appendVec v1 v2)

