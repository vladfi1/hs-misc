{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Vec where

import Prelude hiding (replicate, reverse)
import Zippable
import Nats

data Vec n a where
  Nil :: Vec Z a
  Cons :: a -> Vec n a -> Vec (S n) a

instance Functor (Vec n) where
  fmap f Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Foldable (Vec n) where
  foldr f b Nil = b
  foldr f b (Cons a l) = f a (foldr f b l)

replicate :: SNat n -> a -> Vec n a
replicate SZ _ = Nil
replicate (SS n) a = Cons a (replicate n a)

instance Zippable (Vec n) where
  Nil <**> Nil = Nil
  Cons f fs <**> Cons a as = Cons (f a) (fs <**> as)

-- ZipVec semantics
instance (ReifyNat n) => Applicative (Vec n) where
  pure = replicate nat
  (<*>) = (<**>)

