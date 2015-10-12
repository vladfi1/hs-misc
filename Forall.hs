{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Forall where

import Data.Functor.Identity

unwrap :: (forall a. Identity (a->a)) -> forall a. a->a
unwrap i = runIdentity i

-- we show there isn't any magic in runIdentity by defining it ourselves
unwrap' :: (forall a. Identity (a->a)) -> forall a. a->a
unwrap' i = runId i
  where runId (Identity a) = a

-- we'd like to be generic in the type function a -> (a -> a)
unwrapF :: (forall a. Identity (f a)) -> forall a. f a
unwrapF i = runIdentity i

type family Id (a :: *) :: * where
  Id a = a -> a

unwrapId :: (forall a. Identity (Id a)) -> forall a. Id a
unwrapId i = runIdentity i

type Pair x y = forall z. (x -> y -> z) -> z

data List a = Nil | Cons (Pair a (List a))

mk_pair :: x -> y -> Pair x y
mk_pair x y = \f -> f x y
get_fst x y = x
get_snd x y = y

pair12 = mk_pair 1 2

infixr 5 <:
x <: y = Cons $ mk_pair x y

toList :: List a -> [a]
toList Nil = []
toList (Cons pair) = pair (\x xs -> x : toList xs)
