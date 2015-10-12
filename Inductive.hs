--{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inductive where

import Prelude (
  ($), id, (.),
  const, flip,
  Functor (..),
  Enum (..),
  Num (..))

import Data.Monoid
import Data.Foldable
import Control.Monad
import Control.Applicative

newtype Bool = Bool { runBool :: forall a. a -> a -> a }

true :: Bool
true = Bool $ \a b -> a

false :: Bool
false = Bool $ \a b -> b

if_ :: Bool -> a -> a -> a
if_ = runBool

not :: Bool -> Bool
not b = if_ b false true

and :: Bool -> Bool -> Bool
and a b = if_ a b false

or :: Bool -> Bool -> Bool
or a b = if_ a true b

xor :: Bool -> Bool -> Bool
xor a b = if_ a (not b) b

cnot :: Bool -> Bool -> Bool
cnot a b = if_ a b (not b)

{-
instance Eq Bool where
  a == b = if_ a b (not b)
  a /= b = if_ a (not b) b
-}

newtype Pair a b = Pair { runPair :: forall c. (a -> b -> c) -> c }

pair :: a -> b -> Pair a b
pair a b = Pair $ \p -> p a b

first :: Pair a b -> a
first (Pair p) = p $ \a b -> a

second :: Pair a b -> b
second (Pair p) = p $ \a b -> b

newtype Either a b = Either { runEither :: forall c. (a->c) -> (b->c) -> c }

left :: a -> Either a b
left a = Either $ \l r -> l a

right :: b -> Either a b
right b = Either $ \l r -> r b

--data List a = Nil | Cons (Pair a (List a))

instance Functor (Either l) where
  fmap f (Either lr) = lr left (right . f)

newtype Nat = Nat { runNat :: forall a. (a -> a) -> a -> a}

zero :: Nat
zero = Nat $ const id

instance Enum Nat where
  succ (Nat n) = Nat $ \f -> f . n f
  
  pred (Nat n) = Nat $ \f a -> second $ n (\p -> runPair p (\b _ -> pair (f b) b)) (pair a a)
  
  toEnum 0 = zero
  toEnum n = succ $ toEnum (pred n)
  
  fromEnum (Nat n) = n succ 0

instance Num Nat where
  (Nat n) + (Nat m) = Nat $ \f -> (n f) . (m f)
  (Nat n) * (Nat m) = Nat $ n . m

  negate _ = 0
  abs n = n
  signum _ = 1
  
  fromInteger 0 = zero
  fromInteger n = succ $ fromInteger (pred n)

infixr 8 ^
(Nat n) ^ (Nat m) = Nat $ m n

-- list represented as a right fold
newtype ListR a = ListR { runList :: forall b. (a -> b -> b) -> b -> b }

nil :: ListR a
nil = ListR $ \_ b -> b

cons :: a -> ListR a -> ListR a
cons a (ListR l) = ListR $ \f b -> f a (l f b)

length :: ListR a -> Nat
length = foldr (\_ n -> succ n) zero

instance Functor ListR where
  fmap f l = foldr (\a b -> cons (f a) b) nil l

instance Foldable ListR where
  foldr f b (ListR l) = l f b

instance Monoid (ListR a) where
  mempty = nil
  --mappend l1 l2 = foldr cons l2 l1
  mappend (ListR l) = l cons

instance Monad ListR where
  return a = cons a nil
  (>>=) = flip foldMap

instance Applicative ListR where
  pure = return
  (<*>) = ap



filter :: (a -> Bool) -> ListR a -> ListR a
filter f l =
  foldr
    (\a b ->
      if_ (f a)
        (cons a b)
        b)
    nil l

insertBy :: (a -> a -> Bool) -> a -> ListR a -> ListR a
insertBy leq a l =
  flip runPair
    (\l1 b ->
      if_ b l1 $
        cons a l1
    ) $
    foldr
      (\a1 p ->
        runPair p (\l1 b ->
          if_ b (pair (cons a1 l1) b) $
            if_ (leq a1 a)
              (pair (cons a1 $ cons a l1) true)
              (pair (cons a1 l1) false)))
      (pair nil false) l

insertionSortBy leq l =
  runList l (insertBy leq) nil

quickSort :: (a -> a -> Bool) -> ListR a -> ListR a
quickSort leq l =
  

