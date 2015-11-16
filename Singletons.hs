{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE GADTs, DataKinds, PolyKinds #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Singletons where

import Data.Type.Equality

data family Sing (a :: k) :: *

class SingI (a :: k) where
  sing :: Sing a

data Nat = Z | S Nat
  deriving (Eq, Ord, Show)

type SNat = (Sing :: Nat -> *)

data instance Sing (n :: Nat) where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

instance SingI Z where
  sing = SZ

instance SingI n => SingI (S n) where
  sing = SS sing

instance TestEquality SNat where
  testEquality SZ SZ = Just Refl
  testEquality (SS n) (SS m) = do
    Refl <- testEquality n m
    return Refl
  testEquality _ _ = Nothing

type SList = (Sing :: [a] -> *)

data instance Sing (l :: [a]) where
  SNil :: SList '[]
  SCons :: Sing a -> SList l -> SList (a ': l)

instance TestEquality (Sing :: a -> *) => TestEquality (Sing :: [a] -> *) where
  testEquality SNil SNil = Just Refl
  testEquality (SCons a1 l1) (SCons a2 l2) = do
    Refl <- testEquality a1 a2
    Refl <- testEquality l1 l2
    return Refl
  testEquality _ _ = Nothing
