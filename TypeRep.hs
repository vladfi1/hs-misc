{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module TypeRep where

import Data.Type.Equality

data TypeRep t where
  --AppRep :: TypeRep f -> TypeRep a -> TypeRep (f a)
  BoolRep :: TypeRep Bool
  IntRep :: TypeRep Int
  MaybeRep :: TypeRep a -> TypeRep (Maybe a)
  ListRep :: TypeRep a -> TypeRep [a]
  

class Typeable t where
  typeRep :: TypeRep t

instance Typeable Bool where
  typeRep = BoolRep

instance Typeable Int where
  typeRep = IntRep

instance Typeable a => Typeable (Maybe a) where
  typeRep = MaybeRep typeRep

instance Typeable a => Typeable [a] where
  typeRep = ListRep typeRep

instance TestEquality TypeRep where
{- We aren't allowed to be GADT-like in kind parameters
  testEquality (AppRep f a) (AppRep g b) = do
    Refl <- testEquality f g
    Refl <- testEquality a b
    return Refl
-}

  testEquality BoolRep BoolRep = Just Refl
  testEquality BoolRep _ = Nothing

  testEquality IntRep IntRep = Just Refl
  testEquality IntRep _ = Nothing
  
  testEquality (MaybeRep a) (MaybeRep b) = do
    Refl <- testEquality a b
    return Refl
  
  testEquality (MaybeRep _) _ = Nothing
  
  testEquality (ListRep a) (ListRep b) = do
    Refl <- testEquality a b
    return Refl

--data KindRep 

{-
type family Bool' :: k where
  Bool' = Bool

type family Maybe' :: k where
  Maybe' = Maybe

data TypeRep' :: k -> * where
  BoolRep' :: TypeRep' Bool'
  MaybeRep' :: TypeRep' Maybe'
  AppRep' :: forall (f :: k1 -> k2) (a :: k1). TypeRep' f -> TypeRep' a -> TypeRep' (f a)

instance TestEquality TypeRep' where
  testEquality BoolRep' BoolRep' = Just Refl
  testEquality BoolRep' _ = Nothing
  
  testEquality MaybeRep' MaybeRep' = Just Refl
  testEquality MaybeRep' _ = Nothing
  
  testEquality (AppRep' f a) (AppRep' g b) = do
    Refl <- testEquality f g
    Refl <- testEquality a b
    return Refl
-}


data TypeRep' (t :: k) where
  AppRep' :: forall (f :: k1 -> k2) (a :: k1). TypeRep' f -> TypeRep' a -> TypeRep' (f a)

{-
instance TestEquality TypeRep' where
  testEquality (AppRep' f a) (AppRep' g b) = do
    Refl <- testEquality f g
    Refl <- testEquality a b
    return Refl
-}

