{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}

module TypeRep3 where

import Data.Type.Equality
import Singletons
import TypeEq

type family TypeTable (s :: Nat) :: *

data Typed (t :: k)

class (SingI (Index t), Typed t ~ TypeTable (Index t)) => BaseType t where
  type Index t :: Nat

baseRep :: BaseType t => TypeRep t
baseRep = BaseRep sing  

data TypeRep (t :: k) where
  BaseRep :: BaseType t => Sing (Index t) -> TypeRep t
  AppRep :: TypeRep f -> TypeRep a -> TypeRep (f a)

class Typeable t where
  typeRep :: TypeRep t

-- requires UndecidableInstances
instance (Typeable f, Typeable a) => Typeable (f a) where
  typeRep = AppRep typeRep typeRep

instance {-# OVERLAPPABLE #-} BaseType t => Typeable t where
  typeRep = baseRep

equal' :: TypeRep a -> TypeRep b -> Maybe (Het a b)
equal' (BaseRep i1) (BaseRep i2) = do
  Refl <- testEquality i1 i2
  return Het

equal' (AppRep f a) (AppRep g b) = do
  Het <- equal' f g
  Het <- equal' a b
  return Het

equal' _ _ = Nothing

instance Equality TypeRep where
  equal = equal'

instance TestEquality TypeRep where
  testEquality a b = het2homo <$> equal' a b

-- these should be TH-generated?
-- how to guarantee unique IDs?
type instance TypeTable Z = Typed Int
instance BaseType Int where
  type Index Int = Z

type instance TypeTable (S Z) = Typed []
instance BaseType [] where
  type Index [] = S Z

type instance TypeTable (S (S Z)) = Typed (->)
instance BaseType (->) where
  type Index (->) = S (S Z)

