{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module TypeRep2 where

import Data.Type.Equality
import TypeEq

data TypeRep :: k -> * where
  BoolRep :: TypeRep Bool
  MaybeRep :: TypeRep Maybe
  AppRep :: TypeRep f -> TypeRep a -> TypeRep (f a)

equal' :: TypeRep a -> TypeRep b -> Maybe (Het a b)

equal' BoolRep BoolRep = Just Het
equal' MaybeRep MaybeRep = Just Het

equal' (AppRep f a) (AppRep g b) = do
  Het <- equal' f g
  Het <- equal' a b
  return Het

equal' _ _ = Nothing

instance Equality TypeRep where
  equal = equal'

{-
instance Equality TypeRep => TestEquality TypeRep where
  testEquality a b = het2homo <$> equal a b
-}
