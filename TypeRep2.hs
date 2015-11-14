{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE TypeInType #-}

module TypeRep2 where

import Data.Type.Equality

-- heterogenous equality
data Het (a :: k1) (b :: k2) where
  Het :: Het a a

het2homo :: Het a b -> a :~: b
het2homo Het = Refl

homo2het :: a :~: b -> Het a b
homo2het Refl = Het

data TypeRep :: k -> * where
  BoolRep :: TypeRep Bool
  MaybeRep :: TypeRep Maybe
  AppRep :: TypeRep f -> TypeRep a -> TypeRep (f a)

equal :: TypeRep a -> TypeRep b -> Maybe (Het a b)
equal BoolRep BoolRep = Just Het
equal MaybeRep MaybeRep = Just Het

equal (AppRep f a) (AppRep g b) = do
  Het <- equal f g
  Het <- equal a b
  return Het

equal _ _ = Nothing

instance TestEquality TypeRep where
  testEquality a b = het2homo <$> equal a b

