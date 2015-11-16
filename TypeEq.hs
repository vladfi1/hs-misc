{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module TypeEq where

import Data.Type.Equality

-- heterogenous equality
data Het (a :: k1) (b :: k2) where
  Het :: Het a a

instance Show (Het a b) where
  show Het = "Het"

het2homo :: Het a b -> a :~: b
het2homo Het = Refl

homo2het :: a :~: b -> Het a b
homo2het Refl = Het

hsym :: Het a b -> Het b a
hsym Het = Het

htrans :: Het a b -> Het b c -> Het a c
htrans Het Het = Het

hcastWith :: Het a b -> a -> b
hcastWith Het a = a

-- for some reason this class isn't very useful
class Equality (f :: forall k. k -> *) where
  equal :: f a -> f b -> Maybe (Het a b)


