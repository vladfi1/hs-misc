{-# LANGUAGE GADTs, DataKinds, TypeFamilies, TypeOperators, PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module FunctionProofs where

import Data.Type.Equality
import Data.Singletons

newtype FunEq (f :: TyFun k k1 -> *) (g :: TyFun k k1 -> *) =
  FunEq { funEq :: forall a. Sing a -> f @@ a :=: g @@ a}

{- can't do this without TypeInType
data (:=:) (a :: k) (b :: k) where
  FunEq :: (forall a. Sing a -> f @@ a :=: g @@ a) -> f :=: g
  TypeEq :: a :=: a
-}

-- Functional Extensionality
type family (:=:) (a :: k) (b :: k) :: * where
  f :=: g = FunEq f g
  a :=: b = a :~: b

infix 4 :=:

data Inverse f b where
  Inverse :: Sing a -> Inverse f (f @@ a)

data Surjective f = Surjective { surjective :: forall b. Sing b -> Inverse f b }

data Injective f = Injective { injective :: forall a b. Sing a -> Sing b -> f @@ a :~: f @@ b -> a :~: b }

data Associative (op :: TyFun k (TyFun k k -> *) -> *) =
  Associative { associative :: forall a b c. Sing a -> Sing b -> Sing c -> op @@ (op @@ a @@ b) @@ c :~: op @@ a @@ (op @@ b @@ c) }

data Commutative (op :: TyFun k (TyFun k k -> *) -> *) =
  Commutative { commutative :: forall a b. Sing a -> Sing b -> (op @@ a @@ b) :~: (op @@ b @@ a) }


