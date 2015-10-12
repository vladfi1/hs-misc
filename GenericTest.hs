{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs, DataKinds, PolyKinds, TypeFamilies, TypeOperators, ConstraintKinds, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module GenericTest where

--import GHC.Prim (Constraint)

import Data.Constraint

import qualified GHC.Generics as GHC
import Generics.SOP

import TypeLevel

data Nat = Z | S Nat deriving (Show, GHC.Generic)

instance Generic Nat
instance HasDatatypeInfo Nat

data A = MkA B | ANil deriving (Show, GHC.Generic)
data B = MkB A | BNil deriving (Show, GHC.Generic)

instance Generic A
instance Generic B
instance HasDatatypeInfo A

class Contains l a where
  get :: NP f l -> f a

instance Contains (a ': l) a where
  get (fa :* _) = fa

instance {-# OVERLAPPABLE #-} Contains l a => Contains (b ': l) a where
  get (_ :* l) = get l

type family All' (c :: a -> Constraint) (l :: k) :: Constraint where
  All' c '[] = ()
  All' c (a ': l) = (All' c a, All' c l)
  All' c a = c a

type family MapCode (xs :: [k]) :: [[[*]]] where
  MapCode '[] = '[]
  MapCode (x ': xs) = Code x ': MapCode xs

type family Unique (xs :: [k]) :: [k] where
  Unique '[] = '[]
  Unique (x ': xs) = x ': Unique (Remove x xs)

type family Remove (x :: k) (xs :: [k]) where
  Remove x '[] = '[]
  Remove x (x ': xs) = Remove x xs
  Remove x (y ': xs) = y ': Remove x xs

type AllGeneric x = (Generic x, All2 AllGeneric (Code x))

type Grammar xs = (All Generic xs, All' (Contains xs) (MapCode xs))

grammar :: Dict (Grammar '[A, B])
grammar = Dict

