{-# LANGUAGE TypeOperators, TypeFamilies, DataKinds, PolyKinds, GADTs #-}

module Tuples where

--import Data.HList

data Z

data x :. y = x :. y

type family FMap (f :: * -> *) (a :: [*]) :: [*]

{-
type instance FMap f Z = Z
type instance FMap f (x :. y) = (f x) :. (FMap f y)

type instance FMap f (x, y)       = (f x, f y)
type instance FMap f (x, y, z)    = (f x, f y, f z)
type instance FMap f (x, y, z, w) = (f x, f y, f z, f w)
-}

type instance FMap f '[] = '[]
type instance FMap f (e ': l) = (f e) ': (FMap f l)

--stype instance FMap f (`[]` :: *) = (`[]` :: *)

