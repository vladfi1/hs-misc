{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE DataKinds, PolyKinds #-}

module VarArgs where

import Data.Vinyl

type family Curried (f :: k -> *) (l :: [k]) (r :: *) :: * where
  Curried f '[] r = r
  Curried f (a ': l) r = f a -> Curried f l r

class Curry (l :: [k]) where
  curry' :: (Rec f l -> a) -> Curried f l a

instance Curry '[] where
  curry' f = f RNil

instance Curry l => Curry (a ': l) where
  curry' f fa = curry' (\args -> f $ fa :& args)

{-
type family Uncurry (f :: k -> *) (g :: *) :: * where
  Uncurry (f a -> b) = 
  Uncurry r = 
-}
