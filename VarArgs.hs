{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module VarArgs where

import Data.Vinyl
import Data.Vinyl.Functor

import Prelude hiding (curry, uncurry)

type family Curried (f :: k -> *) (l :: [k]) (r :: k) :: * where
  Curried f '[] r = Identity r
  Curried f (a ': l) r = f a -> Curried f l r

-- use singletons instead of a custom class?
class Curry f l r c | f l r -> c, f c -> l, f c -> r where
  curry' :: (Rec f l -> Identity r) -> c
  uncurry' :: c -> Rec f l -> Identity r

curry f = curry' (Identity . f)
uncurry f = getIdentity . (uncurry' f)

instance Curry f '[] r (Identity r) where
  curry' f = f RNil
  uncurry' f RNil = f

instance Curry f l r c => Curry f (a ': l) r (f a -> c) where
  curry' f fa = curry' (\args -> f $ fa :& args)
  uncurry' f (fa :& args) = uncurry' (f fa) args


