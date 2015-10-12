{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module VinylTest where

import Data.Vinyl
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel

import GHC.TypeLits
import Data.Proxy

import Map

infixr 2 &:
infixr 2 &.

(&.) = (:&)

x &: xs = (Identity x) :& xs

xs = (1 :: Int) &: "1" &: RNil

f x = [x]

fxs = rmap (f . getIdentity) xs

(=.) :: KnownSymbol s => proxy '(s, a) -> a -> ElField '(s, a)
_ =. x = Field x

name = Proxy :: Proxy '("name", a)

--andrew :: FieldRec '[ '("name", String) ]
andrew = name =. "andrew" &.
         RNil

--rgetField = getField . rget

rgetField :: ('(s, a) ∈ rs) => proxy '(s, a) -> FieldRec rs -> a
rgetField proxy = getField . rget proxy

rtail :: Rec f (r ': rs) -> Rec f rs
rtail (x :& xs) = xs

class RSubset' (rs :: [k]) (ss :: [k]) where
  rsubset' :: Rec f ss -> Rec f rs

instance RSubset' '[] ss where
  rsubset' _ = RNil

instance (RSubset' rs ss) => RSubset' rs (s ': ss) where
  rsubset' = rsubset' . rtail

-- This one is harder
--instance (r ∈ ss, RSubset' rs ss) => RSubset' (r ': rs) ss

-- This instance is missing from Vinyl
--instance (is' ~ Map 'S is, RSubset rs ss is) => RSubset rs (s ': ss) is' where
--  rsubset = _

