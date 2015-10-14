{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Constraints where

import Data.Proxy

data Dict c where
  Dict :: c => Dict c

newtype Implies a b = Sub (a => Dict b)

sub :: a => Implies a b -> (b => r) -> r
sub (Sub Dict) r = r

transitivity :: Implies a b -> Implies b c -> Implies a c
transitivity ab bc = Sub $ sub ab $ sub bc Dict

newtype Forall c = Forall (forall a. Dict (c a))

class Trivial a
instance Trivial a

all :: Forall Trivial
all = Forall Dict

