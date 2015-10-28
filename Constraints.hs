{-# LANGUAGE PolyKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

class ForallC c where
  forallC :: Forall c

forallImplies :: Implies (ForallC c) (c a)
forallImplies = Sub $
  case forallC of
    Forall dict -> dict

--withForall :: forall c b r. (forall a. (Dict (c a))) -> (c b => r) -> r
--withForall (Dict :: Dict (c b)) r = r

withForall :: forall c a r. Forall c -> (c a => r) -> r
withForall (Forall (Dict :: Dict (c a))) r = r

class Trivial a
instance Trivial a

all :: Forall Trivial
all = Forall Dict

class c b a => FlipC c a b
instance c b a => FlipC c a b

class c (f a) => CompC c f a
instance c (f a) => CompC c f a

type ForallC1 c f = ForallC (CompC c f)

