{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE FunctionalDependencies #-}
--{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module DeBruijn where

--import Prelude hiding (last, lookup, (-))

import Data.HList
import Data.HList.HArray
import Data.Proxy
import Nats

--import Nats
--import qualified GHC.TypeLits as TL

-- unifies two lists, one of which is a prefix of the other
-- App :: DB l1 (t1 -> t2) -> DB l2 t1 -> DB (Unify l1 l2) t2
-- might actually work now with closed type families
type family Unify (l1 :: [*]) (l2 :: [*]) :: [*] where
  Unify '[] '[] = '[]
  Unify '[] (t ': l) = t ': l
  Unify (t ': l) '[] = t ': l
  Unify (t ': l1) (t ': l2) = t ': (Unify l1 l2)

-- this might work, but the current approach is nicer
-- App :: (Prefix l1 l, Prefix l2 l) => DB l1 (t1 -> t2) -> DB l2 t1 -> DB l t2
class Prefix (p :: [*]) (l :: [*]) where
  prefix :: HList l -> HList p

instance Prefix '[] l where
  prefix _ = HNil

instance (Prefix p l) => Prefix (t ': p) (t ': l) where
  prefix (HCons t l) = HCons t (prefix l)

-- Typed lambda calculus where variables are encoded by De Bruijn indices.
-- The "l" parameter is the environement type, a simple list.
-- The "t" parameter is the (Haskell) type that the expression evaluates to.
-- Ed Kmett's Bound package might provide some interesting ideas.
data DB (l :: [*]) (t :: *) where
  Val :: t -> DB l t
  Var :: Var l t -> DB l t
  VarN :: (HLookupByHNat n l) => Proxy n -> DB l (HLookupByHNatR n l)
  App :: DB l (t1 -> t2) -> DB l t1 -> DB l t2
  Lam :: DB (b ': l) t -> DB l (b -> t)

-- Taken from Stephanie Weirich's implementation:
-- http://www.cs.ox.ac.uk/projects/gip/school/tc.hs
data Var (l :: [*]) (t :: *) where
  ZVar :: Var (t ': l) t
  SVar :: Var l t -> Var (s ': l) t

lookupVar :: HList l -> Var l t -> t
lookupVar (HCons t _) ZVar = t
lookupVar (HCons _ l) (SVar v) = lookupVar l v
--lookup HNil ZVar = error "Unreachable pattern."
--lookup HNil (SVar _) = error "Unreachable pattern."

-- Either GHC can't tell this is impossible,
-- or it must be possible because of bottom.
lookupVar HNil _ = error "Unreachable pattern."

eval' :: HList l -> DB l t -> t

eval' _ (Val t) = t
eval' l (Var v) = lookupVar l v
eval' l (VarN v) = hLookupByHNat v l

eval' l (App f x) = (eval' l f) (eval' l x)
eval' l (Lam b) = \x -> eval' (HCons x l) b

eval = eval' HNil

-- some utilities for actually writing terms

-- some de Bruijn indices
var0 = ZVar
var1 = SVar var0
var2 = SVar var1
var3 = SVar var2

v0 = Var var0
v1 = Var var1
v2 = Var var2
v3 = Var var3

#define VAR(n) (VarN (Proxy::Proxy (ToHNat n)))

--lift f = App (Val f)

app2 f x y = App (App f x) y
--lift2 f = app2 (Val f)

plus = Val (+)
times = Val (*)

times2 = Lam $ app2 plus v0 VAR(0)

six = eval $ App times2 (Val 3)

mk_pair = Lam $ Lam $ Lam $ app2 v0 v2 v1
first = Lam $ Lam $ v1
second = Lam $ Lam $ v0

get_fst = Lam $ App v0 first

pair12 = app2 mk_pair (Val 1) (Val 2)

