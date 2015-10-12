{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# LANGUAGE OverlappingInstances #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE CPP #-}

module Expression where

import Prelude hiding (lookup)

import GHC.TypeLits hiding (Nat)
import Data.Vinyl
import Data.Proxy

--type Proxy s = forall proxy. proxy s

data Exp (l :: [(Symbol, *)]) (t :: *) where
  Val :: t -> Exp l t
  SVal :: String -> t -> Exp l t
  Var :: (KnownSymbol s, '(s, t) ∈ l) => proxy s -> Exp l t
  App :: Exp l (t1 -> t2) -> Exp l t1 -> Exp l t2
  Lam :: (KnownSymbol s) => proxy s -> Exp ('(s, b) ': l) t -> Exp l (b -> t)

--data Proxy (t :: k) = Proxy

tag :: proxy s -> Proxy '(s, t)
tag _ = Proxy

eval' :: FieldRec l -> Exp l t -> t

eval' _ (Val t) = t
eval' _ (SVal _ t) = t

eval' l (Var v) = getField $ rget (tag v) l

eval' l (App f x) = (eval' l f) (eval' l x)
eval' l (Lam _ b) = \x -> eval' (Field x :& l) b

eval :: Exp '[] t -> t
eval = eval' RNil

--lift f = App (Val f)
sval :: (Show t) => t -> Exp l t
sval t = SVal (show t) t

app2 f x y = App (App f x) y
--lift2 f = app2 (Val f)

-- variables
lx :: Proxy "x"
lx = Proxy
vx = Var lx

ly :: Proxy "y"
ly = Proxy
vy = Var ly

lz :: Proxy "z"
lz = Proxy
vz = Var lz

lf :: Proxy "f"
lf = Proxy
vf = Var lf

#define P(s) (Proxy::Proxy s)
#define V(s) (Var P(s))
#define LAM(s) Lam P(s)

--lift f = App (Val f)
--lift2 f = app2 (Val f)

plus = SVal "+" (+)
times = SVal "*" (*)

times2 = LAM("x") $ app2 plus V("x") V("x")

--six = eval $ App times2 (Val 3)

{-
mk_pair = Lam lx $ Lam ly $ Lam lf $ app2 vf vx vy
get_fst = Lam lz $ App vz $ Lam lx $ Lam ly $ vx
get_snd = Lam lz $ App vz $ Lam lx $ Lam ly $ vy

pair12 = app2 mk_pair (sval 1) (sval 2)
one = eval $ App get_fst pair12
two = eval $ App get_snd pair12
-}
