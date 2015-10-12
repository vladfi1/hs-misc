{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Expression where

import qualified DeBruijn as DB

import GHC.TypeLits
import HList

data Exp (l :: [*]) (t :: *) where
  Val :: t -> Exp l t
  SVal :: String -> t -> Exp l t
  Var :: (KnownSymbol s, HField s l t) => Proxy s -> Exp l t
  App :: Exp l (t1 -> t2) -> Exp l t1 -> Exp l t2
  Lam :: (KnownSymbol s) => Proxy s -> Exp (Tagged s b ': l) t -> Exp l (b -> t)

instance Show (Exp l t) where
  show (SVal s _) = s
  show (Val _) = "Val"
  show (Var v) = symbolVal v
  show (App f x) = "(" ++ (show f) ++ " " ++ (show x) ++ ")"
  show (Lam v b) = "\\" ++ (symbolVal v) ++ " -> " ++ (show b)


-- call by value evaluator
eval' :: HList l -> Exp l t -> t

eval' _ (SVal _ t) = t
eval' _ (Val t) = t

eval' l (Var v) = hField v l

eval' l (App f x) = (eval' l f) (eval' l x)

eval' l (Lam v b) = \x -> eval' (HCons (tagWith v x) l) b

eval :: Exp '[] t -> t
eval = eval' HNil

{-
whnf' :: HList l -> Exp l t -> Exp '[] t
whnf' l (App f x) =
  case whnf' l f of
    Lam v b -> whnf' (HCons (tagWith v x) l) b
    f'      -> App f' x

whnf' (Val v) = Val v
whnf' (SVal s v) = SVal s v
-}

-- helpers
sval :: (Show t) => t -> Exp l t
sval t = SVal (show t) t

app2 f x y = App (App f x) y

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

six = eval $ App times2 (Val 3)

mk_pair = Lam lx $ Lam ly $ Lam lf $ app2 vf vx vy
get_fst = Lam lz $ App vz $ Lam lx $ Lam ly $ vx
get_snd = Lam lz $ App vz $ Lam lx $ Lam ly $ vy

pair12 = app2 mk_pair (sval 1) (sval 2)
one = eval $ App get_fst pair12
two = eval $ App get_snd pair12

-- to de Bruijn indices

toDB :: forall l t. Exp l t -> DB.DB (RecordValuesR l) t

toDB (Val t) = DB.Val t
toDB (SVal _ t) = DB.Val t

toDB (App f x) = DB.App (toDB f) (toDB x)
toDB (Lam _ b) = DB.Lam (toDB b)

toDB (Var s) = DB.VarN $ hRecordIndex s (Proxy::Proxy l)

