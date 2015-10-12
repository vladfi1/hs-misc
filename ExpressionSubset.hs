{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}

module Expression where

import DeBruijn (DB)

import GHC.TypeLits
--import Data.HList
--import Subset
import HList
import Data.Proxy
import Data.Tagged

data Exp (l :: [*]) (t :: *) where
  Val :: t -> Exp l t
  SVal :: String -> t -> Exp l t
  Var :: (KnownSymbol s, HasField s l t) => Proxy s -> Exp l t
  App :: Exp l (t1 -> t2) -> Exp l t1 -> Exp l t2
  Lam :: (KnownSymbol s) => Proxy s -> Exp (Tagged s b ': l) t -> Exp l (b -> t)

instance Show (Exp l t) where
  show (SVal s _) = s
  show (Val _) = "Val"
  show (Var v) = symbolVal v
  show (App f x) = "(" ++ (show f) ++ " " ++ (show x) ++ ")"
  show (Lam v b) = "\\" ++ (symbolVal v) ++ " -> " ++ (show b)

eval' :: HList l -> Exp l t -> t

eval' _ (SVal _ t) = t
eval' _ (Val t) = t

eval' l (Var v) = hLookupByLabel v l

eval' l (App f x) = (eval' l f) (eval' l x)

eval' l (Lam v b) = \x -> eval' (HCons (tagWith v x) l) b

eval :: Exp '[] t -> t
eval = eval' HNil

{- Ill-typed :(
whnf (App f x) =
  case whnf f of
    Lam v b -> whnf _
    f'      -> App f' x
whnf e = e
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

--lift f = App (Val f)
--lift2 f = app2 (Val f)

plus = SVal "+" (+)
times = SVal "*" (*)

times2 = Lam lx $ app2 plus vx vx
{-
six = eval $ App times2 (Val 3)

mk_pair = Lam lx $ Lam ly $ Lam lf $ app2 vf vx vy
get_fst = Lam lz $ App vz $ Lam lx $ Lam ly $ vx
get_snd = Lam lz $ App vz $ Lam lx $ Lam ly $ vy

pair12 = app2 mk_pair (sval 1) (sval 2)
one = eval $ App get_fst pair12
two = eval $ App get_snd pair12
-}
-- to DeBruijn indices

--toDB :: Exp l t -> DB l t
--toDB (Val t) = Val t

