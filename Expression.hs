{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Expression where

import DeBruijn (DB)

--import HList (RecordLabels)
import GHC.TypeLits
import Data.HList
--import Data.HList.HArray

{-
class ( HFind l (RecordLabels r) n
      , HLookupByHNat n r
      , HLookupByHNatR n r ~ Tagged l v
         ) => HasField2 n l r v | n l r -> v
  where
    hLookupByLabel2 :: label l -> Record r -> v
    hLookupByLabel2 l (Record r) = v
      where
        (Tagged v) = hLookupByHNat (Proxy :: Proxy n) r
-}

data Exp (l :: [*]) (t :: *) where
  Val :: t -> Exp l t
  SVal :: String -> t -> Exp l t
  Var :: (KnownSymbol s, HasField s (Record l) t) => Label s -> Exp l t
  App :: Exp l (t1 -> t2) -> Exp l t1 -> Exp l t2
  Lam :: (KnownSymbol s) => Label s -> Exp (Tagged s b ': l) t -> Exp l (b -> t)

instance Show (Exp l t) where
  show (SVal s _) = s
  show (Val _) = "Val"
  show (Var v) = symbolVal v
  show (App f x) = "(" ++ (show f) ++ " " ++ (show x) ++ ")"
  show (Lam v b) = "\\" ++ (symbolVal v) ++ " -> " ++ (show b)

eval' :: Record l -> Exp l t -> t

eval' _ (SVal _ t) = t
eval' _ (Val t) = t

eval' l (Var v) = l .!. v
--eval' l (Var v) = hLookupByLabel2 v l

eval' l (App f x) = (eval' l f) (eval' l x)

eval' (Record l) (Lam v b) = \x -> eval' (Record $ HCons (v .=. x) l) b

eval :: Exp '[] t -> t
eval = eval' (Record HNil)

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

lx :: Label "x"
lx = Label
vx = Var lx

ly :: Label "y"
ly = Label
vy = Var ly

lz :: Label "z"
lz = Label
vz = Var lz

lf :: Label "f"
lf = Label
vf = Var lf

--lift f = App (Val f)
--lift2 f = app2 (Val f)

plus = Val (+)
times = Val (*)

times2 = Lam lx $ app2 plus vx vx
six = eval $ App times2 (Val 3)

mk_pair = Lam lx $ Lam ly $ Lam lf $ app2 vf vx vy
get_fst = Lam lz $ App vz $ Lam lx $ Lam ly $ vx
get_snd = Lam lz $ App vz $ Lam lx $ Lam ly $ vy

pair12 = app2 mk_pair (sval 1) (sval 2)
one = eval $ App get_fst pair12
two = eval $ App get_snd pair12

-- to DeBruijn indices

--toDB :: Exp l t -> DB l t
--toDB (Val t) = Val t

