{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Expression where

import Debug.Trace

data ExpK =
  RetT |
  AppT ExpK ExpK |
  LamT ExpK

data Exp t a where
  Ret :: a -> Exp 'RetT a
  App :: Exp t1 (a -> b) -> Exp t2 a -> Exp ('AppT t1 t2) b
  --Lam :: (Exp t1 a -> Exp t2 b) -> Exp 'LamT (a -> b)
  Lam :: (a -> Exp t b) -> Exp ('LamT t) (a -> b)

--let_ :: Exp a -> (Exp a -> Exp b) -> Exp b
let_ :: Exp t2 a -> (a -> Exp t b) -> Exp ('AppT ('LamT t) t2) b
let_ x y = (Lam y) `App` x

if_ :: Exp t2 Bool ->
       Exp t b ->
       Exp t b ->
       Exp ('AppT ('LamT t) t2) b
if_ b x y = let_ b (\b' -> if b' then x else y)

-- call by value

eval :: Exp t a -> a
eval (Ret a) = a
eval (App f a) = (eval f) (eval a)
eval (Lam f) = eval . f


class EDSL exp where
  ret :: a -> exp a
  app :: exp (a -> b) -> exp a -> exp b
  lam :: (exp a -> exp b) -> exp (a -> b)

test1 = if_ (Ret True) (Ret $ traceShowId True) (Ret $ traceShowId False)

