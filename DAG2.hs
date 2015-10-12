{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module DAG2 where

import Data.Functor.Identity

class Functor f => Pointed f where
  point :: a -> f a

data DAG f a where
  Var :: f a -> DAG f a
  Const :: a -> DAG f a
  Add :: Num a => DAG f a -> DAG f a -> DAG f a 
  Let :: DAG f a -> (f a -> DAG f b) -> DAG f b

--type Closed a = forall f. DAG f a

eval :: DAG Identity a -> a
eval (Var v) = runIdentity v
eval (Const a) = a
eval (Add a b) = (eval a) + (eval b)
eval (Let a f) = eval . f . Identity $ eval a

infixr 1 $$
($$) = Let

add a b = Add (Var a) (Var b)

dag :: Num a => DAG f a
dag =
  Const 1 $$ \a ->
  Const 2 $$ \b ->
  add a b $$ \c ->
  add b c

instance Monad (DAG Identity) where
  return = Var . Identity
  ma >>= f = Let ma (f . runIdentity)
