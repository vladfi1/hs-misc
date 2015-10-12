{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module BoundTest where

import Bound
import Control.Applicative
import Control.Monad (ap)
import Prelude.Extras
import Data.Foldable
import Data.Traversable

infixl 9 :@
data Exp a = V a | Exp a :@ Exp a | Lam (Scope () Exp a)
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

instance Eq1 Exp
instance Ord1 Exp
instance Show1 Exp
instance Read1 Exp
instance Applicative Exp where pure = V; (<*>) = ap

instance Monad Exp where
  return = V
  V a      >>= f = f a
  (x :@ y) >>= f = (x >>= f) :@ (y >>= f)
  Lam e    >>= f = Lam (e >>>= f)

lam :: Eq a => a -> Exp a -> Exp a
lam v b = Lam (abstract1 v b)

whnf :: Exp a -> Exp a
whnf (f :@ a) = case whnf f of
  Lam b -> whnf (instantiate1 a b)
  f'    -> f' :@ a
whnf e = e

idExp = lam "x" $ V "x"
