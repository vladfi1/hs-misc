{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module PHOAS where

import Data.Profunctor

import Control.Applicative
import Control.Monad (ap, join)

data ExpF a b
  = App b b
  | Lam (a -> b)

instance Profunctor ExpF where
  dimap a2b c2d (App c1 c2) = App (c2d c1) (c2d c2)
  dimap a2b c2d (Lam b2c) = Lam (c2d . b2c . a2b)

instance Functor (ExpF a) where
  fmap f (App x y) = App (f x) (f y)
  fmap f (Lam g) = Lam (f . g)

data Rec p a b
  = Place b
  | Roll (p a (Rec p a b))

instance Profunctor p => Profunctor (Rec p) where
  dimap a2b c2d = f where
    f (Place c)  = Place (c2d c)
    f (Roll pbr) = Roll (dimap a2b f pbr)

instance Functor (p a) => Functor (Rec p a) where
  fmap a2b = f where
    f (Place b)  = Place (a2b b)
    f (Roll par) = Roll (fmap f par)

instance Functor (p a) => Applicative (Rec p a) where
  pure = return
  (<*>) = ap

instance Functor (p a) => Monad (Rec p a) where
  return = Place
  Place b >>= f = f b
  Roll bs >>= f = Roll $ fmap (>>= f) bs

cata :: Functor (p a) => (p a b -> b) -> Rec p a b -> b
cata f = g where
  g (Place b)  = b
  g (Roll par) = f (fmap g par)

type Exp = Rec ExpF

vars = [[i] | i <- ['a'..'z']] ++ [i:show j | j <- [1..], i <- ['a'..'z']]

showExpF (App x y) vs = "(" ++ x vs ++ " " ++ y vs ++ ")"
showExpF (Lam f) (v:vs) = "(\\" ++ v ++ " -> " ++ f (const v) vs ++ ")"

showExp exp = cata showExpF exp vars

{- Not very useful, because types can't be inferred :(
instance Show (Exp (b -> [Char]) ([[Char]] -> [Char])) where
  show = showExp
-}

lam :: (a -> Exp a b) -> Exp a b
lam f = Roll (Lam f)

app :: Exp a b -> Exp a b -> Exp a b
app x y = Roll (App x y)

var :: b -> Exp a b
var = return

let_ x f = app (lam f) x

idExp = lam $ \x -> var x

loop = app f f
  where f = lam $ \g -> app (var g) (var g)


main = putStrLn . showExp $ idExp >> idExp
