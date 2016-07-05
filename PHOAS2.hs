{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module PHOAS2 where

import Control.Applicative
import Control.Monad (ap, join)

data ExpF a b
  = App b b
  | Lam (a -> b)

instance Functor (ExpF a) where
  fmap f (App x y) = App (f x) (f y)
  fmap f (Lam g) = Lam (f . g)

data Rec f a
  = Place a
  | Roll (f (Rec f a))

instance Functor f => Functor (Rec f) where
  fmap a2b = f where
    f (Place a)  = Place (a2b a)
    f (Roll fs) = Roll (fmap f fs)

instance Functor f => Applicative (Rec f) where
  pure = return
  (<*>) = ap

instance Functor f => Monad (Rec f) where
  return = Place
  (>>=) = flip g where
    g f = h where
      h (Place a) = f a
      h (Roll fs) = Roll (fmap h fs)
  
{-- 
  Place a >>= f = f a
  Roll fs >>= f = Roll $ fmap (>>= f) fs
--}

cata :: Functor f => (f a -> a) -> Rec f a -> a
cata f = g where
  g (Place a)  = a
  g (Roll fs) = f (fmap g fs)

type Exp a = Rec (ExpF a)

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
