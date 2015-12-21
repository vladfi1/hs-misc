module Lob where

lob :: Functor f => f (f a -> a) -> f a
lob x = y where y = fmap (\f -> f y) x

instance Num a => Num (x -> a) where
  fromInteger = const . fromInteger
  f + g = \x -> f x + g x
  f * g = \x -> f x * g x
  negate = (negate .)
  abs = (abs .)
  signum = (signum .)
