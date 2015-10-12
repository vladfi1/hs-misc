{-# LANGUAGE GADTs, KindSignatures #-}

module Paradox where

data J c = J (c ())

data R :: * -> * -> * where
 MkR :: (c (J c) -> a) -> R a (J c)

cond_false :: R a (J (R a)) -> a
cond_false x@(MkR f) = f x

absurd :: a
absurd = cond_false (MkR cond_false)

