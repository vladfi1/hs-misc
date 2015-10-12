{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE KindSignatures #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rec where

import Data.Function (fix)
import Control.Monad.Fix
import Control.Monad.State

import Mem

-- This sends ghc into an infinite loop.
-- See https://ghc.haskell.org/trac/ghc/ticket/8168
-- The {-# NOINLINE g #-} pragma fixes this.

newtype Rec a = Rec { runRec :: Rec a -> a }

y :: (a -> a) -> a
y f = g (Rec g)
  where g h = f (runRec h h)
        {-# NOINLINE g #-}

{- Some implementations of the fixed point operator via value recursion.
fix :: (a -> a) -> a
fix' f = fix $ Rec $ f . runRec
fix' f = f (fix' f)
fix f = let x = f x in x
--}

fact' :: (Int -> Integer) -> (Int -> Integer)
fact' _ 0 = 1
fact' f n = (fromIntegral n) * f (n-1)

fact = fix fact'

fibRec' :: (Int -> Integer) -> (Int -> Integer)
fibRec' _ 0 = 0
fibRec' _ 1 = 1
fibRec' f n = f (n - 2) + f (n - 1)

fib = fix $ memInt . fibRec'

-- doesn't work! only certain monads have mfix
--mfix :: (Monad m) => (a -> m a) -> m a
--mfix f = let x = x >>= f in x
--mfix f = fix (>>= f)

