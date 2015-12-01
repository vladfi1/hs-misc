{-# LANGUAGE RankNTypes #-}

module Polymorphism where

import Debug.Trace

newtype Negater = Negater { runNegater :: forall a. Num a => a -> a}

make :: Negater
make = trace "trace" (Negater negate)
