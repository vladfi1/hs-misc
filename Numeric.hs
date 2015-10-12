{-# LANGUAGE NoImplicitPrelude #-}

module Numeric where

import NumericPrelude
import qualified Algebra.Ring as Ring

instance (Ring.C a) => Ring.C (b -> a) where
  (f1 * f2) b = (f1 b) * (f2 b)
  one = const one
  fromInteger = const . fromInteger
  (f ^ n) b = (f b) ^ n

