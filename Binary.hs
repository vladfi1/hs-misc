{-# LANGUAGE GADTs, DataKinds, PolyKinds, TypeFamilies, KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Binary where

import Data.Singletons.Prelude
import Data.Singletons.TH

type BiNat = [Bool]

$(singletons [d|
  xor a b = if a then not b else b

  sumBit a b c = a `xor` b `xor` c
  carryBit a b c = (a && b) || ((a || b) && c)

  add :: Bool -> BiNat -> BiNat -> BiNat
  add c as [] = as
  add c [] bs = bs
  add c (a:as) (b:bs) = s : add r as bs
    where s = sumBit a b c
          r = carryBit a b c
  
  instance Num BiNat where
    a + b = add False a b
    a - b = error "unimplemented"
    a * b = error "unimplemented"
    
    fromInteger n = if n == 0 then [] else 1 + (n-1)
    abs a = a
    negate a = error "negate BiNat"
  
  |])

