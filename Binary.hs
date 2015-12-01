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

  add :: [Bool] -> [Bool] -> [Bool]
  add as [] = as
  add [] bs = bs
  add (a:as) (b:bs) = r : s : cs
    where c:cs = add as bs
          s = sumBit a b c
          r = carryBit a b c
  
  instance Num BiNat where
    a + b = add a b
    a - b = error "unimplemented"
    a * b = error "unimplemented"
    
    fromInteger n = if n == 0 then [] else 1 + n
    abs a = a
    negate a = error "negate BiNat"
  
  |])

