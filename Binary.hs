{-# LANGUAGE GADTs, DataKinds, PolyKinds, TypeFamilies, KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE InstanceSigs, DefaultSignatures #-}

module Binary where

import Data.Singletons.TH
import Data.Singletons.Prelude
import Data.Singletons.Prelude.Enum
import Data.Singletons.Prelude.Num

type Bit = Bool
type BiNat = [Bit]

$(singletons [d|
    
  instance Enum BiNat where
    succ [] = [True]
    succ (False:as) = True : as
    succ (True:as) = False : succ as
    
    pred [] = error "pred 0"
    pred (False:as) = True : pred as
    pred (True:as) = False : as
    
    {- no type-level quoteRem or div :(
    toEnum i | i < 0 = error "negative toEnum"
             | i == 0 = []
             | otherwise = let (q, r) = quotRem i 2 in r /= 0 : toEnum q
    -}
    {-
    toEnum i | i < 0 = error "negative toEnum"
             | i == 0 = []
             | otherwise = succ (toEnum (pred i))
    -}
    
    fromEnum [] = 0
    fromEnum (False:as) = 2 * fromEnum as
    fromEnum (True:as) = 1 + 2 * fromEnum as
{-
  xor a b = if a then not b else b

  sumBit a b c = a `xor` b `xor` c
  carryBit a b c = (a && b) || ((a || b) && c)

  add :: Bit -> BiNat -> BiNat -> BiNat
  add c as [] = if c then succ as else as
  add c [] bs = if c then succ bs else bs
  add c (a:as) (b:bs) = sumBit a b c : add (carryBit a b c) as bs
  
  instance Num BiNat where
    a + b = add False a b
    a - b = error "unimplemented"
    a * b = error "unimplemented"
    
    fromInteger i | i < 0 = error "negative toEnum"
                  | i == 0 = []
                  | otherwise = succ (toEnum (pred i))

    abs a = a
    negate a = error "negate BiNat"
  -}
  |])

