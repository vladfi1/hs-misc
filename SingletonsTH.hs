{-# LANGUAGE DataKinds,
             PolyKinds,
             TypeOperators,
             TypeFamilies,
             GADTs,
             UndecidableInstances,
             KindSignatures,
             InstanceSigs,
             ScopedTypeVariables,
             TemplateHaskell
             #-}

module SingletonsTH where

import Data.Singletons.Prelude
import Data.Singletons.TH
import Data.Singletons.Prelude.Enum
import Data.Singletons.Prelude.Num

$(singletons [d|
  data Nat = Z | S Nat
    deriving (Eq, Ord, Show)

  instance Enum Nat where
    succ n = S n

    pred Z = error "pred Z"
    pred (S n) = n

    toEnum n = if n == 0 then Z else S $ toEnum (n-1)

    fromEnum Z = 0
    fromEnum (S n) = 1 + (fromEnum n)

  instance Num Nat where
    Z + x = x
    S x + y = S (x + y)

    x - Z = x
    Z - S _ = error "Z - S _"
    S x - S y = x - y

    Z * _ = Z
    S x * y = y + (x * y)

    fromInteger n = if n == 0 then Z else S (fromInteger (n-1))

    negate Z = Z
    negate (S _) = error "negate (S _)"
    
    abs n = n
    signum _ = 1

  |])

