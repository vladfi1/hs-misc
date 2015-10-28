{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Nats where

import TypeLevel
import Data.Proxy
import qualified GHC.TypeLits as TL
import Generics.SOP.Sing

data Nat = Z | S Nat

instance Enum Nat where
  succ = S
  
  pred Z = Z
  pred (S n) = n
  
  toEnum 0 = Z
  toEnum n = S $ toEnum (n-1)
  
  fromEnum Z = 0
  fromEnum (S n) = 1 + (fromEnum n)

type family Min (x :: Nat) (y :: Nat) :: Nat where
  Min Z y = Z
  Min x Z = Z
  Min (S x) (S y) = S (Min x y)

type family Max (x :: Nat) (y :: Nat) :: Nat where
  Max Z y = y
  Max x Z = x
  Max (S x) (S y) = S (Max x y)

infix 4 :<:
type family (x :: Nat) :<: (y :: Nat) :: Bool where
  x :<: Z = False
  Z :<: (S x) = True
  (S x) :<: (S y) = x :<: y

infix 4 :<=:
type family (x :: Nat) :<=: (y :: Nat) :: Bool where
  Z :<=: x = True
  (S x) :<=: Z = False
  (S x) :<=: (S y) = x :<=: y

infixl 6 :+:
type family (x :: Nat) :+: (y :: Nat) :: Nat where
  Z :+: x = x
  (S x) :+: y = S (x :+: y)

type family Sum (xs :: [Nat]) :: Nat where
  Sum '[] = Z
  Sum (x ': xs) = x :+: (Sum xs)

infixl 6 :-:
type family (x :: Nat) :-: (y :: Nat) :: Nat where
  Z :-: x = Z
  x :-: Z = x
  (S x) :-: (S y) = x :-: y

-- this requires undecidable instances...
infixl 7 :*:
type family (x :: Nat) :*: (y :: Nat) :: Nat where
  Z :*: x = Z
  (S x) :*: y = y :+: (x :*: y)

infixl 7 :/:
type family (x :: Nat) :/: (y :: Nat) :: Nat where
  Z :/: y = Z -- avoid infinite loops
  x :/: y =
    If (x :<: y) Z
      (S ((x :-: y) :/: y))

infixl 7 :%:
type family (x :: Nat) :%: (y :: Nat) :: Nat where
  Z :%: y = Z -- avoid infinite loops
  x :%: y =
    If (x :<: y) x
      ((x :-: y) :%: y)

type family GCD (m :: Nat) (n :: Nat) :: Nat where
  GCD Z m = m
  GCD m n =
    If (m :<: n)
      (GCD (n :-: m) m)
      (GCD (m :-: n) n)

gcd' :: p1 m -> p2 n -> Proxy (GCD m n)
gcd' _ _ = Proxy

-- use singletons?

type family ToNat (n :: TL.Nat) :: Nat where
  ToNat 0 = Z
  ToNat n = S (ToNat (n TL.- 1))

type SNat = (Sing :: Nat -> *)

data instance Sing (n :: Nat) where
  SZ :: SNat Z
  SS :: SNat n -> Sing (S n)

class ReifyNat (n :: Nat) where
  nat :: SNat n

instance ReifyNat Z where
  nat = SZ

instance (ReifyNat n) => ReifyNat (S n) where
  nat = SS nat

instance ReifyNat n => SingI n where
  sing = nat

snat2nat :: SNat n -> Nat
snat2nat SZ = Z
snat2nat (SS n) = S (snat2nat n)

instance Show (SNat n) where
  show = show . fromEnum . snat2nat

snat :: (n' ~ ToNat n, ReifyNat n') => proxy n -> SNat n'
snat _ = nat

zero = SZ
one = SS zero
two = SS one
three = SS two

nPred :: proxy (S n) -> Proxy n
nPred _ = Proxy

nSucc :: proxy n -> Proxy (S n)
nSucc _ = Proxy

data LTE n where
  ZLTE :: LTE n
  SLTE :: LTE n -> LTE (S n)

data LT n where
  ZLT :: LT (S n)
  SLT :: LT n -> LT (S n)

