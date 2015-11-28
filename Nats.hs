{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Nats where

--import TypeLevel
import Data.Proxy
import qualified GHC.TypeLits as TL
--import Generics.SOP.Sing
import Data.Singletons.Prelude
import Data.Singletons.TH
import Data.Singletons.Prelude.Enum
import Data.Singletons.TypeLits (withKnownNat)
import Unsafe.Coerce

$(singletons [d|
  data Nat = Z | S Nat
    deriving (Eq, Ord)
  |])

instance Enum Nat where
  succ = S

  pred Z = Z
  pred (S n) = n

  toEnum 0 = Z
  toEnum n = S $ toEnum (n-1)

  fromEnum Z = 0
  fromEnum (S n) = 1 + (fromEnum n)

type family FromTLNat (n :: TL.Nat) :: Nat where
  FromTLNat 0 = Z
  FromTLNat n = S (FromTLNat (n TL.- 1))

instance PEnum ('KProxy :: KProxy Nat) where
  type Succ n = S n
  type Pred Z = Error "Pred Z"
  type Pred (S n) = n
  type ToEnum n = FromTLNat n
  type FromEnum Z = 0
  type FromEnum (S n) = 1 TL.+ (FromEnum n)

instance SEnum ('KProxy :: KProxy Nat) where
  sSucc = SS
  sPred SZ = error "Pred SZ"
  sPred (SS n) = n

  sToEnum = sFromInteger
  -- I don't think we can write sFromEnum

instance Num Nat where
  Z + x = x
  S x + y = S (x + y)

  x - Z = x
  Z - _ = error "Negative Nat."
  S x - S y = x - y

  Z * _ = Z
  S x * y = y + (x * y)

  fromInteger 0 = Z
  fromInteger n = S (fromInteger (n-1))

  negate = error "Can't negate Nats."
  abs = id
  signum _ = 1

instance PNum ('KProxy :: KProxy Nat) where
  type Z :+ x = x
  type S x :+ y = S (x :+ y)

  type x :- Z = x
  type Z :- S x = Error "Negative Nat"
  type S x :- S y = x :- y

  type Z :* x = Z
  type S x :* y = y :+ (x :* y)

  type Abs (n :: Nat) = n
  type Signum n = S Z

  type FromInteger (n :: TL.Nat) = FromTLNat n

instance SNum ('KProxy :: KProxy Nat) where
  SZ %:+ x = x
  SS x %:+ y = SS (x %:+ y)

  x %:- SZ = x
  SZ %:- _ = error "Negative SNat"
  SS x %:- SS y = x %:- y

  SZ %:* _ = SZ
  SS x %:* y = y %:+ (x %:* y)

  sAbs = id
  sSignum _ = SS SZ
  sFromInteger n = withKnownNat n $
    case TL.natVal n of
      0 -> unsafeCoerce SZ
      _ -> unsafeCoerce $ SS (sFromInteger $ sPred n)


type family Sum (xs :: [Nat]) :: Nat where
  Sum '[] = Z
  Sum (x ': xs) = x :+ (Sum xs)

{-
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
-}

instance Show Nat where
  show = show . fromEnum

instance Show (SNat n) where
  show = show . fromSing

s0 = SZ
s1 = SS s0
s2 = SS s1
s3 = SS s2

data LTE n where
  LTE_Z :: LTE n
  LTE_S :: LTE n -> LTE (S n)

data LT n where
  LT_Z :: LT (S n)
  LT_S :: LT n -> LT (S n)
