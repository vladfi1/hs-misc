{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE GADTs #-}

module NatTest where

import Data.Proxy
import Nats

{-
data Nat = Z | S Nat

nPred :: proxy (S n) -> Proxy n
nPred _ = Proxy

nSucc :: proxy n -> Proxy (S n)
nSucc _ = Proxy
-}

class Nat2Integral (n :: Nat) where
  nat2Integral :: Integral i => proxy n -> i

instance Nat2Integral Z where
  nat2Integral _ = 0

instance Nat2Integral n => Nat2Integral (S n) where
  nat2Integral sn = 1 + nat2Integral (nPred sn)

type family IdNat (n :: Nat) :: Nat where
  IdNat Z = Z
  IdNat (S n) = S (IdNat n)

idNat :: proxy n -> Proxy (IdNat n)
idNat _ = Proxy

idNat2Int :: forall proxy (n :: Nat). (Nat2Integral n) => proxy n -> Int
idNat2Int = nat2Integral . idNat
