{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module TensorHMatrix where

import Data.Proxy
import GHC.TypeLits hiding (natVal')
import Numeric.LinearAlgebra
import Data.Default

natVal' :: (KnownNat n, Num a) => proxy n -> a
natVal' = fromInteger . natVal

data Tensor a (dims :: [Nat]) where
  Scalar :: a -> Tensor a '[]
  Vector :: Vector a -> Tensor a '[n]
  Matrix :: Matrix a -> Tensor a '[n, m]

deriving instance (Show a, Element a) => Show (Tensor a dims)

instance (Default a) => Default (Tensor a '[]) where
  def = Scalar def

instance (KnownNat n, Default a, Container Vector a) => Default (Tensor a '[n]) where
  def = Vector $ konst def (natVal' (Proxy::Proxy n))

instance (KnownNat n, KnownNat m, Default a, Container Vector a, Num a) => Default (Tensor a '[n, m]) where
  def = Matrix $ konst def (natVal' (Proxy::Proxy n), natVal' (Proxy::Proxy m))


instance Num a => Num (Tensor a '[]) where
  Scalar a + Scalar b = Scalar (a + b)
  Scalar a - Scalar b = Scalar (a - b)
  Scalar a * Scalar b = Scalar (a * b)
  negate (Scalar a) = Scalar (negate a)
  abs (Scalar a) = Scalar (abs a)
  signum (Scalar a) = Scalar (signum a)
  fromInteger n = Scalar (fromInteger n)

instance (KnownNat n, Num a, Num (Vector a), Container Vector a) => Num (Tensor a '[n]) where
  Vector a + Vector b = Vector (a + b)
  Vector a - Vector b = Vector (a - b)
  Vector a * Vector b = Vector (a * b)
  negate (Vector a) = Vector (negate a)
  abs (Vector a) = Vector (abs a)
  signum (Vector a) = Vector (signum a)
  fromInteger n = Vector $ konst (fromInteger n) (natVal' (Proxy::Proxy n))

instance (KnownNat n, KnownNat m, Num a, Num (Vector a), Container Vector a) => Num (Tensor a '[n, m]) where
  Matrix a + Matrix b = Matrix (a + b)
  Matrix a - Matrix b = Matrix (a - b)
  Matrix a * Matrix b = Matrix (a * b)
  negate (Matrix a) = Matrix (negate a)
  abs (Matrix a) = Matrix (abs a)
  signum (Matrix a) = Matrix (signum a)
  fromInteger n = Matrix $ konst (fromInteger n) (natVal' (Proxy::Proxy n), natVal' (Proxy::Proxy m))

tmap :: (Container Vector a, Num a, Element b) => (a -> b) -> Tensor a dims -> Tensor b dims
tmap f (Scalar a) = Scalar $ f a
tmap f (Vector v) = Vector $ cmap f v
tmap f (Matrix m) = Matrix $ cmap f m

transpose :: (Numeric a) => Tensor a '[n, m] -> Tensor a '[m, n]
transpose (Matrix m) = Matrix (tr m)

mv :: Numeric a => Tensor a '[n, m] -> Tensor a '[m] -> Tensor a '[n]
mv (Matrix m) (Vector v) = Vector $ m #> v

