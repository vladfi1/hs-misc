{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module TensorHMatrix where

import Data.Proxy
import Nats
import Numeric.LinearAlgebra
import Data.Vector.Storable
import Data.Default
import Data.Singletons.Prelude

type Usable a = (Element a, Num a, Numeric a, Num (Vector a), Container Vector a)

natVal' :: (Num a) => SNat n -> a
natVal' = fromIntegral . fromSing

data Tensor a (dims :: [Nat]) where
  Scalar :: a -> Tensor a '[]
  Vector :: Vector a -> Tensor a '[n]
  Matrix :: Matrix a -> Tensor a '[n, m]

deriving instance (Show a, Element a) => Show (Tensor a dims)

instance (Default a) => Default (Tensor a '[]) where
  def = Scalar def

instance (SingI n, Default a, Usable a) => Default (Tensor a '[n]) where
  def = Vector $ konst def (natVal' (sing::SNat n))

instance (SingI n, SingI m, Default a, Usable a) => Default (Tensor a '[n, m]) where
  def = Matrix $ konst def (natVal' (sing::SNat n), natVal' (sing::SNat m))

fill1 :: forall a n. (SingI n, Usable a) => a -> Tensor a '[n]
fill1 a = Vector $ konst a (natVal' (sing::SNat n))

instance Num a => Num (Tensor a '[]) where
  Scalar a + Scalar b = Scalar (a + b)
  Scalar a - Scalar b = Scalar (a - b)
  Scalar a * Scalar b = Scalar (a * b)
  negate (Scalar a) = Scalar (negate a)
  abs (Scalar a) = Scalar (abs a)
  signum (Scalar a) = Scalar (signum a)
  fromInteger n = Scalar (fromInteger n)

instance (SingI n, Usable a) => Num (Tensor a '[n]) where
  Vector a + Vector b = Vector (a + b)
  Vector a - Vector b = Vector (a - b)
  Vector a * Vector b = Vector (a * b)
  negate (Vector a) = Vector (negate a)
  abs (Vector a) = Vector (abs a)
  signum (Vector a) = Vector (signum a)
  fromInteger n = Vector $ konst (fromInteger n) (natVal' (sing::SNat n))

instance (SingI n, SingI m, Usable a) => Num (Tensor a '[n, m]) where
  Matrix a + Matrix b = Matrix (a + b)
  Matrix a - Matrix b = Matrix (a - b)
  Matrix a * Matrix b = Matrix (a * b)
  negate (Matrix a) = Matrix (negate a)
  abs (Matrix a) = Matrix (abs a)
  signum (Matrix a) = Matrix (signum a)
  fromInteger n = Matrix $ konst (fromInteger n) (natVal' (sing::SNat n), natVal' (sing::SNat m))

instance Fractional a => Fractional (Tensor a '[]) where
  Scalar a / Scalar b = Scalar (a / b)
  recip (Scalar a) = Scalar (recip a)
  fromRational r = Scalar (fromRational r)

instance (SingI n, Numeric a, Fractional a, Fractional (Vector a)) => Fractional (Tensor a '[n]) where
  Vector a / Vector b = Vector (a / b)
  recip (Vector a) = Vector (recip a)
  fromRational r = Vector $ konst (fromRational r) (natVal' (sing::SNat n))

tmap :: (Container Vector a, Num a, Element b) => (a -> b) -> Tensor a dims -> Tensor b dims
tmap f (Scalar a) = Scalar $ f a
tmap f (Vector v) = Vector $ cmap f v
tmap f (Matrix m) = Matrix $ cmap f m

transpose :: (Numeric a) => Tensor a '[n, m] -> Tensor a '[m, n]
transpose (Matrix m) = Matrix (tr m)

mv :: Numeric a => Tensor a '[n, m] -> Tensor a '[m] -> Tensor a '[n]
mv (Matrix m) (Vector v) = Vector $ m #> v

oneHot :: forall a n. (SingI n, Usable a) => Int -> Tensor a '[n]
oneHot m = Vector $ (konst 0 (natVal' (sing::SNat n))) // [(m, 1)]
