{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module TensorHMatrix where

import Nats
import Numeric.LinearAlgebra
import Data.Vector.Storable
import Data.Default
import Data.Singletons.Prelude

import Data.Vinyl

import Prelude hiding (zipWith)

type Usable a = (Element a, Num a, Numeric a, Num (Vector a), Container Vector a)

type IntegralK (p :: KProxy k) = (SingKind p, Integral (DemoteRep p))
type IntegralN (n :: k) = IntegralK ('KProxy :: KProxy k)

natVal' :: (Num a, IntegralN n) => Sing n -> a
natVal' = fromIntegral . fromSing

data Tensor a (dims :: [k]) where
  Scalar :: a -> Tensor a '[]
  Vector :: Vector a -> Tensor a '[n]
  Matrix :: Matrix a -> Tensor a '[n, m]

deriving instance (Show a, Element a) => Show (Tensor a dims)

instance (Default a) => Default (Tensor a '[]) where
  def = Scalar def

fill1 :: forall a n. (SingI n, IntegralN n, Usable a) => a -> Tensor a '[n]
fill1 a = Vector $ konst a (natVal' (sing::Sing n))

instance (SingI n, IntegralN n, Default a, Usable a) => Default (Tensor a '[n]) where
  def = fill1 def

instance (SingI n, IntegralN n, SingI m, Default a, Usable a) => Default (Tensor a '[n, m]) where
  def = Matrix $ konst def (natVal' (sing::Sing n), natVal' (sing::Sing m))

instance Num a => Num (Tensor a '[]) where
  Scalar a + Scalar b = Scalar (a + b)
  Scalar a - Scalar b = Scalar (a - b)
  Scalar a * Scalar b = Scalar (a * b)
  negate (Scalar a) = Scalar (negate a)
  abs (Scalar a) = Scalar (abs a)
  signum (Scalar a) = Scalar (signum a)
  fromInteger n = Scalar (fromInteger n)

instance (SingI n, IntegralN n, Usable a) => Num (Tensor a '[n]) where
  Vector a + Vector b = Vector (a + b)
  Vector a - Vector b = Vector (a - b)
  Vector a * Vector b = Vector (a * b)
  negate (Vector a) = Vector (negate a)
  abs (Vector a) = Vector (abs a)
  signum (Vector a) = Vector (signum a)
  fromInteger n = Vector $ konst (fromInteger n) (natVal' (sing::Sing n))

instance (SingI n, IntegralN n, SingI m, Usable a) => Num (Tensor a '[n, m]) where
  Matrix a + Matrix b = Matrix (a + b)
  Matrix a - Matrix b = Matrix (a - b)
  Matrix a * Matrix b = Matrix (a * b)
  negate (Matrix a) = Matrix (negate a)
  abs (Matrix a) = Matrix (abs a)
  signum (Matrix a) = Matrix (signum a)
  fromInteger n = Matrix $ konst (fromInteger n) (natVal' (sing::Sing n), natVal' (sing::Sing m))

instance Fractional a => Fractional (Tensor a '[]) where
  Scalar a / Scalar b = Scalar (a / b)
  recip (Scalar a) = Scalar (recip a)
  fromRational r = Scalar (fromRational r)

instance (SingI n, IntegralN n, Numeric a, Fractional a, Fractional (Vector a)) => Fractional (Tensor a '[n]) where
  Vector a / Vector b = Vector (a / b)
  recip (Vector a) = Vector (recip a)
  fromRational r = Vector $ konst (fromRational r) (natVal' (sing::Sing n))

tmap :: (Container Vector a, Num a, Element b) => (a -> b) -> Tensor a dims -> Tensor b dims
tmap f (Scalar a) = Scalar $ f a
tmap f (Vector v) = Vector $ cmap f v
tmap f (Matrix m) = Matrix $ cmap f m

tZipWith :: (Storable a, Storable b, Storable c) => (a -> b -> c) -> Tensor a '[n] -> Tensor b '[n] -> Tensor c '[n]
tZipWith f (Vector a) (Vector b) = Vector (zipWith f a b)

transpose :: (Numeric a) => Tensor a '[n, m] -> Tensor a '[m, n]
transpose (Matrix m) = Matrix (tr' m)

dot :: (Storable a, Numeric a) => Tensor a '[n] -> Tensor a '[n] -> a
dot (Vector a) (Vector b) = a <.> b

mv :: Numeric a => Tensor a '[n, m] -> Tensor a '[m] -> Tensor a '[n]
mv (Matrix m) (Vector v) = Vector $ m #> v

mm :: Numeric a => Tensor a '[n, m] -> Tensor a '[m, k] -> Tensor a '[n, k]
mm (Matrix m1) (Matrix m2) = Matrix $ m1 <> m2

asCol :: Storable a => Tensor a '[n] -> Tensor a '[n, FromInteger 1]
asCol (Vector v) = Matrix $ asColumn v

asRow' :: Storable a => Tensor a '[n] -> Tensor a '[FromInteger 1, n]
asRow' (Vector v) = Matrix $ asRow v

gradMV :: Numeric a => Tensor a '[n, m] -> Tensor a '[m] -> Tensor a '[n] -> (Tensor a '[n, m], Tensor a '[m])
gradMV m v g = (mm (asCol g) (asRow' v), mv (transpose m) g)

gradMM :: Numeric a => Tensor a '[n, m] -> Tensor a '[m, k] -> Tensor a '[n, k] -> (Tensor a '[n, m], Tensor a '[m, k])
gradMM a b g = (mm g (transpose b), mm (transpose a) g)

oneHot :: forall a n. (SingI n, IntegralN n, Usable a) => Int -> Tensor a '[n]
oneHot m = Vector $ (konst 0 (natVal' (sing::Sing n))) // [(m, 1)]

