{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WrappedNum where

import Convertible

newtype WrappedNum a = WrapNum { unwrapNum :: a }

instance Convertible a (WrappedNum a) where
  convert = WrapNum

instance Convertible (WrappedNum a) a where
  convert = unwrapNum

instance (Num a) => Num (WrappedNum a) where
  (+) = convert ((+) :: a -> a -> a)
  (-) = convert ((-) :: a -> a -> a)
  (*) = convert ((*) :: a -> a -> a)
  
  abs = convert (abs :: a -> a)
  signum = convert (signum :: a -> a)
  fromInteger = convert (fromInteger :: Integer -> a)
  --fromInteger = convert . (fromInteger :: Integer -> a)

