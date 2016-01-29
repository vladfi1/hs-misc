{-# LANGUAGE
  TypeFamilies,
  RankNTypes
  #-}

module DataFamilyClass where

data A
data B

data family DataFam :: * -> *

data instance DataFam A = DataFamA
data instance DataFam B = DataFamB

class Class (f :: * -> *) where
  method :: f a -> a

f :: forall a. DataFam a -> ()
f DataFamA = ()
  
