{-# LANGUAGE GADTs #-}

module Test where

data Test a where
  TInt :: Test Int
  TAny :: Test a

instance Functor Test where
  fmap f TInt = TAny
  fmap f TAny = TAny
