{-# LANGUAGE TypeSynonymInstances, GADTs, KindSignatures, DataKinds, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Singletons where

import Data.Singletons.Prelude
import Data.Singletons.TH

import Data.Type.Equality


instance TestEquality SBool where
  testEquality STrue STrue = Just Refl
  testEquality SFalse SFalse = Just Refl
  testEquality _ _ = Nothing

data Nat = Z | S Nat
  deriving (Show, Eq, Ord)

genSingletons [''Nat]
singDecideInstances [''Nat]

