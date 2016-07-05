{-# LANGUAGE
  GADTs,
  DataKinds,
  KindSignatures
  #-}


module DAGDB where

import Nats

data DAG (n :: Nat) where
  Empty :: DAG Z
  Node :: DAG n -> [LT n] -> DAG (S n)


