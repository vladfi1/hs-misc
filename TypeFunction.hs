{-# LANGUAGE TypeFamilies #-}

module TypeFunction where

type family F x where
  F Int = Int
  F x = Bool

