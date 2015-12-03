{-# LANGUAGE GADTs #-}

module RecordGADT where

data Test where
  Test :: Num t => {x :: t} -> Test
