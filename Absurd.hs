{-# LANGUAGE EmptyCase #-}

module Absurd where

data Void

absurd :: Void -> a
absurd void = case void of {}
