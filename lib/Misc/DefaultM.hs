{-# LANGUAGE MultiParamTypeClasses #-}

module Misc.DefaultM where

class DefaultM m a where
  defM :: m a
