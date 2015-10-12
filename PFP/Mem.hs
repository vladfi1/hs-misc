{-# LANGUAGE ConstraintKinds #-}

module Mem where

import Control.Applicative
import Data.Traversable

import SplitEval

memInt :: (Int -> a) -> (Int -> a)
memInt f = (map f [0..] !!)

memIntA :: (Applicative m) => (Int -> m a) -> m (Int -> a)
memIntA f = (!!) <$> traverse f [0..]

memIntS ::
  (Applicative n, MonadEval s n, Functor m, MonadSplit s m) =>
  (Int -> n a) -> m (Int -> a)
memIntS = evalS . memIntA

