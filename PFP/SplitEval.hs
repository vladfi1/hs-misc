{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             TypeSynonymInstances,
             FlexibleInstances,
             ConstraintKinds
             #-}

module SplitEval (
    MonadSplit,
    MonadEval,
    evalS,
    sequenceS,
    traverseS,
    repeatS
    ) where

import Control.Applicative
import Data.Traversable

import Control.Monad.State
import Control.Monad.Random -- for MonadSplit

class (Monad m) => MonadEval s m | m -> s where
  evalM :: m a -> s -> a
  --runM :: m a -> s -> (a, s)

instance MonadEval s (State s) where
  evalM = evalState

instance (RandomGen g) => MonadEval g (Rand g) where
  evalM = evalRand

--type SplitEval s m = (MonadSplit s m, MonadEval s m)
--class (MonadSplit s m, MonadEval s m) => SplitEval s m

-- Splits the state and uses it to eval a (possibly infinite) stateful computation.
evalS ::
  (Functor m, MonadSplit s m, MonadEval s n) =>
  n a -> m a
evalS x = evalM x <$> getSplit

traverseS ::
  (Traversable t, Applicative n, MonadEval s n, Functor m, MonadSplit s m) =>
  (a -> n b) -> t a -> m (t b)
traverseS f = evalS . (traverse f)

sequenceS ::
  (Traversable t, Applicative n, MonadEval s n, Functor m, MonadSplit s m) =>
  t (n a) -> m (t a)
sequenceS = evalS . sequenceA

repeatS ::
  (Applicative n, MonadEval s n, Functor m, MonadSplit s m) =>
  n a -> m [a]
repeatS = sequenceS . repeat

