{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Transformers where

import "mtl" Control.Monad.State
import "mtl" Control.Monad.List
import Data.Functor.Compose

import MonadJoin

--test1 :: [Int]
test1 = (flip runStateT) 0 $ do
  x <- lift [1..10]
  counter <- get
  put (counter + 1)
  return (x + counter)

--test2 :: ListT (State Int) Int
test2 = (flip runState) 0 $ runListT $ do
  x <- ListT . return $ [1..10]
  counter <- get
  put (counter + 1)
  return (x + counter)
--  return x

-- standard
instance (Functor m, Monad m) => Monad' (StateT s m) where
  return' = return
  join' = (>>= id)

type StateList s = Compose (State s) []

instance (Monad m, Monad (Compose (State s) m)) => MonadState s (Compose (State s) m) where
  get = Compose $ fmap return get
  put = Compose . (fmap return) . put

lift' = Compose . return

--test3 :: StateList Int Int
--test3 :: ([Int], Int)
test3 = (flip runState) 0 $ getCompose $ do
  x <- lift' [0..10]
  counter <- get
  put (counter + 1)
  return (x + counter)
