{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}

module Incremental where

import Control.Monad.Adaptive
import Control.Monad.Adaptive.Ref

import Control.Applicative
import Control.Monad.Random
import Control.Monad.Writer

import PFP

newMod' = newModBy (\_ _ -> False)

foo = run $ do
  m <- newMod $ return 1
  mplus1 <- newMod $ do v <- readMod m
                        return (v+1)
  
  
  let f x = do change m x
               propagate
               inCh $ readMod mplus1
  
  return $ run . f

infer choices = do
  RV mod prior <- inM $ uniform choices
  x <- inCh prior
  change mod x
  propagate


sample model = run $ do
  m <- inCh model
  
  return . run $ do
    (x, choices) <- inCh $ readMod m
    infer choices
    return x

-- could make this more polymorphic
data RV m r where
  RV :: Modifiable m r a -> Changeable m r a -> RV m r

newMod'' = (fmap WriterT) . newMod' . runWriterT
readMod' = WriterT . readMod . runWriterT

make_rv prior = do
  mod <- newMod $ prior
  newMod'' $ do
    x <- lift $ readMod mod
    -- TODO: Is this inefficient?
    -- Appending one element at a time might be.
    -- A sequence or other data structure could be used.
    tell [RV mod prior]
    return x

geometric p = do
  bMod <- make_rv (inM $ bernoulli p)
  
  newMod'' $ do
    b <- readMod' bMod
    if b then return 0
         else do
          g <- lift $ geometric p
          n <- readMod' g
          return $ n+1

tricky_coin = do
  trickyMod <- make_rv (inM $ bernoulli 0.5)
  
  weightMod <- newMod'' $ do
    tricky <- readMod' trickyMod
    if tricky
      then do
        wMod <- lift $ make_rv (inM $ getRandomR (0.0, 1.0))
        readMod' wMod
      else return 0.5

  newMod'' $ do
    w <- readMod' weightMod
    lift $ make_rv (inM $ bernoulli w)


