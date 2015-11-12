{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, PolyKinds #-}

module DAGIO where

import Data.IORef
import Data.STRef

import Data.Vinyl
import Data.Vinyl.Functor
import VinylUtils

import Control.Monad
import Data.Functor.Sum

import VarArgs

import Prelude hiding (curry, uncurry)

data Node (f :: k -> *) (output :: k)
  = forall (inputs :: [k]). Node
  { forward :: Rec f inputs -> f output
  , inputs :: Rec (Node f) inputs
  , output :: IORef (f output)
  , updated :: IORef Bool
  --, gradInputs :: Rec (IORef :. f) inputs
  --, backwards :: Rec f input -> out -> Rec f input
  }
  | Source { source :: IORef (f output) }

readNode :: Node f output -> IO (f output)
readNode Node{output} = readIORef output
readNode Source{source} = readIORef source

--makeNode :: forall f inputs output c. Curry f inputs (f output) c => c -> Curried (Node f) inputs (IO (Node f output))
makeNode f = curry g where
  g inputs' = do
    let forward' = f--uncurry f
    ins <- rtraverse readNode inputs'
    output' <- newIORef (forward' ins)
    updated' <- newIORef True
    return $ Node
      { forward = forward'
      , inputs = inputs'
      , output = output'
      , updated = updated'
      }

resetNode :: Node f output -> IO ()
resetNode Source{} = return ()
resetNode Node{inputs, updated} = do
  todo <- readIORef updated
  when todo $ do
    rtraverse_ (\n -> Const <$> resetNode n) inputs
    writeIORef updated False

evalNode :: Node f output -> IO (f output)
evalNode Source{source} = readIORef source
evalNode Node{..} = do
  done <- readIORef updated
  unless done $ do
    ins <- rtraverse evalNode inputs
    writeIORef output (forward ins)
    writeIORef updated True
  readIORef output

