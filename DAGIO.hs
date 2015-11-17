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

data Node (output :: *)
  = forall (inputs :: [*]). Node
  { forward :: HList inputs -> Identity output
  , inputs :: Rec Node inputs
  , output :: IORef (Identity output)
  , updated :: IORef Bool
  --, gradInputs :: Rec (IORef :. f) inputs
  --, backwards :: Rec f input -> out -> Rec f input
  }
  | Source { source :: IORef (Identity output) }

readNode :: Node output -> IO (Identity output)
readNode Node{output} = readIORef output
readNode Source{source} = readIORef source

makeSource output = Source <$> newIORef (Identity output)

makeBinary f a b = makeNode (uncurry2 f) a b

--makeNode :: forall f inputs output c. Curry f inputs (f output) c => c -> Curried (Node f) inputs (IO (Node f output))
makeNode f = curry g where
  forward' = f --uncurry f
  g inputs' = do
    ins <- rtraverse readNode inputs'
    output' <- newIORef (forward' ins)
    updated' <- newIORef True
    return $ Node
      { forward = forward'
      , inputs = inputs'
      , output = output'
      , updated = updated'
      }

resetNode :: Node output -> IO ()
resetNode Source{} = return ()
resetNode Node{inputs, updated} = do
  todo <- readIORef updated
  when todo $ do
    rtraverse_ (\n -> Const <$> resetNode n) inputs
    writeIORef updated False

evalNode :: Node output -> IO (Identity output)
evalNode Source{source} = readIORef source
evalNode Node{..} = do
  done <- readIORef updated
  unless done $ do
    ins <- rtraverse evalNode inputs
    writeIORef output (forward ins)
    writeIORef updated True
  readIORef output

