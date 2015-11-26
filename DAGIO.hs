{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DAGIO where

import Data.IORef

import Data.Vinyl
import Data.Vinyl.Functor
import VinylUtils

import Control.Monad

import VarArgs

import Data.Default
import DefaultM

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

makeSource :: output -> IO (Node output)
makeSource output = Source <$> newIORef (Identity output)

makeUnary :: (a -> output) -> Node a -> IO (Node output)
makeUnary f = makeNode (uncurry1 f)

makeBinary :: (a -> b -> output) -> Node a -> Node b -> IO (Node output)
makeBinary f = makeNode (uncurry2 f)

instance Default output => DefaultM IO (Node output) where
    defM = Source <$> newIORef (Identity def)

makeNode :: Curry Node inputs (IO (Node output)) c => (HList inputs -> Identity output) -> c
makeNode f = curry g where
  forward' = f --uncurry f
  g inputs' = do
    ins <- rtraverse readNode inputs'
    output' <- newIORef (forward' ins)
    updated' <- newIORef True
    return Node
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

evalNode' :: Node output -> IO (Identity output)
evalNode' Source{source} = readIORef source
evalNode' Node{..} = do
  done <- readIORef updated
  unless done $ do
    ins <- rtraverse evalNode' inputs
    writeIORef output (forward ins)
    writeIORef updated True
  readIORef output

evalNode :: Node output -> IO output
evalNode node = getIdentity <$> evalNode' node
