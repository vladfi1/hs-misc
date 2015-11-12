{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module DAGIO where

import Data.IORef
import Data.STRef

import Data.Vinyl
import Data.Vinyl.Functor

import Control.Monad
import Data.Functor.Sum

type NodeOrRef f = Sum (Node f) (IORef :. f)

data Node f output
  = forall inputs. Node
  { forward :: Rec f inputs -> f output
  , inputs :: Rec (NodeOrRef f) inputs
  , output :: IORef (f output)
  , updated :: IORef Bool
  --, gradInputs :: Rec (IORef :. f) inputs
  --, backwards :: Rec f input -> out -> Rec f input
  }

rtraverse_ :: Applicative h => (forall x. f x -> h (g x)) -> Rec f rs -> h ()
rtraverse_ f (x :& xs) = f x *> rtraverse_ f xs
rtraverse_ _ RNil = pure ()

resetNodeOrRef :: NodeOrRef f a -> IO ()
resetNodeOrRef (InL node) = resetNode node
resetNodeOrRef _ = return ()

resetNode :: Node f output -> IO ()
resetNode Node{inputs, updated} = do
  todo <- readIORef updated
  when todo $ do
    rtraverse_ (\n -> Const <$> resetNodeOrRef n) inputs
    writeIORef updated False

evalNodeOrRef :: NodeOrRef f output -> IO (f output)
evalNodeOrRef (InL node) = evalNode node
evalNodeOrRef (InR (Compose ref)) = readIORef ref

evalNode :: Node f output -> IO (f output)
evalNode Node{..} = do
  done <- readIORef updated
  unless done $ do
    ins <- rtraverse evalNodeOrRef inputs
    writeIORef output (forward ins)
    writeIORef updated True
  readIORef output

