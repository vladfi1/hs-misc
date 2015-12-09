{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module DAGIO where

import Data.IORef

import Data.Vinyl
import Data.Vinyl.Functor
import VinylUtils

import Control.Monad

import VarArgs

import Data.Default
import DefaultM

import Numeric.AD

--import qualified Algebra.Additive as Additive

import Prelude hiding (curry, uncurry)

data Some f where
  Some :: f a -> Some f

data Node (output :: *) where
  Node :: Num output =>
    { forward :: HList inputs -> Identity output
    , inputs :: Rec Node inputs
    , output :: IORef (Identity output)
    , updated :: IORef Bool
    , backwards :: HList inputs -> Identity output -> HList inputs
    , gradOutput :: IORef (Identity output)
    } -> Node output

--makeSource :: Num output => output -> IO (Node output)
--makeSource output = Source <$> newIORef (Identity output) <*> newIORef 0

deriving instance Num a => Num (Identity a)

makeUnary :: Num b => (forall a. Num a => a -> a) -> Node b -> IO (Node b)
makeUnary f = makeNode (uncurry1 f) (\inputs output -> (output * uncurry1 (diff f) inputs) :& RNil)

makeBinary :: Num b => (forall a. Num a => a -> a -> a) -> Node b -> Node b -> IO (Node b)
makeBinary f = makeNode (uncurry2 f) g where
  g (x :& y :& RNil) output = dx :& dy :& RNil
    where [dx, dy] = map (output *) $ grad (\[a, b] -> f a b) [x, y]

--instance Default output => DefaultM IO (Node output) where
--    defM = Source <$> newIORef (Identity def)

makeNode :: Num output => Curry Node inputs (IO (Node output)) c => (HList inputs -> Identity output) -> (HList inputs -> Identity output -> HList inputs) -> c
makeNode f b = curry g where
  g inputs' = do
    ins <- rtraverse readNode inputs'
    output' <- newIORef (f ins)
    updated' <- newIORef True
    gradOutput' <- newIORef 0
    return Node
      { forward = f
      , inputs = inputs'
      , output = output'
      , updated = updated'
      , backwards = b
      , gradOutput = gradOutput'
      }

readNode :: Node output -> IO (Identity output)
readNode Node{output} = readIORef output
--readNode Source{source} = readIORef source

resetNode :: Node output -> IO ()
--resetNode Source{} = return ()
resetNode Node{..} = do
  todo <- readIORef updated
  when todo $ do
    rtraverse_ (\n -> Const <$> resetNode n) inputs
    writeIORef gradOutput 0
    writeIORef updated False

type Tape = [Some Node]

evalNode :: IORef Tape -> Node output -> IO (Identity output)
--evalNode' tape Source{..} = readIORef source
evalNode tape node@Node{..} = do
  done <- readIORef updated
  unless done $ do
    modifyIORef tape (Some node :)
    ins <- rtraverse (evalNode tape) inputs
    writeIORef output (forward ins)
    writeIORef updated True
  readIORef output

backprop tape = traverse f tape where
  writeGrad Node{gradOutput} grad = modifyIORef gradOutput (grad +)
  
  f Node{..} = do
    ins <- rtraverse readNode inputs
    out <- readIORef gradOutput
    let gradInputs = backwards ins out
    liftA2_ writeGrad inputs gradInputs

{-
resetGrad :: Node output -> IO ()
resetGrad Node{gradUpdated} = do
  todo <- readIORef gradUpdated
  when todo $ do
    children' <- readIORef children
    rtraverse_ (\n -> Const <$> resetGrad n) children'
    writeIORef gradOutput 0
    writeIORef gradUpdated False

backprop :: Some Node -> IO ()
backprop (Some Node{..}) = do
  done <- readIORef gradUpdated
  unless done $ do
    children' <- readIORef children
    traverse backprop children
    ins <- rtraverse readNode inputs
    out <- readIORef gradOutput
    let gradInputs = backwards ins out
    rZipWith inputs gradInputs _
-}

