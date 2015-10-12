{-# LANGUAGE NoMonomorphismRestriction,
             ScopedTypeVariables
             #-}


module PFP where

import Debug.Trace

import Data.Traversable
import Control.Applicative
import Control.Monad

import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Fix

import Numeric.SpecFunctions -- From cabal install spec-functions

--import Rec
import SplitEval
import Mem

bernoulli :: (Functor m, MonadRandom m) => Double -> m Bool
bernoulli p = (< p) <$> getRandom

-- From Alexey Radul's HSVenture
beta :: (MonadRandom m) => Double -> Double -> m Double
beta alpha beta = do
  -- Adapted from Statistics.Distribution.Beta; not reused because of
  -- funny randomness management convention.
  x <- getRandomR (0.0,1.0)
  return $ quantile x
    where
      quantile x | x == 0 = 0
                 | x == 1 = 1
                 | 0 < x && x < 1 = invIncompleteBeta alpha beta x
                 | otherwise = error $ "x must be in the range [0,1], got: " ++ show x

-- Makes a CRP sampler given the concentration parameter alpha via stick breaking.
makeCRP ::
  (RandomGen g, Functor m, MonadSplit g m, Functor f, MonadRandom f) =>
  Double -> m (f Int)
makeCRP alpha = do
  -- This is probably our only choice for this type
  -- without adding Proxy arguments. Oh well.
  let breakStick :: (RandomGen g) => Rand g Double
      breakStick = beta 1 alpha
  
  sticks <- repeatS breakStick
  
  let choose (p:ps) = do
        b <- bernoulli p
        if b
          then return 0
          else (1 +) <$> choose ps
  
  return $ choose sticks

-- This type was inferred by ghc, so should be the most general.
dpmem ::
  (MonadEval g n, MonadRandom f, MonadSplit g m, RandomGen g,
      Applicative n, Functor f, Functor m) =>
  Double -> n a -> m (f a)
dpmem alpha sampler = do
  crp <- makeCRP alpha
  tables <- memIntS (const sampler)
  return $ tables <$> crp

dpMemInt :: forall a g n m f.
  (MonadEval g n, MonadRandom f, MonadSplit g m, RandomGen g,
      Applicative n, Functor f, Functor m) =>
  Double -> (Int -> n a) -> m (Int -> f a)
dpMemInt alpha f = memIntS f'
  where -- This type needs to have both (MonadSplit g m) for dpmem
        -- and (MonadEval g n) for memIntS
        f' :: Int -> Rand g (f a)
        f' = (dpmem alpha) . f


dpBernoulli = dpmem 1.0 $ (bernoulli 0.5 :: (RandomGen g) => Rand g Bool)

randomWalkRec ::
  (Functor m, MonadRandom m) => Double -> (Int -> m Int) -> Int -> m Int
randomWalkRec _ _ 0 = return 0
randomWalkRec p f n = do
  x <- f (n-1)
  b <- bernoulli p
  let dx = if b then 1 else -1
  return $ x + dx

randomWalk
  :: (MonadSplit g m, RandomGen g, MonadFix m, Functor m) =>
     Double -> m (Int -> Int)
randomWalk p = mfix $ memIntS . f
  where f :: RandomGen g => (Int -> Int) -> (Int -> Rand g Int)
        f = (randomWalkRec p) . (return .)

dpRandomWalk :: forall g n m f.
  (MonadEval g n, MonadRandom n, MonadSplit g m, RandomGen g,
    Applicative n, MonadFix m, Functor m) =>
   Double -> Double -> m (Int -> n Int)
dpRandomWalk alpha p = mfix $ (dpMemInt alpha) . (randomWalkRec p)

dpRandomWalkIO alpha p = (evalRandIO .) <$> dpRandomWalk alpha p


