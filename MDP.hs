{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module MDP where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Ord
import Data.Foldable

import qualified Algebra.Ring as Ring
import qualified Algebra.Module as Module
import qualified Algebra.Field as Field

import Random

import Prelude hiding (sum)

data State r s = State
  { actions :: [Action r s]
  , reward :: r
  }

data Action r s = Action { outcome :: (MonadDiscrete r m) => m s}

data MDP r s = MDP { states :: Map s (State r s) }

--solve :: MDP 

solve MDP {..} _ 0 = Map.map (const []) states

solve mdp@MDP {..} discount n = Map.map maximize states
  where
    next = solve mdp discount (n-1)
    
    eval Action {..} = expectation score
      where score = fmap (next Map.!) outcome
    
    total score = sum $ zipWith discount [1..] score
    
    maximize State {..} = reward : maximumBy (comparing total) (map eval actions)

