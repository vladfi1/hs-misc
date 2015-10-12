{-# LANGUAGE RecordWildCards #-}

module FactorGraph where

--import Data.Sequence
import Data.Map (Map, (!))
import qualified Data.Map as Map
--import Data.IntMap
import Data.Set (Set)
import qualified Data.Set as Set

type VKey = String
type FKey = Int

--type Keyed a = (Key, a)

data Factor value = Factor {
  vars :: Set VKey,
  weight :: (VKey -> value) -> Double,
  -- messages from variables
  marginals :: Map VKey [(value, Double)]
}

data Variable value = Variable {
  name :: VKey,
  values :: [value],
  -- messages from the factors
  weights :: Map FKey (value -> Double)
}

--mkVar key vals = Variable key vals empty

data FactorGraph value =
  FactorGraph {
    variables :: Map VKey (Variable value),
    factors :: Map FKey (Factor value),
    counter :: FKey
  }

emptyFactorGraph :: FactorGraph value
emptyFactorGraph =
  FactorGraph {
    variables = Map.empty,
    factors = Map.empty,
    counter = 0
  }

addVar :: VKey -> [v] -> FactorGraph v -> FactorGraph v
addVar key vals graph@FactorGraph{..} =
  graph { variables = Map.insert key (Variable key vals Map.empty) variables }

addFactor :: [VKey] -> ((VKey -> v) -> Double) -> FactorGraph v -> FactorGraph v
addFactor vkeys w graph@FactorGraph{..} =
  FactorGraph {
    variables = variables',
    factors = Map.insert counter factor factors,
    counter = counter + 1
  } where
    factor =
      Factor {
        vars = Set.fromList vkeys,
        weight = w,
        marginals = Map.fromList [(vkey, [(val, 1) | val <- values $ variables ! vkey]) | vkey <- vkeys]
      }
    adjustVar var = var { weights = Map.insert counter (const 1) (weights var) }
    variables' = foldr (Map.adjust adjustVar) variables vkeys

messageVF :: Variable v -> FKey -> [(v, Double)]
messageVF Variable{..} fkey = [(val, f val) | val <- values]
  where
    weights' = Map.elems $ Map.delete fkey weights
    f val = product [w val | w <- weights']

messageFV :: Factor v -> VKey -> v -> Double
messageFV Factor{..} vkey =
  \val -> sum [weight (Map.insert vkey val kvs !) * m | (kvs, m) <- domain]
  where
    -- might be able to smash these all into one list-monad do block?
    
    marginals' = do
      (k, vms) <- Map.assocs $ Map.delete vkey marginals
      return $ [((k, v), m) | (v, m) <- vms]
    
    prod = sequence marginals'
    
    -- this variable needs a better name than "domain"
    domain = map
      (\kvms ->
        let (kvs, ms) = unzip kvms in
        (Map.fromList kvs, product ms))
      prod



