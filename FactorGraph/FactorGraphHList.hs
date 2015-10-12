{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds, PolyKinds, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
--{-# LANGUAGE OverlappingInstances #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}

module FactorGraph where

import Control.Applicative

import Data.HList

import Data.Proxy
import GHC.TypeLits

import Map

data Factor vs where
  Factor :: (Record vs -> Double) -> Factor vs

data MyFactor v where
  MyFactor :: (HasField l (Record vs) a) =>
              Factor vs ->
              MyFactor (Tagged l a)

--data Factor' a = forall t. Factor' { getFactor :: Factor a t }

data Variable v where
  Variable :: [a] -> Variable (Tagged l a)

mkVar :: proxy l -> [a] -> Variable (Tagged l a)
mkVar _ = Variable

type Message = [Double]

-- Message from variable to factor
--type MessageVF v vs = [(Double, a)]
type MessageVF a = [(Double, a)]
--data MessageVF v vs where
--  MessageVF :: [(Double, a)] -> MessageVF (Tagged l a)

-- Message from factor to variable
--type MessageFV vs v = a -> Double
type MessageFV a = a -> Double
--data MessageFV where

{-
data FactorGraph vs =
  FactorGraph {
    variables :: Record (Map Variable vs),
    factors 
    messagesVF :: 
-}

-- need to specify the kind of l to avoid overlapping instances
bernoulli ::
    forall proxy (l :: Symbol).
    proxy l -> Double -> Factor '[Tagged l Bool]
--bernoulli :: Label l -> Double -> Factor '[Tagged l Bool]
bernoulli _ p = Factor f
  where --f :: Record '[Tagged l Bool] -> Double
        f = weight . (.!. (Label :: Label l))
        weight True = p
        weight False = 1-p

-- we can't use makeLabels anymore because we are mono-kinded (to Symbol)
-- maybe we want to do something fancy like singletons?
-- makeLabels ["bool"]
bool :: Label "bool"
bool = Label

--boolean :: Variable (Tagged "bool" Bool)
boolVar = mkVar bool [True, False] $ map MyFactor [bernoulli bool 0.5]


{-
marginal :: Variable t -> [Double]
marginal (Variable values factors) = [product [f v | f <- fs] | v <- values']
  where fs = map factor factors
        values' = map Field values  

--newtype ZippedVar a = ZippedVar { unzipVar :: [(a, Double)] }
type ZippedVar = Compose [] (Lift (,) (Const Double) ElField)

zipVar :: Variable t -> ZippedVar t
zipVar var@(Variable values _) =
  Compose $ map Lift $ zip (map Const $ marginal var) (map Field values)

--zippedVars variables = rmap zipVar variables

rsequence :: Applicative h => Rec (Compose h g) rs -> h (Rec g rs)
rsequence = rtraverse getCompose

runzip :: Rec (Lift (,) f g) rs -> (Rec f rs, Rec g rs)
runzip RNil = (RNil, RNil)
runzip ((Lift (fx, gx)) :& xs) = (fx :& fxs, gx :& gxs)
  where (fxs, gxs) = runzip xs

runzip' :: Rec ((,) a) rs -> ([a], Rec Identity rs)
runzip' RNil = ([], RNil)
runzip' ((a, x) :& axs) = (a:as, (Identity x) :& xs)
  where (as, xs) = runzip' axs

factor :: Factor t -> ElField t -> Double
factor (Factor variables f) =
  \v -> sum [m * (f $ v :& vs) | (m, vs) <- domain]
  where 
        zippedVars = rmap zipVar variables
        prod = rsequence zippedVars
        -- this variable needs a better name than "domain"
        domain = map
          (\vms ->
            let (ms, vs) = runzip vms in
            (product $ recordToList ms, vs))
          prod
-}
