{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds, KindSignatures, PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}

module FactorGraph where

import Control.Applicative hiding (Const)

import Data.Vinyl
import Data.Vinyl.Functor
import Data.Vinyl.Derived

import Data.Proxy
import GHC.TypeLits

import VinylTest

data Factor v where
  Factor :: Rec Variable ts ->
            (FieldRec ts -> ElField v -> Double) ->
            Factor v

--data Factor' a = forall t. Factor' { getFactor :: Factor a t }

data Variable t where
  Variable :: (KnownSymbol s) =>
              [a] ->
              [Factor '(s, a)] ->
              Variable '(s, a)

bernoulli :: forall s. Double -> Factor '(s, Bool)
bernoulli p = Factor RNil f
  where --f :: forall rs. ('(s, Bool) ∈ rs) => FieldRec rs -> Double
        f _ = weight . getField
        weight True = p
        weight False = 1-p

boolean :: Variable '("bool", Bool)
boolean = Variable [True, False] $ [bernoulli 0.5]

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
  \v -> sum [m * (f' v) | (m, f') <- domain]
  where 
        zippedVars = rmap zipVar variables
        prod = rsequence zippedVars
        -- this variable needs a better name than "domain"
        domain = map
          (\vms ->
            let (ms, vs) = runzip vms in
            (product $ recordToList ms, f vs))
          prod

