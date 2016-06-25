{-# LANGUAGE
  RankNTypes, ScopedTypeVariables
  #-}

module RL where

import Data.Functor.Compose
import Data.Functor.Product
import Control.Applicative
import Numeric.AD

--generate :: 
generate step policy = f where
  f current = (current, action) : f next where
    action = policy current
    next = step (current, action)

sq x = x * x

updateCritic :: forall t critic state action. (Num t, Traversable critic, Applicative critic, Traversable state, Traversable action) =>
  (forall t. Num t => critic t -> state t -> action t -> t) -> critic t -> (forall t. Num t => state t -> t) -> t -> Compose [] (Product state action) t -> t -> critic t
updateCritic q critic reward discount (Compose experience) alpha = let
  qDiff (Pair critic' (Pair state' action')) = q critic' state' action'
  
  q' critic' state' action' = dCritic where
    Pair dCritic _ = grad qDiff (Pair critic' (Pair state' action'))

  --q' :: forall t'. Num t' => critic t' -> state t' -> action t' -> action t'
  --q' critic' s' = grad (q critic' s')
  delta ((Pair s a), (Pair s' a')) = (alpha * (reward s' + discount * q critic s' a' - q critic s a) *) <$> q' critic s a
  
  ds = map delta (zip experience (tail experience))
  
  in foldl (liftA2 (+)) critic ds

updateActor :: (Num t, Traversable actor, Applicative actor, Traversable critic, Traversable state) =>
  (forall t. Num t => actor t -> state t -> action t) -> actor t -> (forall t. Num t => critic t -> state t -> action t -> t) -> critic t -> Compose [] state t -> t -> actor t
updateActor p actor q critic states alpha = let
  reward (Pair actor' critic') s = q critic' s $ p actor' s
  
  total (Pair actor_critic (Compose states')) = sum $ reward actor_critic <$> states'
  
  -- only care about actor gradient
  Pair (Pair delta _) _ = grad total (Pair (Pair actor critic) states)
  
  in liftA2 (+) actor ((alpha *) <$> delta)

type LinearActor state action = Compose action state
type LinearCritic state action = Product state action

dot a b = foldl (+) 0 $ liftA2 (*) a b

linearActor :: (Num t, Functor action, Applicative state, Foldable state) => LinearActor state action t -> state t -> action t
linearActor (Compose actor) state = dot state <$> actor

linearCritic :: (Num t, Foldable state, Applicative state, Foldable action, Applicative action) => LinearCritic state action t -> state t -> action t -> t
linearCritic (Pair state' action') state action = dot state' state + dot action' action

