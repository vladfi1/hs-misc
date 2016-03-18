module RL where

import Data.Functor.Compose
import Data.Functor.Product
import Control.Applicative

type LinearActor model controls = Compose controls model
type LinearCritic model controls = Product model controls

dot a b = foldl (+) 0 $ liftA2 (*) a b

linearActor (Compose actor) model = dot model <$> actor

generate step policy = f where
  f current = (current, action) : f next where
    action = policy current
    next = step (current, action)


