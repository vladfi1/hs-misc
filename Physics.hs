{-# LANGUAGE
  DeriveFunctor, DeriveFoldable, DeriveTraversable,
  TemplateHaskell
  #-}

module Physics where

import Control.Lens
import Data.Functor.Compose
import Control.Applicative

data Pair a = Pair {
  _val :: a,
  _ddt :: a
} deriving (Show, Functor, Foldable, Traversable)

makeLenses ''Pair

instance Applicative Pair where
  pure a = Pair a a
  Pair f g <*> Pair a b = Pair (f a) (g b)

sq x = x * x

integrate :: (Fractional a, Applicative f) => a -> Compose Pair f a -> f a -> Compose Pair f a
integrate dt (Compose (Pair v v')) v'' =
  Compose $ Pair (foldl1 (liftA2 (+)) [v, (dt *) <$> v', (* (0.5 * sq dt)) <$> v'' ]) (liftA2 (+) v' ((dt *) <$> v''))


