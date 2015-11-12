{-# LANGUAGE RankNTypes, GADTs, PolyKinds #-}

module VinylUtils where

import Data.Vinyl

rtraverse_ :: Applicative h => (forall x. f x -> h (g x)) -> Rec f rs -> h ()
rtraverse_ f (x :& xs) = f x *> rtraverse_ f xs
rtraverse_ _ RNil = pure ()

