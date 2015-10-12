--{-# LANGUAGE ImpredicativeTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Impredicative where

import Data.Functor.Identity

wrap :: (forall a. Identity (a->a)) -> forall a. a->a
wrap i = runIdentity i
