{-# LANGUAGE ConstraintKinds, PolyKinds, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module CFunctor where

import Generics.SOP.Constraint
import Numeric.LinearAlgebra hiding (C)

class CFunctor f where
  type C f :: * -> Constraint
  cfmap :: (C f a, C f b) => (a -> b) -> f a -> f b

{-
instance {-# OVERLAPPABLE #-} Functor f => CFunctor f where
  type C f = Top
  cfmap = fmap
-}

instance CFunctor Vector where
  type C Vector = And Element (Container Vector)
  cfmap = cmap

