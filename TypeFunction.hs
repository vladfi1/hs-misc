{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module TypeFunction where

import Data.Type.Equality

type family F x where
  F Int = Int
  F x = Bool

-- candidate type-level "function"
class C x r | x -> r

{- Fails FunDep checker.
instance C Int Int
instance {-# OVERLAPPABLE #-} C x Bool
-}

{- GHC can't invert FunDep :(
invert :: (C x r, C y s) => x :~: y -> r :~: s
invert Refl = Refl
-}

