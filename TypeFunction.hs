{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module TypeFunction where

type family F x where
  F Int = Int
  F x = Bool

class C x r | x -> r

instance C Int Int
instance {-# OVERLAPPABLE #-} C x Bool

