{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE ConstraintKinds #-}

module Id where

newtype Id a = Id { runId :: a }
  deriving (Functor, Foldable, Traversable)

