{-# LANGUAGE GADTs, DataKinds, PolyKinds, KindSignatures, TypeOperators #-}

module Reverse where

data Foldl :: (b -> a -> b) -> b -> [a] -> b -> * where
  Empty :: Foldl f b '[] b
  NonEmpty :: proxy1 f -> proxy2 b -> proxy3 (a ': as) -> Foldl f (f b a) as r -> Foldl f b (a ': as) r
