module RecList where

import Data.Foldable

import Prelude hiding (foldr)

data RecursiveList a
  = RecursiveListLeaf [a] 
  | RecursiveListNode [RecursiveList a]

instance Functor RecursiveList where
  fmap f (RecursiveListLeaf leaf) = RecursiveListLeaf $ map f leaf
  fmap f (RecursiveListNode node) = RecursiveListNode $ map (fmap f) node

instance Foldable RecursiveList where
  foldr f b (RecursiveListLeaf leaf) = foldr f b leaf
  foldr f b (RecursiveListNode node) = foldr (flip $ foldr f) b node


