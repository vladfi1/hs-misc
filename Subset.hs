{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
--{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module Subset where

import Prelude hiding (Either(..))

--import GHC.TypeLits hiding (Nat)
import HList
import Nats
import Bools
import Map

data Tree a = Empty | Leaf a | Node (Tree a) (Tree a)

-- unify with Map from Map.hs?
type family TreeMap (f :: k1 -> k2) (t :: Tree k) :: Tree k2 where
  TreeMap f Empty = Empty
  TreeMap f (Leaf a) = Leaf (f a)
  TreeMap f (Node a b) = Node (TreeMap f a) (TreeMap f b)

type family TreeFold (id :: a) (f :: a -> a -> a) (t :: Tree a) :: b where
  TreeFold id f Empty = id
  TreeFold id f (Leaf a) = a
  TreeFold id f (Node a b) = f (TreeFold id f a) (TreeFold id f b)

type family TreeFold1 (f :: a -> a -> a) (t :: Tree a) :: b where
  TreeFold1 f (Leaf a) = a
  TreeFold1 f (Node a b) = f (TreeFold1 f a) (TreeFold1 f b)

type family Const1 (x :: k) :: Nat where
  Const1 x = S Z

type TreeSize t = TreeFold Z (:+:) (TreeMap Const1 t)

data HTree (t :: Tree *) where
  HEmpty :: HTree Empty
  HLeaf :: a -> HTree (Leaf a)
  HNode :: HTree l -> HTree r -> HTree (Node l r)

data Branch = Left | Right
  deriving (Eq, Ord, Show)

#define Path (List Branch)

lTail :: proxy (Cons h t) -> Proxy t
lTail _ = Proxy

class HTreeLookup (p :: Path) (t :: Tree *) where
  type HTreeLookupR p t :: *
  hTreeLookup :: proxy p -> HTree t -> HTreeLookupR p t
  
instance HTreeLookup Nil (Leaf a) where
  type HTreeLookupR Nil (Leaf a) = a
  hTreeLookup _ (HLeaf a) = a

instance (HTreeLookup p l) => HTreeLookup (Cons Left p) (Node l r) where
  type HTreeLookupR (Cons Left p) (Node l r) = HTreeLookupR p l
  hTreeLookup p (HNode l r) = hTreeLookup (lTail p) l

instance (HTreeLookup p r) => HTreeLookup (Cons Right p) (Node l r) where
  type HTreeLookupR (Cons Right p) (Node l r) = HTreeLookupR p r
  hTreeLookup p (HNode l r) = hTreeLookup (lTail p) r

type family GetLabel (t :: *) :: k where
  GetLabel (Tagged l v) = l

type TreeLabels t = TreeMap GetLabel t

-- These wouldn't be necessary if we took the Vinyl approach
-- and parameterized by a functor Field :: (l, *) -> *
-- Then we could just fmap getField over the HTree
-- The downside is that we would have to use pairs instead of
-- Data.Tagged, which might pose issues?
class TreeValues (t :: Tree *) where
  type TreeValuesR t :: Tree *
  treeValues :: HTree t -> HTree (TreeValuesR t)

instance TreeValues Empty where
  type TreeValuesR Empty = Empty
  treeValues HEmpty = HEmpty

instance TreeValues (Leaf (Tagged l v)) where
  type TreeValuesR (Leaf (Tagged l v)) = Leaf v
  treeValues (HLeaf (Tagged v)) = HLeaf v

instance (TreeValues t1, TreeValues t2) => TreeValues (Node t1 t2) where
  type TreeValuesR (Node t1 t2) = Node (TreeValuesR t1) (TreeValuesR t2)
  treeValues (HNode t1 t2) = HNode (treeValues t1) (treeValues t2)

type family HTreeContains (a :: k) (t :: Tree k) :: Bool where
  HTreeContains a Empty = False
  HTreeContains a (Leaf a) = True
  HTreeContains a (Node l r) = And (HTreeContains a l) (HTreeContains a r)


-- This is all a bit clumsy. Singletons promises better type-level programming.
-- 
type family HTreeFind (a :: k) (t :: Tree k) :: Maybe Path where
  HTreeFind a Empty = Nothing
  HTreeFind a (Leaf a) = Just Nil
  HTreeFind a (Node l r) =
    MaybeCase' (HTreeFind a l)
      (FMap (Cons Right) (HTreeFind a r))
      (Cons Left)
  
  --HTreeFind' a (HTreeFind a l) r

{-
type family HTreeFind' (a :: k) (p :: Maybe Path) (r :: Tree k) :: Maybe Path where
  HTreeFind' a (Just p) r = Just (Left ': p)
  HTreeFind' a Nothing r = AddRight (HTreeFind a r)

type family AddRight (p :: Maybe Path) :: Maybe Path where
  AddRight Nothing = Nothing
  AddRight (Just p) = Just (Right ': p)

type family FromJust (j :: Maybe k) :: k where
  FromJust (Just j) = j

type HTreePath a t = HTreeFind 

type HTreeField (l :: k) (t :: Tree *) (v :: *) =
  (

type HTreeField' 
-}
