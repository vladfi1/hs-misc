{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module List where

import Data.Vinyl

data SList l where
  SNil :: SList '[]
  SCons :: SList l -> SList (a ': l)

class ReifyList l where
  sList :: SList l

instance ReifyList '[] where
  sList = SNil

instance ReifyList l => ReifyList (a ': l) where
  sList = SCons sList

data Map f l r where
  MapNil :: Map f '[] '[]
  MapCons :: Map f as bs -> Map f (a ': as) (f a ': bs)

data FoldR f b l r where
  FoldRNil :: FoldR f b '[] b
  FoldRCons :: FoldR f b l r -> FoldR f b (a ': l) (f a r)

type Concat s l = FoldR '(:) l s

concatNil :: SList l -> Concat l '[] l
concatNil SNil = FoldRNil
concatNil (SCons l) = FoldRCons (concatNil l)

--concatAssociative :: Concat s l sl -> Concat sl r slr -> Concat l r lr -> Concat s lr slr

data FoldL :: (b -> a -> b) -> b -> [a] -> b -> * where
  FoldLNil :: FoldL f b '[] b
  FoldLCons :: FoldL f (f b a) as r -> FoldL f b (a ': as) r

-- a zipper with no focus
data Zipper s l r where
  ZipperNil :: Zipper r '[] r
  ZipperCons :: Zipper (a ': s) l r -> Zipper s (a ': l) r

type Reverse = Zipper '[]

{-
reverseReflexive :: Zippererse l r -> Reverse r l
reverseReflexive RevNil = RevNil
reverseReflexive (RevCons p) = 
-}

--class Offset 

-- an index n into l such that l[n] = a
data Index (l :: [k]) a where
  ZIndex :: Index (t ': l) t
  SIndex :: Index l t -> Index (a ': l) t

index :: Index l a -> Rec f l -> f a
index ZIndex (a :& _) = a
index (SIndex i) (_ :& l) = index i l

indices :: Rec f l -> Rec (Index l) l
indices RNil = RNil
indices (a :& l) = ZIndex :& rmap SIndex (indices l)

class Find l a where
  find :: Index l a

instance {-# OVERLAPS #-} Find (a ': l) a where
  find = ZIndex

instance Find l a => Find (b ': l) a where
  find = SIndex find

rZipWith :: (forall x. f x -> g x -> h x) -> Rec f l -> Rec g l -> Rec h l
rZipWith _ RNil RNil = RNil
rZipWith f (fa :& fl) (ga :& gl) = f fa ga :& rZipWith f fl gl

data Length (l :: [k]) where
  LZero :: Length '[]
  LSucc :: Length l -> Length (a ': l)

