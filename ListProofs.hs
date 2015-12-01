{-# LANGUAGE GADTs, DataKinds, TypeFamilies, TypeOperators, PolyKinds #-}

module ListProofs where

import Data.Singletons.Prelude

data MapP f l r where
  MapNil :: MapP f '[] '[]
  MapCons :: MapP f as bs -> MapP f (a ': as) (Apply f a ': bs)

mapP :: SList l -> MapP f l (Map f l)
mapP SNil = MapNil
mapP (SCons _ l) = MapCons (mapP l)

data FoldRP f b l r where
  FoldRNil :: FoldRP f b '[] b
  FoldRCons :: FoldRP f b l r -> FoldRP f b (a ': l) (f a r)

type ConcatP s l = FoldRP '(:) l s

concatNil :: SList l -> ConcatP l '[] l
concatNil SNil = FoldRNil
concatNil (SCons _ l) = FoldRCons (concatNil l)

--concatAssociative :: Concat s l sl -> Concat sl r slr -> Concat l r lr -> Concat s lr slr
--concatAssociative FoldRNil _ _ = FoldRNil

data FoldLP :: (b -> a -> b) -> b -> [a] -> b -> * where
  FoldLNil :: FoldLP f b '[] b
  FoldLCons :: FoldLP f (f b a) as r -> FoldLP f b (a ': as) r

-- a zipper with no focus
data ZipperP s l r where
  ZipperNil :: ZipperP r '[] r
  ZipperCons :: ZipperP (a ': s) l r -> ZipperP s (a ': l) r

type ReverseP = ZipperP '[]

{-
reverseReflexive :: Zippererse l r -> Reverse r l
reverseReflexive RevNil = RevNil
reverseReflexive (RevCons p) =
-}

data Length (l :: [k]) where
  LZero :: Length '[]
  LSucc :: Length l -> Length (a ': l)
