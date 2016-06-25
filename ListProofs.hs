{-# LANGUAGE GADTs, DataKinds, TypeFamilies, TypeOperators, PolyKinds #-}

module ListProofs where

import Data.Singletons.Prelude
import Data.Type.Equality
import FunctionProofs

{-
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

z2 :: SList r -> ZipperP s l r -> ReverseP l l' -> ConcatP s l' r
z2 r ZipperNil ZipperNil = concatNil r
--z2 r (ZipperCons p1) (ZipperCons p2) = FoldRCons _

--z1 :: ZipperP s l r -> ZipperP l s r' -> ReverseP r r'
--z1 ZipperNil ZipperNil = 

reverseReflexive :: ReverseP l r -> ReverseP r l
reverseReflexive ZipperNil = ZipperNil
reverseReflexive (ZipperCons p) =

data Length (l :: [k]) where
  LZero :: Length '[]
  LSucc :: Length l -> Length (a ': l)

-}

appendNil :: SList l -> (l :~: l :++ '[])
appendNil SNil = Refl
appendNil (SCons _ l) = case appendNil l of Refl -> Refl

appendCommutative :: Commutative (:++$)
appendCommutative = Commutative f where
  f :: Sing a -> Sing b -> a :++ b :~: b :++ a
  f SNil b = appendNil b

blah :: Associative (:++$)
blah = Associative f where
  f :: Sing a -> Sing b -> Sing c -> (a :++ b) :++ c :~: a :++ (b :++ c)

  f SNil _ _ = Refl
  f (SCons _ a) b c = case f a b c of Refl -> Refl


