module ListProofs where

data Map f l r where
  MapNil :: Map f '[] '[]
  MapCons :: Map f as bs -> Map f (a ': as) (f a ': bs)

data FoldR f b l r where
  FoldRNil :: FoldR f b '[] b
  FoldRCons :: FoldR f b l r -> FoldR f b (a ': l) (f a r)

type Concat s l = FoldR '(:) l s

concatNil :: SList l -> Concat l '[] l
concatNil SNil' = FoldRNil
concatNil (SCons' _ l) = FoldRCons (concatNil l)

--concatAssociative :: Concat s l sl -> Concat sl r slr -> Concat l r lr -> Concat s lr slr
--concatAssociative FoldRNil _ _ = FoldRNil

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

data Length (l :: [k]) where
  LZero :: Length '[]
  LSucc :: Length l -> Length (a ': l)
