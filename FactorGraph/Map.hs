{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds, PolyKinds, KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Map (Map, hmap) where

import Data.HList

type family Map (f :: k1 -> k2) (l :: [k1]) :: [k2]

type instance Map f '[] = '[]
type instance Map f (e ': l) = (f e) ': (Map f l)

-- Might as well use Vinyl if we're going to do this?
hmap :: (forall a. a -> f a) -> HList l -> HList (Map f l)
hmap _ HNil = HNil
hmap f (HCons h t) = HCons (f h) (hmap f t)

f :: a -> [a]
f = return

xs :: HList '[Int, [Char]]
xs = (1 :: Int) .*. "1" .*. HNil

--fxs :: HList '[[Int], [[Char]]]
fxs = hmap f xs

type family FoldR (f :: a -> b -> b) (end :: b) (l :: [a]) :: b
type instance FoldR f end '[] = end
type instance FoldR f end (e ': l) = f e (FoldR f end l)

{- Undecidable instances?
type family FoldL (f :: b -> a -> b) (acc :: b) (l :: [a]) :: b
type instance FoldL f acc '[] = acc
type instance FoldL f acc (e ': l) = FoldL f (f acc e) l
-}

type family ZipWith (f :: a -> b -> c) (as :: [a]) (bs :: [b]) :: [c]
type instance ZipWith f as '[] = '[]
type instance ZipWith f '[] bs = '[]
type instance ZipWith f (a ': as) (b ': bs) = (f a b) ': ZipWith f as bs

-- this requires UndecidableInstances, but is probably ok?
--type family Zip (as :: [a]) (bs :: [b]) :: [(a, b)]
--type instance Zip as bs = ZipWith '(,) as bs

