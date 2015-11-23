{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module List where

import Data.Vinyl
import Data.Type.Equality
import Nats
--import Generics.SOP.Sing
import Data.Singletons.Prelude

{-
data SList l where
  SNil' :: SList '[]
  SCons' :: Sing a -> SList l -> SList (a ': l)

slist :: forall l. SingI l => SList l
slist = case sing :: Sing l of
  SNil -> SNil'
  SCons -> SCons' sing slist
-}

--class Offset

-- an index n into l such that l[n] = a
data Index (l :: [k]) a where
  ZIndex :: Index (t ': l) t
  SIndex :: Index l t -> Index (a ': l) t

index :: Index l a -> Rec f l -> f a
index ZIndex (a :& _) = a
index (SIndex i) (_ :& l) = index i l

instance TestEquality (Index l) where
  testEquality ZIndex ZIndex = Just Refl
  testEquality (SIndex i) (SIndex j) = do
    Refl <- testEquality i j
    return Refl
  testEquality _ _ = Nothing

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

{-
type family Len (l :: [k]) :: Nat where
  Len '[] = Nats.Z
  Len (a ': l) = Nats.S (Len l)

reifyLen :: SList l -> SNat (Len l)
reifyLen SNil' = SZ
reifyLen (SCons' _ l) = SS $ reifyLen l
-}
