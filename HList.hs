{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE FunctionalDependencies #-}
--{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module HList 
  ( module HList
  , module Data.HList
  ) where

import Data.HList
import Data.HList.HArray

{-
-- It appears that replacing the typeclass with a closed type family
-- allows us to avoid the OverlappingInstances in Data.HList.
type family HEq (x :: k) (y :: k) :: Bool where
  HEq x x = True
  HEq x y = False

-- from Data.HList.Record
-- requires UndecidableInstanes
class HasField (l::k) (r :: [*]) v | l r -> v where
    hLookupByLabel :: label l -> HList r -> v

instance (b ~ HEq l l1, HasField' b l (Tagged l1 v1 ': r) v)
    => HasField l (Tagged l1 v1 ': r) v where
    hLookupByLabel l r =
             hLookupByLabel' (Proxy::Proxy b) l r

class HasField' (b::Bool) (l :: k) (r :: [*]) v | b l r -> v where
    hLookupByLabel':: proxy b -> label l -> HList r -> v

instance HasField' True l (Tagged l v ': r) v where
    hLookupByLabel' _ _ (HCons (Tagged v) _) = v

instance HasField l r v => HasField' False l (fld ': r) v where
    hLookupByLabel' _ l (HCons _ r) = hLookupByLabel l r
-}

-- alternative to HFind
type family HIndex (e :: k) (l :: [k]) :: HNat where
  HIndex e (e ': l) = 'HZero
  HIndex e (e1 ': l) = 'HSucc (HIndex e l)

type HRecordIndex l r = HIndex l (RecordLabels r)

hRecordIndex :: Proxy l -> Proxy r -> Proxy (HRecordIndex l r)
hRecordIndex _ _ = Proxy

type HField' n vs v = (HLookupByHNat n vs, v ~ HLookupByHNatR n vs)

type HField l r v =
--  (RecordValues r, HField' l v (RecordLabels r) (RecordValuesR r))
  (RecordValues r, HField' (HRecordIndex l r) (RecordValuesR r) v)

hField :: forall l r v. (HField l r v) => Proxy l -> HList r -> v
hField _ = hLookupByHNat (Proxy::Proxy (HRecordIndex l r)) . recordValues'

