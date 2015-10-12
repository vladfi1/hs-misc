{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds, PolyKinds, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs, RankNTypes #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}

module HGraph where

import Data.HList
import Data.HList.Record
import Map

data HNode self nbrs where
  HNode :: HList (Map (NbrNode self) nbrs) -> HNode self nbrs

mkNode :: proxy self -> HList (Map (NbrNode self) nbrs) -> HNode self nbrs
mkNode _ = HNode

data NbrNode nbr self where
  NbrNode :: HasField nbr (Record nbrs) (HNode self nbrs') => HNode self nbrs -> NbrNode nbr self

aLabel :: Label "a"
aLabel = undefined

--aNode :: "a"
aNode :: HNode "a" '["a"]
aNode = HNode $ (NbrNode aNode) `HCons` HNil

