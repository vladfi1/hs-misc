{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts, DataKinds, PolyKinds #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}

module HListTest where

import Control.Lens
import Data.HList
import Map


--makeLabelable "name awesomeness glasses"
makeLabels ["name", "awesomeness", "glasses"]

andrew = name .=. "Andrew" .*.
         awesomeness .=. 8000 .*.
         glasses .=. True .*.
         emptyRecord

f rec = 2 * rec .!. awesomeness

{-
(:.) = HCons
infixr 2 :.

myList = 1 :. "1" :. HNil
-}
