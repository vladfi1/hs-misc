{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds #-}

module Java where

import Language.Java.Syntax
import Language.Java.Parser

import qualified GHC.Generics as GHC
import Generics.SOP
import Generics.SOP.TH

import Data.Constraint

import GrammarNN
import JavaGeneric
import TypeLevel
import List




