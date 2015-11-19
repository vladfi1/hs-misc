{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module JavaDAG where

import Language.Java.Syntax
import Language.Java.Parser

import Generics.SOP
import Generics.SOP.NP

import Data.Default
import DefaultM

import GrammarNNDAG
import JavaGeneric
import JavaGen
import TypeLevel
import GHC.TypeLits
import TensorHMatrix
import Utils
import DAGIO

data SList xs where
  SNil' :: SList '[]
  SCons' :: Sing x -> SList xs -> SList (x ': xs)

fromSing :: Sing xs -> SList xs
fromSing SNil = SNil'
fromSing SCons = SCons' sing (fromSing sing)

chars = [' ' .. '~']
numChars = length chars -- 95

unsafeIndex :: Eq a => a -> [a] -> Int
unsafeIndex _ [] = error "Element not found."
unsafeIndex a (x:xs) =
  if a == x then 0
    else 1 + unsafeIndex a xs

instance Neural Char where
  type Size Char = 95

instance {-# OVERLAPPABLE #-} Neural t where
  type Size t = 95

initialParams :: (Default a, Usable a) => IO (NP (EncodeParams a) GenericTypes)
initialParams = sequence'_NP $ cpure_NP (Proxy::Proxy HasParams) (Comp defM)

newtype Params ts a = Params (NP (EncodeParams a) ts)

instance Encode GenericTypes Char where
  encode _ c = Primitive . Repr <$> makeSource (oneHot $ unsafeIndex c chars)

instance Encode GenericTypes Int where
  encode _ i = Primitive . Repr <$> makeSource (fromIntegral i)

instance Encode GenericTypes Integer where
  encode _ i = Primitive . Repr <$> makeSource (fromIntegral i)

instance Encode GenericTypes Double where
  encode _ d = Primitive . Repr <$> (makeSource $ realToFrac d)

--encodeJava ::

main :: IO (Encoding Float CompilationUnit)
main = do
  ps <- initialParams
  java <- readFile "Test.java"
  let Right parsed = parser compilationUnit java
  encode ps parsed
