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

import Control.Monad.Fix


chars = [' ' .. '~']
numChars = length chars -- 95

unsafeIndex :: Eq a => a -> [a] -> Int
unsafeIndex _ [] = error "Element not found."
unsafeIndex a (x:xs) =
  if a == x then 0
    else 1 + unsafeIndex a xs

data Java

type family JavaSize t where
  JavaSize Char = 95
  JavaSize Int = 1
  JavaSize Integer = 1
  JavaSize Double = 1
  JavaSize t = 50

type instance Size Java t = JavaSize t

initialParams :: (Default a, Usable a) => IO (NP (EncodeParams Java a) GenericTypes)
initialParams = sequence'_NP $ cpure_NP (Proxy::Proxy (HasParams Java)) (Comp defM)

--newtype Params ts a = Params (NP (EncodeParams a) ts)

instance EncodeRec GenericTypes AllTypes Java Char where
  encodeRec _ _ = Encoder f where
    f c = Primitive . Repr <$> makeSource (oneHot $ unsafeIndex c chars)

instance EncodeRec GenericTypes AllTypes Java Int where
  encodeRec _ _ = Encoder f where
    f i = Primitive . Repr <$> makeSource (fromIntegral i)

instance EncodeRec GenericTypes AllTypes Java Integer where
  encodeRec _ _ = Encoder f where
    f i = Primitive . Repr <$> makeSource (fromIntegral i)

instance EncodeRec GenericTypes AllTypes Java Double where
  encodeRec _ _ = Encoder f where
    f d = Primitive . Repr <$> (makeSource $ realToFrac d)

main :: IO (Encoding Java Float CompilationUnit)
main = do
  java <- readFile "Test.java"
  let Right parsed = parser compilationUnit java

  ps <- np2Rec <$> initialParams

  let encode = makeEncoder ps (Proxy::Proxy AllTypes)

  encode parsed
