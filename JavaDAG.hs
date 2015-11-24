{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module JavaDAG where

import Language.Java.Syntax
import Language.Java.Parser

import Generics.SOP
import Generics.SOP.NP
import Data.Vinyl

import Data.Default
import DefaultM

import GrammarNNDAG
import JavaGeneric
import GHC.TypeLits
import TensorHMatrix
import DAGIO

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

encodeChar :: Usable a => Encoder Java a Char
encodeChar = Encoder f where f c = Primitive . Repr <$> makeSource (oneHot $ unsafeIndex c chars)

encodeInt :: Usable a => Encoder Java a Int
encodeInt = Encoder f where f i = Primitive . Repr <$> makeSource (fromIntegral i)

encodeInteger :: Usable a => Encoder Java a Integer
encodeInteger = Encoder f where f i = Primitive . Repr <$> makeSource (fromIntegral i)

encodeDouble :: (Usable a, Fractional a) => Encoder Java a Double
encodeDouble = Encoder f where f d = Primitive . Repr <$> (makeSource $ realToFrac d)

main :: IO (Encoding Java Float CompilationUnit)
main = do
  java <- readFile "Test.java"
  let Right parsed = parser compilationUnit java

  params <- initialParams

  let prim = encodeChar :& encodeInt :& encodeInteger :& encodeDouble :& RNil

  let encode = makeEncoder javaComplete params prim

  encode parsed
