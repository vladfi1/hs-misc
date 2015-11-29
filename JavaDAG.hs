{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module JavaDAG where

import Data.Vector.Storable (toList, (!))

import Language.Java.Syntax
import Language.Java.Parser

import Generics.SOP
import Generics.SOP.NP
import Data.Vinyl
import Data.Singletons.Prelude hiding (And)

import Data.Default
import DefaultM

import GrammarNNDAG
import JavaGeneric
import GHC.TypeLits
import TensorHMatrix
import DAGIO
import Random
import List

chars = [' ' .. '~']
numChars = length chars -- 95

unsafeIndex :: Eq a => a -> [a] -> Int
unsafeIndex _ [] = error "Element not found."
unsafeIndex a (x:xs) =
  if a == x then 0
    else 1 + unsafeIndex a xs

data Java (p :: Nat)

type family JavaSize t where
  JavaSize Char = FromInteger 95
  JavaSize Int = FromInteger 1
  JavaSize Integer = FromInteger 1
  JavaSize Double = FromInteger 1
  JavaSize t = FromInteger 50

type instance Size Java t = JavaSize t

encodeParams :: (Default a, Usable a) => IO (NP (EncodeParams Java a) GenericTypes)
encodeParams = sequence'_NP $ cpure_NP (Proxy::Proxy (HasParams Java)) (Comp defM)

encodeChar :: Usable a => Encoder Java a Char
encodeChar = Encoder f where f c = Primitive . Repr <$> makeSource (oneHot $ unsafeIndex c chars)

encodeInt :: Usable a => Encoder Java a Int
encodeInt = Encoder f where f i = Primitive . Repr <$> makeSource (fromIntegral i)

encodeInteger :: Usable a => Encoder Java a Integer
encodeInteger = Encoder f where f i = Primitive . Repr <$> makeSource (fromIntegral i)

encodeDouble :: (Usable a, Fractional a) => Encoder Java a Double
encodeDouble = Encoder f where f d = Primitive . Repr <$> makeSource (realToFrac d)

testEncoding :: IO (Encoding Java Float CompilationUnit)
testEncoding = do
  java <- readFile "Test.java"
  let Right parsed = parser compilationUnit java

  params <- encodeParams

  let prim = encodeChar :& encodeInt :& encodeInteger :& encodeDouble :& RNil

  let encode = makeEncoder javaComplete params prim

  encode parsed

decodeParams :: (Default a, Usable a) => IO (NP (DecodeParams Java a) GenericTypes)
decodeParams = sequence'_NP $ cpure_NP (Proxy::Proxy (And (KnownCode Java) (And (KnownSize Java) (KnownSizes Java)))) (Comp defM)

decodeChar :: forall a. (Real a, Usable a) => Decoder Java a Char
decodeChar = Decoder f where
  f :: Repr' Java a Char -> IO Char
  f c = do
    Vector v <- evalNode (runRepr' c)
    sample $ zip chars (map toRational $ toList v)

decodeInt :: forall a. (RealFrac a, Usable a) => Decoder Java a Int
decodeInt = Decoder f where
  f :: Repr' Java a Int -> IO Int
  f i = do
    Vector v <- evalNode (runRepr' i)
    return . round $ v ! 0

decodeInteger :: forall a. (RealFrac a, Usable a) => Decoder Java a Integer
decodeInteger = Decoder f where
  f :: Repr' Java a Integer -> IO Integer
  f i = do
    Vector v <- evalNode (runRepr' i)
    return . round $ v ! 0

decodeDouble :: forall a. (Real a, Usable a) => Decoder Java a Double
decodeDouble = Decoder f where
  f :: Repr' Java a Double -> IO Double
  f i = do
    Vector v <- evalNode (runRepr' i)
    return . realToFrac $ v ! 0

javaDecoder :: IO (AnyDecoder Java Float AllTypes)
javaDecoder = do
  params <- decodeParams
  let prim = decodeChar :& decodeInt :& decodeInteger :& decodeDouble :& RNil
  return $ makeDecoder javaComplete params prim
