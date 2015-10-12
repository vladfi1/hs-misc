{-# LANGUAGE NoMonomorphismRestriction #-}

module PolyTuple where

class Blah a

someNum :: Num a => a
someNum = undefined

--a :: Show a => a -> String
(a,b) = (show, ())
--b :: Show a => a -> String
--b = show
