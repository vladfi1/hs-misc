{-# LANGUAGE TypeOperators, TupleSections #-}

module Infix where

data a :*: b = Foo a b
data a :* b = a :* b

type Fst a = (:*) a
--type Snd b = (:* b)


second x = (:* x)
unSnd x = (, x)
