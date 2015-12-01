{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module DSL where

import Numeric.AD

data Exp a
  = Const a
  | Neg (Exp a)
  | (Exp a) :+: (Exp a)
  | (Exp a) :*: (Exp a)
  deriving (Functor, Foldable, Traversable)

instance Num a => Num (Exp a) where
  (+) = (:+:)
  (*) = (:*:)
  fromInteger = Const . fromInteger
  negate = Neg

eval :: Num a => Exp a -> a
eval (Const a) = a
eval (Neg e) = negate (eval e)
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :*: e2) = eval e1 * eval e2


