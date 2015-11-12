module DSL where

import Numeric.AD

data Exp a
  = Const a
  | Neg (Exp a)
  | (Exp a) :+: (Exp a)
  | (Exp a) :*: (Exp a)

instance Num a => Num (Exp a) where
  (+) = (:+:)
  (*) = (:*:)
  fromInteger = Const . fromInteger
  negate = Neg


