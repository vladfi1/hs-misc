module Num where

data N
  = N :+ N
  | N :* N
  | N :- N
  | Negate N
  | Abs N
  | Signum N
  | FromInteger Integer
  
  | N :/ N
  | Recip N
  | FromRational Rational
  
  | Pi
  | Exp N
  | Log N
  | Sin N
  | Cos N
  | Tan N
  | ASin N
  | ACos N
  | ATan N
  | Sinh N
  | Cosh N
  | Tanh N
  | ASinh N
  | ACosh N
  | ATanh N
  deriving (Show)

instance Num N where
  (+) = (:+)
  (*) = (:*)
  (-) = (:-)
  negate = Negate
  abs = Abs
  signum = Signum
  fromInteger = FromInteger

instance Fractional N where
  (/) = (:/)
  recip = Recip
  fromRational = FromRational

instance Floating N where
  pi = Pi
  exp = Exp
  log = Log
  sin = Sin
  cos = Cos
  tan = Tan
  asin = ASin
  acos = ACos
  atan = ATan
  sinh = Sinh
  cosh = Cosh
  tanh = Tanh
  asinh = ASinh
  acosh = ACosh
  atanh = ATanh

