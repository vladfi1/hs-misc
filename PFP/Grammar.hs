module Grammar where

data Terminal t = Terminal t

data Unary t = Unary String (t -> t)


