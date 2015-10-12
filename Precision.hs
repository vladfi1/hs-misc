module Precision where

import Control.Monad.Random

data Stream a = Stream a (Stream a)
--data BitSequence = BitSequence Int (Stream Bool)
type BitSequence = Stream Bool

xor a b = if a then not b else b

sumBit a b c = a `xor` b `xor` c
carryBit a b c = (a && b) || ((a || b) && c)

add :: [Bool] -> [Bool] -> [Bool]
add as [] = as
add [] bs = bs
add (a:as) (b:bs) = r : s : cs
  where c:cs = add as bs
        s = sumBit a b c
        r = carryBit a b c

testAdd = do
  as <- getRandoms
  bs <- getRandoms
  return $ add as bs

test0 = evalRand testAdd (mkStdGen 0)

trues = True : trues
falses = False : falses

test1 = add trues trues
test2 = add trues falses -- doesn't terminate
