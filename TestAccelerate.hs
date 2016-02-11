module Main where

import Data.IORef
import Data.Array.Accelerate
--import Data.Array.Accelerate.CUDA
--import Data.Array.Accelerate.Interpreter

import Prelude hiding (zipWith, replicate, (++), sum, map)

type Matrix e = Array DIM2 e

dot :: (Elt a, IsNum a) => Acc (Vector a) -> Acc (Vector a) -> Acc (Scalar a)
dot xs ys = fold1 (+) (zipWith (*) xs ys)

norm xs = map sqrt (dot xs xs)

mult
  :: (Elt a, IsNum a) =>
     Acc (Matrix a) -> Acc (Vector a) -> Acc (Vector a)

mult m v = fold1 (+) (zipWith (*) m vs)
  where
    n = indexHead . indexTail $ shape m
    vs = replicate (lift $ Z :. n :. All) v

iter k = do
  let n = 1000 :: Int

  let m = fill (constant $ Z :. n :. n) (constant (1 :: Float))
  vRef <- newIORef $ fill (constant $ Z :. n) (constant 1)

  let iterate = do
      v <- readIORef vRef
      let mv = mult m v
          vv = norm v
          v' = map (/ the vv) mv
      --writeIORef vRef (compute v')
      writeIORef vRef v'

  traverse (const iterate) [1..k]

  readIORef vRef
  --print $ run (norm v)
