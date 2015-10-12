{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}

import Prelude hiding (repeat)

repeat :: Int -> (a -> a) -> a -> a
repeat 1 f x = f x
repeat n f x = n `seq` x `seq` repeat (n-1) f $ f x

---- Buggy version
------------------

type Numerical a = (Fractional a, Real a)

data Box a = Box
    { func :: forall num. (Numerical num) => num -> a -> a
    , obj :: !a }

do_step :: (Numerical num) => num -> Box a -> Box a
do_step number Box{..} = Box{ obj = func number obj, .. }

start :: Box Double
start = Box { func = \x y -> realToFrac x + y
            , obj = 0 }

test :: Int -> IO ()
test steps = putStrLn $ show $ obj $ repeat steps (do_step 1) start

---- Driver
-----------

main :: IO ()
main = test 20000 -- compare test2 10000000 or test3 10000000, but test4 20000

---- No tuple constraint synonym is better
------------------------------------------

data Box2 a = Box2
    { func2 :: forall num. (Fractional num, Real num) => num -> a -> a
    , obj2 :: !a }

do_step2 :: (Fractional num, Real num) => num -> Box2 a -> Box2 a
do_step2 number Box2{..} = Box2{ obj2 = func2 number obj2, ..}

start2 :: Box2 Double
start2 = Box2 { func2 = \x y -> realToFrac x + y
              , obj2 = 0 }

test2 :: Int -> IO ()
test2 steps = putStrLn $ show $ obj2 $ repeat steps (do_step2 1) start2

---- Not copying the function field works too
---------------------------------------------

do_step3 :: (Numerical num) => num -> Box a -> Box a
do_step3 number b@Box{..} = b{ obj = func number obj }

test3 :: Int -> IO ()
test3 steps = putStrLn $ show $ obj $ repeat steps (do_step3 1) start

---- But record wildcards are not at fault
------------------------------------------

do_step4 :: (Numerical num) => num -> Box a -> Box a
do_step4 number Box{func = f, obj = x} = Box{ obj = f number x, func = f }

test4 :: Int -> IO ()
test4 steps = putStrLn $ show $ obj $ repeat steps (do_step4 1) start
