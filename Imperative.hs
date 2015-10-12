{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Imperative where

import Data.IORef
import Control.Applicative

class ConvertM m t a where
  convert :: t -> m a

instance ConvertM m (m a) a where
  convert = id

instance (Monad m) => ConvertM m a a where
  convert = return

instance (Monad m, ConvertM m t a) => ConvertM m (m t) a where
  convert = (>>= convert)

var = newIORef

a += b = modifyIORef a (+ b)
a -= b = modifyIORef a (- b)
a *= b = modifyIORef a (* b)

lift f = (fmap f) . convert
lift2 f a b = f <$> (convert a) <*> (convert b)

infixl 7 *.
(*.) = lift2 (*)

infix 4 <.
(<.) = lift2 (<)

ifM p a b = do
  b <- convert p
  if b
    then convert a
    else convert b

--whileM :: (Monad m) => m Bool -> 

whileM pred body = do
  p <- pred
  if p then do
    body
    whileM pred body
  else return ()

fact1 n = do
  m <- var 1
  
  while 
  
  return m
