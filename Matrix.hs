{-# LANGYAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Matrix where

import Nats

class Matrix m where
  (+) :: m a b -> m a b -> m a b
  (*) :: m a b -> m b c -> m a c

data Scalar 

instance Matrix 
