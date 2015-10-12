{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Convertible where

import Control.Monad

class Convertible a b where
  convert :: a -> b

type Isomorphic a b = (Convertible a b, Convertible b a)

instance Convertible a a where
  convert = id

instance (Convertible a b, Convertible c d) => Convertible (a -> d) (b -> c) where
  convert f = convert . f . convert

{-
instance (Monad m) => Convertible a (m a) where
  convert = return
-}

instance (Monad m) => Convertible (m (m a)) (m a) where
  convert = join  
