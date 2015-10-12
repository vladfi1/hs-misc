{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Supply where

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except

import ListT

class Monad m => MonadSupply a m | m -> a where
  supply :: m a
  peek :: m a
  exhausted :: m Bool

newtype SupplyT s m a = SupplyT { runSupplyT :: StateT (ListT m s) m a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans (SupplyT s) where
  lift = SupplyT . lift

-- lucky that MonadState doesn't have monadic values in negative positions
instance MonadReader r m => MonadReader r (SupplyT s m) where
  ask = SupplyT ask
  reader = SupplyT . reader
  local f = SupplyT . (local f) . runSupplyT

instance MonadError e m => MonadError e (SupplyT s m) where
  throwError = SupplyT . throwError
  catchError (SupplyT error) handler = SupplyT $ catchError error (runSupplyT . handler)

instance MonadState s m => MonadState s (SupplyT s' m) where
  get = lift get
  put = lift . put
  state = lift . state

instance Monad m => MonadSupply s (SupplyT s m) where
  supply = SupplyT $ do
    ListT l <- get
    Cons a ml <- lift l
    put (ListT ml)
    return a
  
  peek = SupplyT $ do
    ListT l <- get
    Cons a ml <- lift l
    return a
  
  exhausted = SupplyT $ do
    ListT ml <- get
    l <- lift ml
    case l of
      Nil -> return True
      Cons _ _ -> return False

