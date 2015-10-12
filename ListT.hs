{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module ListT where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

data MList m a = Nil | Cons a (ListT' m a)
  deriving (Functor)

type ListT' m a = m (MList m a)

newtype ListT m a = ListT { runListT :: ListT' m a }
  deriving (Functor)

instance Functor m => Monoid (MList m a) where
  mempty = Nil
  mappend Nil l = l
  mappend (Cons a ml) l = Cons a $ flip mappend l <$> ml

-- the trick is to pattern match on both arguments to <*>
-- this allows us to observe when the result is nonempty (when both args are Cons)
-- thus allowing us to apply Cons
instance Applicative m => Applicative (MList m) where
  pure a = Cons a (pure Nil)
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f mfs <*> as@(Cons a mas) =
    Cons (f a) $ mappend <$> (fmap f <$> mas) <*> ((<*> as) <$> mfs)

instance Applicative m => Monoid (ListT m a) where
  mempty = ListT (pure Nil)
  mappend (ListT l1) (ListT l2) = ListT $ mappend <$> l1 <*> l2

instance Applicative m => Applicative (ListT m) where
  pure = ListT . pure . pure
  ListT fs <*> ListT as = ListT $ (<*>) <$> fs <*> as

instance Monad m => Monad (ListT m) where
  return = pure
  l >>= f = ListT $ join' =<< mll
    where
      ListT mll = (runListT . f) <$> l
      
      join' Nil = return Nil
      join' (Cons ml mll) = do
        l <- ml
        mappend l <$> (mll >>= join')

instance Applicative m => Alternative (ListT m) where
  empty = mempty
  (<|>) = mappend

instance Monad m => MonadPlus (ListT m) where
  mzero = mempty
  mplus = mappend

instance MonadTrans ListT where
  lift ma = ListT $ fmap pure ma

