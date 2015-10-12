{-# LANGUAGE TypeFamilies #-}

module Lazy where

import Data.IORef

newtype Thunk a = Thunk { runThunk :: IORef (Either (IO a) a) }

readThunk :: Thunk a -> IO a
readThunk (Thunk ref) = do
  e <- readIORef ref
  case e of
    Left io -> do
      a <- io
      writeIORef ref (Right a)
      return a
    Right a -> return a

mapThunk :: (a -> b) -> Thunk a -> IO (Thunk b)
mapThunk f = (fmap Thunk) . newIORef . Left . (fmap f) . readThunk
