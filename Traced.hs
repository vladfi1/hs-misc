{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, KindSignatures #-}

module Expression where

import Debug.Trace
import Control.Monad.Free

import Data.Map

-- type-level representation of the shape of an expression
data ExpK =
  VarT |
  ValT |
  AppT ExpK ExpK |
  LamT ExpK

data Exp (shape :: ExpK) (l :: [*]) (t :: *) where
  Val :: t -> Exp ValT l t
  Var :: (KnownSymbol s, HasField s (Record l) t) => Label s -> Exp l t
  App :: Exp l (t1 -> t2) -> Exp l t1 -> Exp l t2
  Lam :: (KnownSymbol s) => Label s -> Exp (Tagged s b ': l) t -> Exp l (b -> t)

data Traced t a where
  Ground' :: a -> Traced RetT a
  Arrow' :: (Traced t' a -> Trace t a) -> Traced 

data Trace (t :: ExpK) (a :: *) where
  TraceVar :: Type a -> Trace VarT a
  TraceRet :: Type a -> Trace RetT a
  
  TraceApp :: Type a -> Trace t1 a -> Trace t2 a -> Trace (AppT t1 t2) a
  TraceLam :: (Type a -> Trace t a) -> Trace (LamT t) a


