{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, ConstraintKinds, PolyKinds #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module JavaGen where

--import Data.Constraint

import Generics.SOP
import Generics.SOP.Constraint

--import Constraints
import Random

import JavaGeneric
import Language.Java.Syntax

class Gen a where
  gen :: MonadDiscrete w m => m a

instance Gen Int where
  gen = uniform [-1, 0, 1]

instance Gen Char where
  gen = uniform [' ' .. '~']

instance Gen Integer where
  gen = uniform [-1, 0, 1]

instance Gen Double where
  gen = return 0

type family Gen' a :: Constraint where
  Gen' a = (Generic a, All2 Gen (Code a))

instance (SListI l, All Gen l) => Gen (NP I l) where
  gen = case (sList :: SList l) of
    SNil -> return Nil
    SCons -> (:*) <$> (I <$> gen) <*> gen

instance {-# OVERLAPPABLE #-} (Generic a, All2 Gen (Code a)) => Gen a where
  gen = uniform sums >>= (fmap (to . SOP))

{-
reifyAll :: forall c l. (SingI l, All c l) => NP (Dict :.: c) l
reifyAll = case (sing :: Sing l) of
  SNil -> Nil
  SCons -> Comp Dict :* reifyAll

reifyAll2 :: forall c l. (SingI l, All SingI l, All2 c l) => NP (NP (Dict :.: c)) l
reifyAll2 = case (sing :: Sing l) of
  SNil -> Nil
  SCons -> reifyAll :* reifyAll2

--proof :: All2 Gen ls => Dict (All (All Gen)
-}

sums :: forall w l m. (MonadDiscrete w m, All2 Gen l) => [m (NS (NP I) l)]
sums = case (sList :: SList l) of
  SNil -> []
  SCons -> (Z <$> gen) : map (fmap S) sums

--genExp :: MonadDiscrete w m -> m
