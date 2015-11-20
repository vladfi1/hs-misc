{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module TypePattern where

import Bools

data Any0
data Any1 a
data Any2 a b
data Any3 a b c
data Any4 a b c d
data Any5 a b c d e

type family Matches (t :: k1) (p :: k2) :: Bool where
  Matches t Any0 = True
  Matches t Any1 = True
  Matches t Any2 = True
  Matches t Any3 = True
  Matches t Any4 = True
  Matches t Any5 = True

  Matches t t = True
  Matches (f t) (g p) = And (Matches f g) (Matches t p)
  Matches t p = False 

{-
data Type = Base | App Type Type

class Shape t (s :: Type) | t -> s
instance (Shape f fs, Shape t ts) => Shape (f t) (App fs ts)
instance Shape t Base

class Matches t p (b :: Bool) | t p -> b

instance Matches t t True

instance Matches t Any0 True
instance Matches t Any1 True
instance Matches t Any2 True
instance (Matches f g False) => Matches (f t) (g p) False

instance b ~ False => Matches t p b
-}

type family Lookup (table :: [(k, v)]) (key :: k) :: Maybe v where
  Lookup '[] key = Nothing
  Lookup ('(k, v) : t) key = If (Matches key k) (Just v) (Lookup t key)

type family Lookup' (table :: [(k, v)]) (key :: k) :: v where
  Lookup' ('(k, v) : t) key = If (Matches key k) v (Lookup' t key)

--instance Matches t p b => Search (
