{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Panic where

import GHC.TypeLits

type family TypeTable (s :: Symbol) :: k

class (KnownNat (Index t), t ~ TypeTable (Index t)) => Typeable t where
  type Index t :: Symbol

