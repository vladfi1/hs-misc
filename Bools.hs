{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE GADTs #-}
--{-# LANGUAGE UndecidableInstances #-}

module Bools where

type family Not (b :: Bool) :: Bool where
  Not False = True
  Not True = False

type family And (b1 :: Bool) (b2 :: Bool) :: Bool where
  And False b = False
  And True b = b

type family Or (b1 :: Bool) (b2 :: Bool) :: Bool where
  Or False b = b
  Or True b = True

type family If (b :: Bool) (t :: k) (f :: k) :: k where
  If True t f = t
  If False t f = f

