{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures, ScopedTypeVariables #-}

module Symbol where

import Data.Singletons.TH

data Symbol = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Z
  deriving (Eq, Ord, Show)

genSingletons [''Symbol]

