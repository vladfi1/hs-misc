module Proof where

data Void

test :: Void -> a
test _ = undefined
