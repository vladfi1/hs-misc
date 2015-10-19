import Data.STRef

module DAGST where

import Data.Vinyl

data Node input out
  = Node
  { forwards :: Rec f input -> out
  --, backwards :: Rec f input -> out -> Rec f input
  }

data Module input output


