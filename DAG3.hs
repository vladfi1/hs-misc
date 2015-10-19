{-# LANGUAGE RebindableSyntax #-}

module DAG where

import PHOAS hiding (var, let_)

import GHC.Err (undefined)
import Data.Functor

import Prelude (($), show, (.), (++), const, map, fromInteger)

data DAGF a b
  = Node [b]
  | Let b (a->b)

instance Functor (DAGF a) where
  fmap f (Node bs) = Node $ map f bs
  fmap f (Let b ab) = Let (f b) (f . ab)

type DAG = Rec DAGF

showDAGF (Node bs) vs = show $ map ($ vs) bs
showDAGF (Let b ab) (v:vs) =
  b vs ++ "\n" ++ "Node " ++ v ++ ": " ++ ab (const v) vs

showDAG dag = cata showDAGF dag $ map show [1..]

{-
data DAG a
  = Node [DAG a]
  | Let (DAG a) (a -> DAG a)
  | Var a
-}

return = Place
var = Place
let_ a f = Roll $ Let a f
(>>=) = let_
node = Roll . Node . map var
fail = undefined

empty = node []

dag = do
  a <- empty
  b <- empty
  c <- node [a, b]
  d <- node [a, b]
  node [c, d]

