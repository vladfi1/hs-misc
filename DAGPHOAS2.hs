{-# LANGUAGE RebindableSyntax #-}

module DAG where

import PHOAS2 hiding (var, let_)

import GHC.Err (error)

import Data.Functor

import Prelude (($), show, (.), (++), const, map, fromInteger, String)

data DAGF a b
  = Node [b]
  | Let b (a->b)

instance Functor (DAGF a) where
  fmap f (Node bs) = Node $ map f bs
  fmap f (Let b ab) = Let (f b) (f . ab)

type DAG a = Rec (DAGF a)

{-
data DAG a b
  = Node [DAG a b]
  | Let (DAG a b) (a -> DAG a b)
  | Var b
-}


showDAGF :: DAGF (b -> String) ([String] -> String) -> [String] -> String
showDAGF (Let b ab) (v:vs) = v ++ " <- " ++ b vs ++ "\n" ++ ab (const v) vs
showDAGF (Node bs) (v:vs) = show $ map ($ vs) bs

showDAG :: DAG (b -> String) ([String] -> String) -> String
showDAG dag = cata showDAGF dag vars

return = Place
var = Place
let_ a f = Roll (Let (node a) f)
(>>=) = let_
node = Roll . Node . map var
fail = error "failure is not an option"

empty = node []

dag = do
  a <- []
  b <- []
  c <- [a, b]
  d <- [a, b]
  node [c, d]

