{-# LANGUAGE RebindableSyntax #-}

module DAG where

import PHOAS hiding (var, let_)

import GHC.Err (error)

import Data.Functor
import Data.Profunctor

import Prelude (($), show, (.), (++), const, map, fromInteger, String)

data DAGF a b
  = Node [b]
  | Let b (a->b)

instance Functor (DAGF a) where
  fmap f (Node bs) = Node $ map f bs
  fmap f (Let b ab) = Let (f b) (f . ab)

instance Profunctor DAGF where
  dimap a2b c2d (Node cs) = Node $ map c2d cs
  dimap a2b c2d (Let c b2c) = Let (c2d c) (c2d . b2c . a2b)

showDAGF :: DAGF (b -> String) ([String] -> String) -> [String] -> String
showDAGF (Let b ab) (v:vs) = b vs ++ "\n" ++ "Node " ++ v ++ ": " ++ ab (const v) vs
showDAGF (Node bs) (v:vs) = show $ map ($ vs) bs

type DAG = Rec DAGF

showDAG :: DAG (b -> String) ([String] -> String) -> String
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
fail = error "failure is not an option"

empty = node []

dag = do
  a <- empty
  b <- empty
  c <- node [a, b]
  d <- node [a, b]
  node [c, d]

