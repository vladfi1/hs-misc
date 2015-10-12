{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}

module DAG where

import Nats

import Data.Vinyl.Functor
import Data.Vinyl

import Prelude hiding (reverse)

data Node f (env :: [k]) (args :: [k]) a where
  Node :: f args a -> Rec (Index env) args -> Node f env args a

data DAG f (ins :: [[k]]) (outs :: [k]) where
  Empty :: DAG f '[] '[]
  More :: Node f outs args a -> DAG f ins outs -> DAG f (args ': ins) (a ': outs)

newtype Forwards f args a = Forwards (Rec f args -> f a)

evalForwards :: DAG (Forwards f) ins outs -> Rec f outs
evalForwards Empty = RNil
evalForwards (More (Node (Forwards f) indices) dag) =
  let rest = evalForwards dag
      args = rmap (`index` rest) indices
  in  f args :& rest

data Edge (ins :: [[k]]) (outs :: [k]) where
  Edge :: Index outs a -> Index ins args -> Index args a -> Edge ins outs

incEdge :: Edge ins outs -> Edge (i ': ins) (o ': outs)
incEdge (Edge o i arg) = Edge (SIndex o) (SIndex i) arg


edges :: DAG f ins outs -> [Edge ins outs]
edges Empty = []
edges (More (Node _ back) dag) = new ++ old
  where
    old = map incEdge (edges dag)
    new = recordToList $ rZipWith (\o arg -> Const $ Edge (SIndex o) ZIndex arg) back (indices back)
    
data Backwards f args a = Backwards (Forwards f args a) (Rec f args -> f a -> Rec f args)

evalBackwards :: (forall a. f a -> f a -> f a) -> Rec f outs -> DAG (Backwards f) ins outs -> Rec f outs


--reverse

