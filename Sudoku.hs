{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}

module Sudoku where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import "mtl" Control.Monad.State

n :: Int
n = 3

type Index = (Int, Int)

data Square = Square {
  x :: (Int, Int),
  y :: (Int, Int)
} deriving (Eq, Ord, Show)

xy :: Square -> Index
xy Square {..} = (fst x, fst y)

type Number = Int
type Map' = Map Index (Set Number)

data BoardState = BoardState {
  grid :: Map',
  rows :: Map',
  cols :: Map',
  rest :: Set Square,
  board :: Map Square Number
}

indices = [(x, y) | x <- [1..n], y <- [1..n]]
squares = [Square {x, y} | x <- indices, y <- indices]
numbers = [1..n*n]

showNum :: Maybe Number -> String
showNum Nothing = " "
showNum (Just x) = show x

instance Show BoardState where
  show BoardState {board} =
    unlines $ map showRow indices
      where showRow y = concat [showNum $ getSquare y x | x <- indices]
            getSquare y x = Map.lookup Square {x, y} board

emptyMap' = Map.fromList [(i, Set.empty) | i <- indices]

initial :: BoardState
initial = BoardState {
  grid = emptyMap',
  rows = emptyMap',
  cols = emptyMap',
  rest = Set.fromList squares,
  board = Map.empty
}

--solve :: (MonadTrans t, MonadState BoardState (t [])) => t [] ()
solve :: StateT BoardState [] ()
solve = do
  boardState@BoardState{..} <- get
  
  let next = Set.findMin rest
      rest' = Set.delete next rest
      maps = [grid, rows, cols]
      ixs  = map ($ next) [xy, y, x]
      sets = zipWith (Map.!) maps ixs
  
  num <- lift numbers
  
  lift $ guard $ all (Set.notMember num) sets
  
  let sets' = [Set.insert num s | s <- sets]
      maps' = zipWith3 Map.insert ixs sets' maps
      [grid', rows', cols'] = maps'
  
  put $ boardState {
    grid = grid',
    rows = rows',
    cols = cols',
    rest = rest',
    board = Map.insert next num board
  }

  when (not $ Set.null rest') solve

allSolutions = map snd $ runStateT solve initial
