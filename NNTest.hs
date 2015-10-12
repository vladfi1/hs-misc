{-# LANGUAGE DeriveDataTypeable #-}

module NNTest where

import NN

import NN.Backend.Torch as Torch

import Control.Lens
import Data.Data

graphFile = "./graph.png"
torchFile = "./graph.lua"

showNet = do
  png graphFile . visualize . parse

torchNet net =
  case Torch.backend $ parse net of
    Just s -> writeFile torchFile s
    Nothing -> putStrLn "Torch output failed."

nn1 = layer' relu

diag x = (x, x)
with = return . diag

nn2 = do
    x <- layer' $ def & ty Data
    y <- layer' relu
    x >-> y
    with y >- sequential [conv, softmax]
    with x >- sequential [lrn, ip 10, accuracy 5]
    return ()

data List a = Nil | Cons a (List a)
  deriving (Eq, Show, Typeable, Data)

--main = nn1 showNet
