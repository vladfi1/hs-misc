{-# LANGUAGE
  DeriveFunctor, DeriveFoldable, DeriveTraversable,
  RecordWildCards, NamedFieldPuns,
  TemplateHaskell
  #-}
module InvertedPendulum where

import Data.Functor.Compose
import Control.Lens
import Physics
import Graphics.Gloss

data Variables a = Variables {
  _x :: a, -- position of cart
  _theta :: a -- angle of pendulum, ccw from vertical
} deriving (Show, Functor, Foldable, Traversable)

makeLenses ''Variables

instance Applicative Variables where
  pure a = Variables a a
  Variables f g <*> Variables a b = Variables (f a) (g b)

type Model = Compose Pair Variables

initialModel :: Floating a => Model a
initialModel = Compose $ Pair (Variables 0 (pi/2)) (pure 0)

data Constants a = Constants {
  m1 :: a, -- mass of cart
  m2 :: a, -- mass of pendulum
  l :: a, -- length of pendulum
  g :: a, -- strength of gravity
  d :: a -- damping coefficient
} deriving (Show)

defaultConstants :: Fractional a => Constants a
defaultConstants = Constants 2 1 1 9.8 0.1

data Controls a = Controls {
  f :: a
} deriving (Show, Functor, Foldable, Traversable)

instance Applicative Controls where
  pure = Controls
  Controls f <*> Controls a = Controls (f a)

nullControls :: Num a => Controls a
nullControls = Controls 0

type World a = (Model a, Controls a)

dynamics :: Floating a => Constants a -> World a -> Variables a
dynamics Constants {..} (Compose (Pair (Variables x theta) (Variables x' theta')), Controls {..}) = Variables x'' theta'' where
  sinTheta = sin theta
  cosTheta = cos theta
  x'' = (f - x' * d + m2 * sinTheta * (l * (sq theta') - g * cosTheta)) / (m1 + m2 * (sq sinTheta))
  theta'' = (g * sinTheta - x'' * cosTheta - d * theta') / l

cart = color red $ rectangleSolid 0.5 0.3
ball = color blue $ circleSolid 0.15

axes = pictures [xAxis, yAxis] where
  xAxis = line [(-10, 0), (10, 0)]
  yAxis = line [(0, -1), (0, 1)]

toPicture :: Constants Float -> World Float -> Picture
toPicture Constants {l} (Compose (Pair (Variables x theta) _), _) = scene where
  ballX = l * sin theta
  ballY = l * cos theta
  rod = line [(0, 0), (ballX, ballY)]
  ball' = translate ballX ballY ball

  system = translate x 0 $ pictures [cart, rod, ball']
  
  scene = scale 50 50 $ pictures [system, axes]

displayMode = InWindow "Inverted Pendulum" (400, 200) (50, 50)

--step :: Floating a => Constants a
step constants dt world@(model, controls) = (integrate dt model (dynamics constants world), controls)

--main = simulate displayMode white 60 initialVariables (toPicture defaultConstants) iter

--handleEvent (EventKey (SpecialKey KeyLeft) 
--handleEvent _ world = world

reward (Compose (Pair (Vairables x theta) _)) = cos theta - sq x

main = play displayMode white 60 (initialModel, nullControls) (toPicture defaultConstants) (const id) (step defaultConstants)

