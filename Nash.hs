module Nash where

import Numeric.LinearProgramming
import Data.List (transpose)

rps :: [[Double]]
rps = [ [0, 1, -1],
        [-1, 0, 1],
        [1, -1, 0] ]

solve payoffs = simplex prob constraints []
  where
    n = length payoffs
    prob = Minimize $ replicate n 0
    positive = (replicate n 1) :==: 1
    constraints = Dense $ positive : (map (:>=: 0) (transpose payoffs))

--pokemon :: [[Double]]

data Type = Normal
          | Fire
          | Water
          | Electric
          | Grass
          | Ice
          | Fighting
          | Poison
          | Ground
          | Flying
          | Psychic
          | Bug
          | Rock
          | Ghost
          | Dragon
          | Dark
          | Steel
          | Fairy
  deriving (Eq, Ord, Show, Bounded, Enum)

-- zero, half, normal, double
data Effectiveness = Z | H | N | D
  deriving (Eq, Ord, Show)

fight e1 e2 =
  case compare e1 e2 of
    LT -> -1
    EQ -> 0
    GT -> 1

typeChart = [
  [N, N, N, N, N, N, N, N, N, N, N, N, H, Z, N, N, H, N],
  [N, H, H, N, D, D, N, N, N, N, N, D, H, N, H, N, D, N],
  [N, D, H, N, H, N, N, N, D, N, N, N, D, N, H, N, N, N],
  [N, N, D, H, H, N, N, N, Z, D, N, N, N, N, H, N, N, N],
  [N, H, D, N, H, N, N, H, D, H, N, H, D, N, H, N, H, N],
  [N, H, H, N, D, H, N, N, D, D, N, N, N, N, D, N, H, N],
  [D, N, N, N, N, D, N, H, N, H, H, H, D, Z, N, D, D, H],
  [N, N, N, N, D, N, N, H, H, N, N, N, H, H, N, N, Z, D],
  [N, D, N, D, H, N, N, D, N, Z, N, H, D, N, N, N, D, N],
  [N, N, N, H, D, N, D, N, N, N, N, D, H, N, N, N, H, N],
  [N, N, N, N, N, N, D, D, N, N, H, N, N, N, N, Z, H, N],
  [N, H, N, N, D, N, H, H, N, H, D, N, N, H, N, D, H, H],
  [N, D, N, N, N, D, H, N, H, D, N, D, N, N, N, N, H, N],
  [Z, N, N, N, N, N, N, N, N, N, D, N, N, D, N, H, N, N],
  [N, N, N, N, N, N, N, N, N, N, N, N, N, N, D, N, H, Z],
  [N, N, N, N, N, N, H, N, N, N, D, N, N, D, N, H, N, H],
  [N, H, H, H, N, D, N, N, N, N, N, N, D, N, N, N, H, D],
  [N, H, N, N, N, N, D, H, N, N, N, N, N, N, D, D, H, N]]

map2 f = map (map f)
zip2 as bs = map (uncurry zip) (zip as bs)

pokemon :: [[Double]]
pokemon = zipWith (zipWith fight) typeChart (transpose typeChart)

pokemonResults = 
  [(Water,0.1875),(Electric,0.0625),
  (Grass,0.0625),(Ground,0.125),
  (Ghost,0.0625),(Dragon,0.1875),
  (Steel,0.1875),(Fairy,0.125)]

decks = [ "Patron Warrior"
        , "Hybrid Hunter"
        , "Oil Rogue"
        , "Zoo Lock"
        , "Fast Druid"
        , "Midrange Hunter"
        , "Hand Lock"
        , "Maly Lock"
        , "Tempo Mage"
        , "Freeze Mage"
        , "Face Hunter"
        , "Mech Shaman"
        , "Ramp Druid"
        , "Dragon Warrior"
        ]

matchups = [
  [  5, 3.5, 2.5, 7.5,   6,   6,   2,   4, 5.5,   7, 6.5, 3.5,   3,   4],
  [6.5,   5, 4.5,   5,   7,   6,   8,   6, 5.5,   6,   3, 7.5,   4, 4.5],
  [7.5, 5.5, 5, 5, 6, 6.5, 3.5, 5.5, 6.5, 2, 2.5, 3, 7, 4.5],
  [2.5, 5, 5, 5, 7.5, 7, 4, 5.5, 3.5, 3, 6, 6, 4, 7],
  [4, 3, 4, 2.5, 5, 4.5, 6, 6.5, 7, 3.5, 5, 2, 3, 6],
  [4, 4, 3.5, 3, 5.5, 5, 7.5, 5.5, 5, 6, 2.5, 2, 4.5, 5.5],
  [8, 2, 6.5, 6, 4, 2.5, 5, 3.5, 6.5, 2.5, 3, 3, 7, 2.5],
  [6, 4, 4.5, 4.5, 3.5, 4.5, 6.5, 5, 4, 5.5, 3.5, 4, 7, 5.5],
  [4.5, 4.5, 3.5, 6.5, 3, 5, 3.5, 6, 5, 7, 6, 3.5, 5, 4.5],
  [3, 4, 8, 7, 6.5, 4, 7.5, 4.5, 3, 5, 5.5, 5.5, 6.5, 3],
  [3.5, 7, 7.5, 4, 5, 7.5, 7, 6.5, 4, 4.5, 5, 7.5, 2, 5],
  [6.5, 2.5, 7, 4, 8, 8, 7, 6, 6.5, 4.5, 2.5, 5, 5, 6],
  [  7,   6,   3,   6,   7, 5.5,   3,   3,   5, 3.5,   8,   5,   5,   4],
  [  6, 5.5, 5.5,   3,   4, 4.5, 7.5, 4.5, 5.5,   7,   5,   4,   6,   5]]

normalized = map2 (subtract 5) matchups

hearthstoneResults =
  [("Hybrid Hunter",0.12110296857533229)
  ,("Oil Rogue",9.1789839771457e-2)
  ,("Zoo Lock",0.16159483294000748)
  ,("Tempo Mage",0.17488510744006952)
  ,("Freeze Mage",5.2912681654452884e-2)
  ,("Face Hunter",5.030431002359955e-2)
  ,("Mech Shaman",7.66364426779282e-2)
  ,("Ramp Druid",0.10011178735560801)
  ,("Dragon Warrior",0.17066202956154522)]

