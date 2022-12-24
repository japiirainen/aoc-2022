module Main where

import AOC.Coord
import AOC.Main (simpleMain)
import Data.Ix (inRange)

import qualified Data.Map as Map
import qualified Data.Set as Set

getCoordMap ∷ String → (Map.Map Coord Char)
getCoordMap = Map.filter ('.' /=) . Map.fromList . coordLines . lines

dir ∷ Char → Coord
dir = \case
  '>' → east
  '<' → west
  '^' → north
  'v' → south
  _ → error "invalid direction"

location ∷ Coord → Coord → Int → Char → Coord
location _ here _ '#' = here
location corner here t c =
  zipCoord mod (here - 1 + scaleCoord t (dir c)) (corner - 1) + 1

worldAt ∷ Coord → Int → Map.Map Coord Char → Set.Set Coord
worldAt corner t input =
  Set.fromList [location corner k t v | (k, v) ← Map.toList input]

grow ∷ Coord → Set.Set Coord → Set.Set Coord → Set.Set Coord
grow corner world best =
  Set.fromList
    [ next
    | here ← Set.toList best
    , next ← here : cardinal here
    , inRange (0, corner) next
    , next `Set.notMember` world
    ]

main ∷ IO ()
main = simpleMain $ \raw →
  let input = getCoordMap raw
      corner = maximum (Map.keys input)
      target = corner + west

      solve t best land
        | land `Set.member` best = t
        | otherwise = solve (t + 1) (grow corner world best) land
        where
          world = worldAt corner (t + 1) input

      p0 = solve 0 (Set.singleton (C 0 1)) target
      p1 = solve p0 (Set.singleton target) (C 0 1)
      p2 = solve p1 (Set.singleton (C 0 1)) target

   in (p0, p2)
