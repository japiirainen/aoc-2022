module Main where

import AOC.Main (pureMain)
import AOC.Search (bfs)
import AOC.V3 (V3 (..), boundingBox)
import Data.Ix (inRange)
import Data.Maybe (fromJust)

import qualified AOC.Parser as P
import qualified AOC.V3 as V3
import qualified Data.Set as Set

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

parseCubes :: P.Parser Char (Set.Set (V3 Int))
parseCubes = Set.fromList . map V3.fromList <$> cube `P.sepBy` P.newline
  where
    cube = P.decimal `P.sepBy` P.char ','

findSurroundingAir :: Set.Set (V3 Int) -> Set.Set (V3 Int)
findSurroundingAir cubes = Set.fromList $ bfs step (hi + 1)
  where
    (lo, hi) = fromJust (boundingBox (Set.toList cubes))
    box = (lo - 1, hi + 1)
    step c = [n | n <- neighbors c, inRange box n, n `Set.notMember` cubes]

neighbors :: forall a. Num a => V3 a -> [V3 a]
neighbors (V3 x y z) =
  [ V3 (x + 1) y z
  , V3 (x - 1) y z
  , V3 x (y + 1) z
  , V3 x (y - 1) z
  , V3 x y (z + 1)
  , V3 x y (z - 1)
  ]

main :: IO ()
main = pureMain $ \input -> do
  cubes <- P.runParser parseCubes input
  let air = findSurroundingAir cubes
  let part1 = length [error "smirk" | c <- Set.toList cubes, n <- neighbors c, n `Set.notMember` cubes]
  let part2 = length [error "smirk" | c <- Set.toList cubes, n <- neighbors c, n `Set.member` air]
  pure (pure part1, pure part2)