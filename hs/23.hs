module Main where

import AOC.Main (simpleMain)
import AOC.V2 (V2 (..))
import Data.List (find, foldl')
import Data.Maybe (fromJust, mapMaybe)

import qualified AOC.Grid as G
import qualified AOC.V2.Box as Box
import qualified Data.Map as Map
import qualified Data.Set as Set

data PrimaryDir = N | E | S | W

primaryDirs :: [PrimaryDir]
primaryDirs = [N, E, S, W]

data Dir = Primary PrimaryDir | NE | SE | SW | NW

dirs :: [Dir]
dirs = (Primary <$> primaryDirs) <> [NE, SE, SW, NW]

diagonals :: PrimaryDir -> [Dir]
diagonals = \case
  N -> [NW, NE]
  E -> [NE, SE]
  S -> [SE, SW]
  W -> [SW, NW]

moveCompass :: G.Pos -> Dir -> G.Pos
moveCompass (V2 x y) = \case
  Primary N -> V2 x (y - 1)
  Primary E -> V2 (x + 1) y
  Primary S -> V2 x (y + 1)
  Primary W -> V2 (x - 1) y
  NE -> V2 (x + 1) (y - 1)
  SE -> V2 (x + 1) (y + 1)
  SW -> V2 (x - 1) (y + 1)
  NW -> V2 (x - 1) (y - 1)

data State = State (Set.Set G.Pos) [PrimaryDir]

initialState :: String -> State
initialState input = State (parseGrid input) [N, S, W, E]
  where
    parseGrid = Map.keysSet . Map.filter (== '#') . G.fromString

proposeMoves :: [PrimaryDir] -> Set.Set G.Pos -> [(G.Pos, G.Pos)]
proposeMoves order0 original = mapMaybe (proposeMove order0) (Set.toList original)
  where
    free pos = all ((`Set.notMember` original) . moveCompass pos)
    proposeMove order pos
      | free pos dirs = Nothing
      | otherwise = do
          prim <- find (\d -> free pos (Primary d : diagonals d)) order
          pure (pos, moveCompass pos (Primary prim))

moveElves :: [(G.Pos, G.Pos)] -> Set.Set G.Pos -> Set.Set G.Pos
moveElves proposals original = foldl' move original proposals
  where
    occupied =
      Map.keysSet . Map.filter (>= 2) $
        Map.fromListWith (+) [(dst, 1 :: Int) | (_, dst) <- proposals]
    move acc (src, dst)
      | dst `Set.member` occupied = acc
      | otherwise = dst `Set.insert` (src `Set.delete` acc)

stepState :: State -> State
stepState (State pos dir) = State (moveElves proposals pos) (tail dir <> [head dir])
  where
    proposals = proposeMoves dir pos

elvesArea :: State -> Int
elvesArea (State pos _) = Box.area box - Set.size pos
  where
    box = fromJust (foldMap (Just . Box.fromV2) pos)

main :: IO ()
main = simpleMain $ \input ->
  let elves = initialState input
      stable !i s@(State p _) = if p == p' then i else stable (i + 1) s'
        where
          s'@(State p' _) = stepState s
   in (elvesArea (iterate stepState elves !! 10), stable (1 :: Int) elves)
