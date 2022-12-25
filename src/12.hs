module Main where

import AOC.Dijkstra (Bfs (..), bfs)
import AOC.Main (pureMain)
import Data.Char (ord)
import Data.Maybe (isJust)
import Data.Traversable (for)

import qualified AOC.Grid.Bounded as G

data St a = St
  { grid ∷ G.Grid a
  , start ∷ G.Pos
  , goal ∷ G.Pos
  }
  deriving stock (Show)

parseGrid ∷ String → Either String (St Int)
parseGrid input = do
  grid0 ← G.fromString input
  start ← find grid0 'S'
  goal ← find grid0 'E'
  grid ← for grid0 $ \case
    'S' → pure (ord 'a')
    'E' → pure (ord 'z')
    c → pure (ord c)
  pure St {..}
  where
    find grid0 c =
      case [pos | (pos, c') ← G.toList grid0, c' == c] of
        [pos] → Right pos
        _ → Left $ "No " ++ show c ++ " found"

shortest ∷ (Int → Int → Bool) → (G.Pos → Bool) → G.Pos → G.Grid Int → Int
shortest valid isEnd start grid =
  let ns' p = [n | n ← G.neighbors p, isJust $ G.lookup n grid, valid (G.index n grid) (G.index p grid)]
   in maybe 0 (flip (-) 1 . length . snd) (bfs ns' isEnd start).bfsGoal

main ∷ IO ()
main = pureMain $ \input → do
  g ← parseGrid input
  pure
    ( pure (shortest (\n c → n - c < 2) (== g.goal) g.start g.grid)
    , pure (shortest (\n c → c - n < 2) ((== ord 'a') . flip G.index g.grid) g.goal g.grid)
    )
