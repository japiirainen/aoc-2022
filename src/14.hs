import qualified AOC.Grid as G

import AOC.Main (pureMain)
import AOC.V2 (V2 (..))
import Data.List (find, foldl')
import Data.Maybe (isNothing)

import qualified AOC.Parser as P
import qualified AOC.V2.Box as Box
import qualified Data.Map as M
import qualified Data.Set as S

type StraightLines = [[G.Pos]]

parseStraightLines ∷ P.Parser Char StraightLines
parseStraightLines = line `P.sepBy1` P.newline
  where
    line = point `P.sepBy1` P.string " -> "
    point = V2 <$> P.signedDecimal <*> (P.char ',' *> P.signedDecimal)

drawStraightLines ∷ StraightLines → Either String (S.Set G.Pos)
drawStraightLines = go S.empty
  where
    go !acc ((p0 : p1 : ps) : sls) = do
      l ← line p0 p1
      go (acc <> S.fromList l) ((p1 : ps) : sls)
    go !acc (_ : sls) = go acc sls
    go !acc [] = Right acc
    line (V2 x0 y0) (V2 x1 y1)
      | x0 == x1 = Right [V2 x0 y | y ← [min y0 y1 .. max y0 y1]]
      | y0 == y1 = Right [V2 x y0 | x ← [min x0 x1 .. max x0 x1]]
      | otherwise = Left "not a straight line"

data Tile = Rock | Sand deriving (Eq, Show)

data Cave = Cave {caveGrid ∷ !(G.Grid Tile), caveBottom ∷ !Int}

caveSand ∷ Cave → Int
caveSand = length . filter (== Sand) . map snd . M.toList . (.caveGrid)

straightLinesToCave ∷ StraightLines → Either String Cave
straightLinesToCave sls = do
  rocks ← drawStraightLines sls
  let caveGrid = M.fromSet (const Rock) rocks
      caveBottom = maybe 0 (\b → b.bBottomRight.v2Y) $ G.box caveGrid
  pure Cave {..}

fall ∷ G.Pos → [G.Pos]
fall (V2 x y) = [bottom, left, right]
  where
    bottom = V2 x (y + 1)
    left = V2 (x - 1) (y + 1)
    right = V2 (x + 1) (y + 1)

dropSand ∷ G.Pos → Cave → Maybe G.Pos
dropSand p0 cave = go p0
  where
    go p = case find (isNothing . flip M.lookup cave.caveGrid) (fall p) of
      Nothing → Just p
      Just n → if n.v2Y > cave.caveBottom then Nothing else go n

pourSand ∷ G.Pos → Cave → Cave
pourSand p cave = case dropSand p cave of
  Nothing → cave
  Just rest → pourSand p cave {caveGrid = M.insert rest Sand cave.caveGrid}

fillSand ∷ G.Pos → Cave → Cave
fillSand p0 cave = cave {caveGrid = go (cave.caveGrid) p0}
  where
    go !grid p@(V2 _ y)
      | y >= cave.caveBottom = grid
      | p `M.member` grid = grid
      | otherwise = foldl' go (M.insert p Sand grid) (fall p)

main ∷ IO ()
main = pureMain $ \input → do
  sls ← P.runParser parseStraightLines input
  cave0 ← straightLinesToCave sls
  let cave1 = pourSand (V2 500 0) cave0
      cave2 = fillSand (V2 500 0) cave0 {caveBottom = cave0.caveBottom + 2}
  pure (pure (caveSand cave1), pure (caveSand cave2))
