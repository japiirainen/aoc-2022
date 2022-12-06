module Main where

import AOC.Main (pureMain)
import Control.Applicative ((<|>))
import Data.List (foldl', transpose)
import Data.Maybe (catMaybes)

import qualified AOC.Parser as P
import qualified Data.Map as Map

newtype Stacks stack item = Stacks {unStacks ∷ Map.Map stack [item]}

data S stack item = S
  { sMoves ∷ [(Int, stack, stack)]
  , sStacks ∷ Stacks stack item
  }

ap ∷ ∀ stack item t. (Ord stack, Foldable t) ⇒ t (Int, stack, stack) → Stacks stack item → Stacks stack item
ap m0 s0 = foldl' move s0 m0
  where
    move (Stacks s) (n, from, to) =
      Stacks $
        case Map.lookup from s of
          Nothing → s
          Just stack → let (t, r) = splitAt n stack in Map.insertWith (<>) to t $ Map.insert from r s

main ∷ IO ()
main = pureMain $ \raw → do
  input ← P.runParser parseInput raw
  let moves = do
        (n, from, to) ← input.sMoves
        replicate n (1, from, to)
      top = map (head . snd) . Map.toAscList . (.unStacks)
  pure
    ( pure (top $ ap moves input.sStacks)
    , pure (top $ ap input.sMoves input.sStacks)
    )

parseInput ∷ P.Parser Char (S Int Char)
parseInput = makeS <$> parseRows <*> parseLabels <* P.newline <*> parseMoves
  where
    makeS rs ls moves = S moves $ Stacks $ Map.fromList $ zip ls (map catMaybes $ transpose rs)
    parseRows = P.many1 $ pRow <* P.horizontalSpaces <* P.newline
    pRow = parseItem `P.sepBy1` P.space
    parseItem =
      Nothing <$ P.space <* P.space <* P.space
        <|> Just <$> (P.char '[' *> P.anyChar <* P.char ']')
    parseLabels = (P.space *> P.decimal <* P.space) `P.sepBy1` P.space <* P.newline
    parseMoves =
      ( (,,)
          <$> (P.string "move " *> P.decimal)
          <*> (P.string " from " *> P.decimal)
          <*> (P.string " to " *> P.decimal)
      )
        `P.sepBy1` P.newline
