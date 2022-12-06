module Main where

import AOC.List.Extended (countBy)
import AOC.Main (pureMain)
import Control.Applicative (many)

import qualified AOC.Parser as P

parseInput ∷ P.Parser Char [((Int, Int), (Int, Int))]
parseInput = many $ (,) <$> (pr <* P.char ',') <*> pr
  where
    pr = (,) <$> P.decimal <* P.char '-' <*> (P.decimal <* P.spaces)

inRange ∷ (Ord a) ⇒ a → (a, a) → Bool
inRange x (a, b) = x >= a && x <= b

main ∷ IO ()
main =
  pureMain $
    fmap
      ( \ip →
          ( pure (countBy (\((a1, b1), (a2, b2)) → inRange a1 (a2, b2) && inRange b1 (a2, b2) || inRange a2 (a1, b1) && inRange b2 (a1, b1)) ip)
          , pure (countBy (\((a1, b1), (a2, b2)) → inRange a1 (a2, b2) || inRange b1 (a2, b2) || inRange a2 (a1, b1) || inRange b2 (a1, b1)) ip)
          )
      )
      . P.runParser parseInput
