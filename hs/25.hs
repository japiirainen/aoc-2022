module Main where

import AOC.Main (simpleMain)
import Data.List (foldl')

from ∷ Char → Int
from = \case
  '=' → -2
  '-' → -1
  '0' → 0
  '1' → 1
  '2' → 2
  c → error ("invalid 'from' input " <> show c)

to ∷ Int → Char
to = \case
  2 → '2'
  1 → '1'
  0 → '0'
  (-1) → '-'
  (-2) → '='
  i → error ("invalid 'to' input " <> show i)

fromSnafu ∷ String → Int
fromSnafu = foldl' f 0
  where
    f acc c = from c + 5 * acc

toSnafu ∷ Int → String
toSnafu = go ""
  where
    go acc 0 = acc
    go acc n = go (to m : acc) ((n - m) `div` 5)
      where
        m =
          let x = n `mod` 5
           in if x > 2 then x - 5 else x

main ∷ IO ()
main = simpleMain $ \input →
  (toSnafu (sum (fromSnafu <$> lines input)), ())
