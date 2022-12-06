module Main where

import AOC.Main (pureMain)
import Control.Applicative ((<|>))

import qualified AOC.Parser as P

data A = AA | AB | AC
data B = BY | BX | BZ

outcome ∷ A → B → Int
outcome AA BX = 3
outcome AA BY = 6
outcome AA BZ = 0
outcome AB BX = 0
outcome AB BY = 3
outcome AB BZ = 6
outcome AC BX = 6
outcome AC BY = 0
outcome AC BZ = 3

shape ∷ B → Int
shape = \case
  BX → 1
  BY → 2
  BZ → 3

choose ∷ A → B → B
choose a b = head [b' | b' ← [BX, BY, BZ], outcome a b' == desired b]
  where
    desired = \case BX → 0; BY → 3; BZ → 6

main ∷ IO ()
main = pureMain $ \input → do
  parsed ← P.runParser parser input
  let solve f = sum . map (\(a, b) → let b' = f a b in outcome a b' + shape b')
  pure (pure $ solve (const id) parsed, pure $ solve choose parsed)

parser ∷ P.Parser Char [(A, B)]
parser =
  let pA = AA <$ P.char 'A' <|> AB <$ P.char 'B' <|> AC <$ P.char 'C'
      pB = BX <$ P.char 'X' <|> BY <$ P.char 'Y' <|> BZ <$ P.char 'Z'
   in P.sepBy1 ((,) <$> (pA <* P.spaces) <*> pB) P.newline
