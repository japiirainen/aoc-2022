module Main where

import AOC.Main (pureMain)

import qualified AOC.Parser as P

main ∷ IO ()
main = pureMain $ \input → do
  let parseLine = ((,) <$> P.signedDecimal <*> (P.char ',' *> P.signedDecimal)) `P.sepBy1` P.string " -> "
  ls ← P.runParser (parseLine `P.sepBy` P.newline) input
  pure (pure (show ls), pure (length ls))
