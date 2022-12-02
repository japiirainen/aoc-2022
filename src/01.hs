module Main where

import AOC.Main (simpleMain)
import Data.List (sortBy)
import qualified Data.Text as T

main :: IO ()
main = simpleMain $ \input -> (countCalories 1 input, countCalories 3 input)
  where
    parse = map (map (read @Int . T.unpack) . T.lines) . T.splitOn "\n\n" . T.pack
    countCalories n = sum . take n . sortBy (flip compare) . map sum . parse
