module Main where

import AOC.Main (simpleMain)
import Data.List (sort)
import qualified Data.Text as T

main :: IO ()
main = simpleMain $ \input -> (countCalories 1 input, countCalories 3 input)
  where
    parse = map (map (read @Int . T.unpack) . T.lines) . T.splitOn "\n\n" . T.pack
    countCalories n = sum . take n . reverse . sort . map sum . parse
