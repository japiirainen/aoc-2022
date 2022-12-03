module Main where

import AOC.List.Extended (chunks)
import AOC.Main (simpleMain)
import Data.Char (isLower, ord)
import Data.List (intersect, nub)

main :: IO ()
main = simpleMain $ \input ->
  let halves xs = chunks (length xs `div` 2) xs
   in ( sum (map (score . halves) (lines input))
      , sum (map score (chunks 3 (lines input)))
      )

score :: [String] -> Int
score = sum . map priority . nub . foldr1 intersect

priority :: Char -> Int
priority c@(isLower -> True) = ord c - ord 'a' + 1
priority c = ord c - ord 'A' + 27