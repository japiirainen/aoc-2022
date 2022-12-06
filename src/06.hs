module Main where

import AOC.Main (simpleMain)
import Data.List (findIndex, nub, tails)

main :: IO ()
main = simpleMain $ \input ->
  let solve n =
        fmap (+ n) $
          findIndex (\xs -> length (nub (take n xs)) == n) $
            tails input
   in (solve 4, solve 14)