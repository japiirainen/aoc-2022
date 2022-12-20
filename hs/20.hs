{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import AOC.Main (simpleMain)
import Data.Sequence (Seq)

import qualified Data.Sequence as Seq

solve :: Int -> [Int] -> Int
solve n xs = sum (map (\i -> sq `Seq.index` ((zero + i) `mod` Seq.length sq)) [1000, 2000, 3000])
  where
    sq = simulate n xs
    Just zero = Seq.elemIndexL 0 sq

simulate :: Int -> [Int] -> Seq Int
simulate n seed = go (Seq.fromList (zip [1 ..] seed)) indices
  where
    indices = concat (replicate n [1 .. length seed])
    go sq [] = snd <$> sq
    go sq (i : is) = go (Seq.insertAt destination (i, target) (back <> a)) is
      where
        Just idx = Seq.findIndexL ((== i) . fst) sq
        (a, (_, target) Seq.:<| back) = idx `Seq.splitAt` sq
        destination = target `mod` (Seq.length sq - 1)

main :: IO ()
main = simpleMain $ \input ->
  let xs = map (read @Int) $ lines input
   in (solve 1 xs, solve 10 (map (* 811589153) xs))
