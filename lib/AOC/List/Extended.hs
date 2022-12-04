module AOC.List.Extended where

import Data.List (foldl')

select :: [a] -> [(a, [a])]
select [] = []
select (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- select xs]

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  case splitAt n xs of
    (a, b) -> a : chunks n b

compose :: [a -> a] -> a -> a
compose = foldr (.) id

countBy :: (Foldable t) => (a -> Bool) -> t a -> Int
countBy p = foldl' (\n x -> if p x then n + 1 else n) 0

count :: (Foldable t, Eq a) => a -> t a -> Int
count = countBy . (==)