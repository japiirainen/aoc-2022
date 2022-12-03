module AOC.List.Extended where

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

