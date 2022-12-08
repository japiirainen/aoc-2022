module Main where

import AOC.Coord (Coord (..), above, below, left, right)
import AOC.List.Extended (countBy)
import AOC.Main (simpleMain)
import Data.Array.Unboxed (IArray (..), Ix (inRange, range), UArray, listArray, (!))

main ∷ IO ()
main = simpleMain $ \input →
  let arr = toArray (lines input)
   in ( countBy (isVisible arr) (range (bounds arr))
      , maximum (map (scenicScore arr) (range (bounds arr)))
      )

isVisible ∷ UArray Coord Char → Coord → Bool
isVisible a c = any (clearView . toEdge a c) [above, below, left, right]
  where
    clearView [] = error "unreachable"
    clearView (x : xs) = all (< x) xs

toEdge ∷ UArray Coord Char → Coord → (Coord → Coord) → [Char]
toEdge a c d = [a ! i | i ← takeWhile (inRange (bounds a)) (iterate d c)]

scenicScore ∷ UArray Coord Char → Coord → Int
scenicScore a c = product (map (treesSeen . toEdge a c) [above, below, left, right])
  where
    treesSeen [] = error "unreachable"
    treesSeen (x : xs) = case break (>= x) xs of
      (d, []) → length d
      (d, _ : _) → length d + 1

toArray ∷ [String] → UArray Coord Char
toArray ls = listArray (C 0 0, C (length ls - 1) (length (head ls) - 1)) (concat ls)
