module Main where

import AOC.Main (simpleMain)
import Data.List (nub)

data Dir = U | R | D | L
  deriving stock (Show, Read)

parseInst ∷ String → (Dir, Int)
parseInst s = case words s of
  [dir, dist] → (read dir, read dist)
  _ → error "parseInst: bad input"

{- | Turn instructions into a full path
 Eg (R, 3) -> [R, R, R]
-}
toPath ∷ (Dir, Int) → [Dir]
toPath (d, n) = replicate n d

move ∷ C → Dir → C
move c = \case
  U → above c
  R → right c
  D → below c
  L → left c

-- | tail -> head -> new-tail
moveTail ∷ C → C → C
moveTail t@(C ty tx) (C hy hx) =
  if abs (ty - hy) < 2 && abs (tx - hx) < 2
    then t
    else C (signum (hy - ty) + ty) (signum (hx - tx) + tx)

main ∷ IO ()
main = simpleMain $ \input → do
  let path = concatMap (toPath . parseInst) $ lines input
      headPath = scanl move origin path
      tailPaths = iterate (scanl moveTail origin) headPath
  (length $ nub (tailPaths !! 1), length $ nub (tailPaths !! 9))

data C = C !Int !Int
  deriving stock (Eq)

origin ∷ C
origin = C 0 0

above ∷ C → C
above (C y x) = C (y + 1) x

below ∷ C → C
below (C y x) = C (y - 1) x

left ∷ C → C
left (C y x) = C y (x - 1)

right ∷ C → C
right (C y x) = C y (x + 1)
