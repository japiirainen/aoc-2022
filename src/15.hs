module Main where

import AOC.Box (Box (..), size, subtractBox)
import AOC.Coord (Coord (C), manhattan)
import AOC.Main (pureMain)
import AOC.Nat (Nat (..))
import Data.List (foldl')

import qualified AOC.Parser as P
import qualified Control.Monad as Monad

data Pos = Pos !Int !Int deriving stock (Show)

data Sensor = Sensor !Pos !Pos deriving stock (Show)

parseSensor ∷ P.Parser Char Sensor
parseSensor = Sensor <$> (P.string "Sensor at " *> pos) <*> (P.string ": closest beacon is at " *> pos)
  where
    pos = Pos <$> (P.string "x=" *> P.signedDecimal) <*> (P.string ", y=" *> P.signedDecimal)

part1 ∷ [Sensor] → Int
part1 ss = sum . map size . subAllOf (beaconsAtY ss 2_000_000) . disjoint $ concatMap (ranges 2_000_000) ss

part2 ∷ [Sensor] → Int
part2 ss = head do
  C y x ← map fromDiamond $ subAllOf (toDiamonds ss) [toDiamond (C 2_000_000 2_000_000) 4_000_000]
  Monad.guard $ 0 <= y && y <= 4_000_000 && 0 <= x && x <= 4_000_000
  pure (4_000_000 * x + y)

fromDiamond ∷ Box ('S ('S 'Z)) → Coord
fromDiamond (Dim xpy _ (Dim xmy _ Pt)) = C ((xpy - xmy) `div` 2) ((xpy + xmy) `div` 2)

toDiamond ∷ Coord → Int → Box ('S ('S 'Z))
toDiamond (C y x) r = cover (x + y) r (cover (x - y) r Pt)

toDiamonds ∷ [Sensor] → [Box ('S ('S 'Z))]
toDiamonds input =
  [toDiamond (C sy sx) (manhattan (C sy sx) (C by bx)) | Sensor (Pos sx sy) (Pos bx by) ← input]

beaconsAtY ∷ [Sensor] → Int → [Box ('S 'Z)]
beaconsAtY ss y = [cover bx 0 Pt | Sensor _ (Pos bx by) ← ss, by == y]

ranges ∷ Int → Sensor → [Box ('S 'Z)]
ranges y (Sensor (Pos sx sy) (Pos px py)) = [cover sx dx Pt | dx >= 0]
  where
    dy = abs (y - sy)
    dx = r - dy
    r = manhattan (C sy sx) (C py px)

subAllOf ∷ [Box n] → [Box n] → [Box n]
subAllOf xs ys = foldl' (\acc x → concatMap (subtractBox x) acc) ys xs

cover ∷ Int → Int → Box n → Box ('S n)
cover x r = Dim (x - r) (x + r + 1)

disjoint ∷ [Box a] → [Box a]
disjoint = foldr (\box rest → box : concatMap (subtractBox box) rest) []

main ∷ IO ()
main = pureMain $ \input → do
  ss ← P.runParser (parseSensor `P.sepBy` P.newline) input
  pure (pure (part1 ss), pure (part2 ss))
