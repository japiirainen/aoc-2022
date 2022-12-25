module Main where

import AOC.Main (simpleMain)
import AOC.V2 (V2 (..))

import qualified AOC.Grid as G
import qualified Data.Map as Map
import qualified Data.Set as Set

data Inst = Noop | Add Int
  deriving stock (Show)

parseInstruction ∷ String → Inst
parseInstruction s = case words s of
  ["noop"] → Noop
  ["addx", n] → Add (read n)
  _ → error "parseInstruction: bad input"

part1 ∷ [Inst] → Int
part1 =
  let go acc cyc x = \case
        [] → acc
        Noop : is → go (acc + sig cyc x) (cyc + 1) x is
        Add n : is → go (acc + sig cyc x + sig (cyc + 1) x) (cyc + 2) (x + n) is
      sig cyc x = if cyc `mod` 40 == 20 then cyc * x else 0
   in go 0 1 1

part2 ∷ [Inst] → Set.Set G.Pos
part2 =
  let go grid cyc sprite = \case
        [] → grid
        Noop : is → go (draw cyc sprite grid) (cyc + 1) sprite is
        Add n : is → go grid' (cyc + 2) (sprite + n) is
          where
            grid' = (draw (cyc + 1) sprite . draw cyc sprite $ grid)
      draw cyc sprite grid =
        if col < sprite - 1 || col > sprite + 1 then grid else Set.insert (V2 col row) grid
        where
          (row, col) = cyc `divMod` 40
   in go Set.empty 0 1

main ∷ IO ()
main = simpleMain $ \input →
  let instructions = map parseInstruction (lines input)
   in (part1 instructions, G.toString (Map.fromSet (const '#') $ part2 instructions))
