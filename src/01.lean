import Std
import «Aoc»

def parseInput (l: List String) : List (List Int) :=
 (l.splitOnP (· = "")).map (·.map String.toInt!)

def sol (toTake : Nat) : List (List Int) → Int :=
  (·.sum) ∘ (·.take toTake) ∘ (·.reverse) ∘ (·.sort) ∘ (·.map (·.sum))

def main : IO Unit := do
  let i ← (parseInput ∘ Array.toList) <$> IO.FS.lines "src/01"
  IO.println (sol 1 i) -- part 1
  IO.println (sol 3 i) -- part 2

#eval main