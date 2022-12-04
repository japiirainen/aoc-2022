import Std

def parseLine (xs : List Char) : (Int × Int × Int × Int) :=
  let is := (xs.splitOnP (λ x => x = ',' ∨ x = '-')).map (String.toInt! ∘ String.mk)
  (is.get! 0, is.get! 1, is.get! 2, is.get! 3)
   
def part1 (xs : List (Int × Int × Int × Int)) : Int :=
  xs.countp (λ ((a, b, c, d) : Int × Int × Int × Int) => 0≥(a-c)*(b-d))

def part2 (xs : List (Int × Int × Int × Int)) : Int :=
  xs.countp (λ ((a, b, c, d) : Int × Int × Int × Int) => 0≥(b-c)*(a-d))
   
def main : IO Unit := do
  let i ← IO.FS.lines "src/04"
  let parsed := i.map (parseLine ∘ String.toList)
  IO.println (part1 (Array.toList parsed))
  IO.println (part2 (Array.toList parsed))

#eval main