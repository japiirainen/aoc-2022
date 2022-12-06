import Std

def parseLine (xs : List Char) : Option (Int × Int × Int × Int) :=
  match (xs.splitOnP (λ x => x = ',' ∨ x = '-')).map (String.toInt! ∘ String.mk) with
  | [i1, i2, i3, i4] => some (i1, i2, i3, i4)
  | _ => none
   
def part₁ (xs : List (Int × Int × Int × Int)) : Int :=
  xs.countp (λ (a, b, c, d) => 0≥(a-c)*(b-d))

def part₂ (xs : List (Int × Int × Int × Int)) : Int :=
  xs.countp (λ (a, b, c, d) => 0≥(b-c)*(a-d))
   
def sol : IO Unit := do
  let i ← IO.FS.lines "input/04"
  let parsed := Array.toList <$> i.sequenceMap (parseLine ∘ String.toList)
  _ ← parsed.mapM (λ xs => IO.println (part₁ xs, part₂ xs)) 