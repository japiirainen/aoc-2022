import Std

def solution (s : String) (n : Nat) : Nat :=
  (· + n) $ s.toList.tails.findIdx
    ((· = n) ∘ (·.toSSet.size) ∘ (·.map (·.toNat)) ∘ (·.take n))

#eval do IO.println (solution (← IO.FS.readFile "src/06") 4)
#eval do IO.println (solution (← IO.FS.readFile "src/06") 14)