import Std

def sol : IO Unit := do
  let ip ← IO.FS.readFile "src/06"
  IO.println ip

def ex1 := "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

def startOfPacket (s: String) : Nat :=
  let rec go (acc : Nat) (s : String) : Nat :=
    match s.toList with
    | a :: b :: c :: d :: rest => 
      if a == b && b == c && c == d then acc + 4
      else go (acc + 4) rest.toString
    | _ => acc
    -- | _ => go (acc + 1) (s.drop 1)
  go 0 s

-- #eval startOfPacket ex1