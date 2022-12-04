import Std
open Std

def update [Add α] [OfNat α 0] (m : RBMap β α cmp) (k : β) (v : α) : RBMap β α cmp :=
  match m.find? k with
  | none => m.insert k v
  | some v' => m.insert k (v + v') 

def List.sort [Ord α] (l : List α) : List α := 
  let x : RBMap α Nat compare := l.foldr (λ x m => update m x 1) RBMap.empty
  x.keys

def List.sum : List Int → Int := List.foldl (· + ·) 0
