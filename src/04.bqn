Split ⇐ ((⊢-˜+`×¬)∘(≡¨)⊔⊢)
i←{•BQN¨¨'-'Split¨','Split𝕩}¨•Flines"src/04"
•Show+´{⟨a‿b,c‿d⟩:0≥(a-c)×(b-d)}¨i # part 1
•Show+´{⟨a‿b,c‿d⟩:0≥(a-d)×(b-c)}¨i # part 2
