import Lake
open Lake DSL

package aoc

@[default_target]
lean_lib Aoc where
  moreLeanArgs := #["-DwarningAsError=true"]

require std from git "https://github.com/leanprover/std4"