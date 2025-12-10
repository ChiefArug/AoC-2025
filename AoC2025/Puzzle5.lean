import AoC2025.Lib
import Lean
open Lib Option

namespace Puzzle5

structure Range where
  min : Nat
  max : Nat

open Range

def Range.parse (str : String) : Range :=
  let l := str.splitToList (· = '-')
  {min := String.toNat! l[0]!, max := String.toNat! l[1]!}

def Range.contains (n : Nat) (r : Range) : Bool := n >= r.min && n <= r.max

instance : ToString Range where toString := λ r => s!"{r.min} to {r.max}"

def puzzle5 : IO Unit := do
  let ranges ← (fileStream "inputs/5a.txt")
  let numbers ← (fileStream "inputs/5b.txt")

  match ranges.map IO.FS.Stream.lines, numbers.map IO.FS.Stream.lines with
  | none , _ => pure ()
  | _ , none => pure ()
  | some ioranges, some ionumbers =>
    let rangeLines ← ioranges
    let numberLines ← ionumbers
    let ranges := rangeLines.map Range.parse
    let numbers := numberLines.map String.toNat!

    let filtered := numbers.filter λ n => ranges.any (contains n)
    IO.println filtered
    IO.println filtered.size




#eval puzzle5
end Puzzle5
