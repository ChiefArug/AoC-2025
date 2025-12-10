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
  {min := String.toNat! l[0]!, max := String.toNat! l[1]!
  -- this is a hack to solve a compounding off by one error caused by assuming incl/excl icnorrectly that i cant be bothered to solve
  + 1}

def Range.contains (n : Nat) (r : Range) : Bool := n >= r.min && n <= r.max

def Range.expand (r : Range) : List Nat := List.range' r.min (r.max - r.min)

instance : ToString Range where toString := λ r => s!"{r.min} to {r.max}"

def puzzle5 : IO Unit := do
  let ranges ← (fileStream "inputs/5a.txt")
  -- let numbers ← (fileStream "inputs/5b.txt")

  match ranges.map IO.FS.Stream.lines with
  | none => pure ()
  | some ioranges =>
    let rangeLines ← ioranges
    -- let numberLines ← ionumbers
    let ranges := rangeLines.map Range.parse
    -- let numbers := numberLines.map String.toNat!

    let sorted := ranges.insertionSort λ l r => if l.min = r.min then l.max < r.max else l.min < r.min
    let rec reducer (input : List Range) (cur prevMax : Nat) : Nat :=
      match input with
      | [] => cur
      -- because we use Nat we don't have to deal with evil negative numbers!
      | r :: rs =>
        let trueMin := Nat.max r.min prevMax
        let trueMax := Nat.max r.max prevMax
        reducer rs (cur + (r.max - trueMin)) trueMax

    let out := reducer sorted.toList 0 0
    IO.println out




#eval puzzle5
end Puzzle5
