import AoC2025.Lib
import Lean
open Lib Option

namespace Puzzle2

structure Range where
  first : Nat
  last : Nat

structure NatWithLength where
  n : Nat
  length : Nat

def Range.parse (s : String) : Range :=
  let split := s.splitToList (· = '-')
  {first := String.toNat! split[0]!, last := String.toNat! split[1]!}

def Range.expandHelper (soFar : List Nat) (r : Range) : List Nat :=
  if r.first >= r.last then
    r.first :: soFar
  else
    { r with first := r.first + 1}.expandHelper (r.first :: soFar)

def Range.expand (r : Range): List Nat :=
  Range.expandHelper [] r

-- this is insane
def natlog10Floor (n : Nat): Nat := (Float.ofNat n).log10.toInt64.toInt.natAbs

def List.unwrapIO {α : Type} (list : List (IO α)): IO (List α) := do
  match list with
  | [] => pure []
  | e :: es =>
    let elem ← e
    let rest ← unwrapIO es
    pure (elem :: rest)

def reduce (ranges : List Range) (previousSum : Nat): IO Nat := do
  match ranges with
  | [] => pure previousSum
  | range :: rest =>
    IO.println (Nat.repr range.first ++ " to " ++ Nat.repr range.last)
    let onlyMistakes := range.expand.filter (λ x =>
      -- example with {n := 514514, length := 6}
      let length := natlog10Floor x

      -- segmentSize := 3
      let segmentSize := length / 2
      -- segmentShifter := 1000
      let segmentShifter := 10 ^ segmentSize
      -- lastHalf := 514
      let lastHalf := x % segmentShifter
      -- firstHalf := 514000
      let firstHalf := lastHalf * segmentShifter
      -- expected := 514514
      let expected := firstHalf + lastHalf
      -- 514514 = 514514
      x = expected
    )
    let currentSum := onlyMistakes.sum
    let totalSoFar := previousSum + currentSum

    reduce rest totalSoFar


def puzzle2 : IO Unit := do
  let file ← (fileStream "inputs/2.txt")
  match file.map IO.FS.Stream.getLine with
  | none => pure ()
  | some ioline =>
    let line ← ioline
    let entries := line.splitToList (· = ',')
    IO.print "Entries "
    IO.println entries
    let ranges := entries.map Range.parse
    IO.print "Ranges "
    IO.println (ranges.map (λ r => Nat.repr r.first ++ " to " ++ Nat.repr r.last))
    let sum ← reduce ranges 0
    IO.print "Sum "
    IO.println sum
    -- let expanded := ranges.flatMap Range.expand
    -- IO.println (Nat.repr expanded[0]!)




#eval puzzle2
end Puzzle2
