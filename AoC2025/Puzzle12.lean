import AoC2025.Lib
import Lean
import Std
open Lib Option Std

def Fin.toUInt64 (f : Fin n) : UInt64 := f.toNat.toUInt64

namespace Puzzle12


def lineToUint (s : String) : UInt64 :=
  let chars := s.toList
  if chars[0]! = '#' then 1 else 0 |||
  if chars[1]! = '#' then 2 else 0 |||
  if chars[2]! = '#' then 4 else 0


def uInt64ToString (lim : UInt64) (u : UInt64) : String :=
  if u = 0 then String.ofList (List.replicate lim.toNat '.') else
  if u.toNat = (UInt64.size - 1) then String.ofList (List.replicate lim.toNat '#') else
  String.ofList ((0 ... lim).toList.map (if u &&& (1 : UInt64) <<< · != 0 then '#' else '.'))

structure Shape where
  lines : Vector UInt64 3
  index : Fin 5

instance : ToString Shape where toString := λ s => (s.lines.map ("\t" ++ uInt64ToString 3 · ++ "\n")).foldl String.append "\n"

def Shape.parse (l : List String) : Shape :={
  lines := #v[lineToUint l[1]!, lineToUint l[2]!, lineToUint l[3]!],
  index := Fin.ofNat 5 (l[0]!.take 1).toNat!
}

structure Tree where
  x : Fin 64
  y : Fin 64
  lines : Vector UInt64 y
  requirements : Vector Nat 6

instance : ToString Tree where toString := λ t =>
  let header :=s!"{t.x}x{t.y}: {t.requirements.toArray}"
  let rest := t.lines.map (uInt64ToString t.x.toUInt64 · ++ "\n")
  header ++ "\n" ++ (rest.foldl String.append "")
instance : GetElem Tree (Fin 64 × Fin 64) Bool (λ t i => i.fst < t.y ∧ i.snd < t.x ) where getElem := λ t i h => t.lines[i.fst] >>> i.snd.toUInt64 &&& 1 == 1
-- we have to split this with an 'if' statement as a.lines == b.lines depends on a.y = b.y
instance : BEq Tree where beq := λ a b => if h : a.x = b.x ∧ a.y = b.y then a.lines == b.lines.cast (by omega) ∧ a.requirements = b.requirements else false
instance : Hashable Tree where hash := λ t => hash #[hash t.x, hash t.y, hash t.lines.toArray, hash t.requirements.toArray]

def Tree.of (x y : Fin 64) (req : Vector Nat 6) : Tree := {x := x, y := y, lines := Vector.replicate y 0, requirements := req}


def loop (t : Tree) : StateT (HashMap (Tree) Nat) Id Nat := do
  sorry


def puzzle12 : IO Unit := do
  let ioshapes := (← (fileStream "inputs/12atest.txt")).map IO.FS.Stream.readToEnd
  let iotrees := (← (fileStream "inputs/12btest.txt")).map IO.FS.Stream.lines

  match ioshapes, iotrees with
  | none, _ => return
  | _, none => return
  | some ioshapes, some iotrees =>
    let shapes ← ioshapes
    let shapes := (shapes.splitOn "\n\n").toArray
    let shapes := shapes.map (·.splitToList (· = '\n'))
    let shapes := shapes.map Shape.parse

    let trees ← iotrees
    let trees := trees.map λ s => s.splitOn ": "
    let trees := trees.map λ ss => (
        (ss[0]!.splitToList (· = 'x')).map String.toNat!,
        (ss[1]!.splitToList (· = ' ')).map String.toNat!
    )

    let trees := trees.map λ nn => Tree.of (Fin.ofNat _ nn.fst[0]!) (Fin.ofNat _ nn.fst[1]!) (
      let t := nn.snd.toArray.toVector
      if h : t.size = 6 then t.cast h else panic "not correct length vector!"
    )
    if h : 0 < trees.size then
      let trees := trees.set 0 {trees[0] with lines := (trees[0].lines.set! 2 0b01)} -- there was a coercion in the thing required to prove so had to use !
    IO.println trees[0]?
    IO.println shapes
    --IO.println trees
#eval timeit "12 took: " puzzle12
end Puzzle12
