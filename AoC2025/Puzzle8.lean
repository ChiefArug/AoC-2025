import AoC2025.Lib
import Lean
import Std
open Lib Option Std

namespace Puzzle8

structure Pos where
  x : Nat
  y : Nat
  z : Nat

structure PosPair where
  a : Pos
  b : Pos
  dist : Nat


postfix:arg "²" => λ x => x * x
postfix:arg "~" => λ x : Nat => (x : Int)

instance : BEq Pos where beq := λ a b => a.x = b.x && a.y = b.y && a.z = b.z
instance : Inhabited Pos where default := {x := 0, y:= 0, z:= 0}
instance : ToString Pos where toString := λ p => s!"{p.x},{p.y},{p.z}"
instance : Hashable Pos where hash := λ p => hash [p.x, p.y, p.z]

instance : ToString PosPair where toString := λ pp => s!"{pp.a} ·[{pp.dist}]· {pp.b}"
instance : Min PosPair where min := λ a b => if a.dist < b.dist then a else b
instance : BEq PosPair where beq := λ a b => a.a == b.a && a.b == b.b


def Pos.distSqrd (a b : Pos) : Nat :=
  ((a.x~ - b.x~)² + (a.y~ - b.y~)² + (a.z~ - b.z~)²).natAbs
infix:arg "⋯" => Pos.distSqrd

def PosPair.of (a b : Pos) : PosPair := {a := a, b := b, dist := a ⋯ b}
infix:arg "⋄" => PosPair.of

def PosPair.toProd (p : PosPair) : Pos × Pos := (p.a, p.b)

def Pos.parse! (s : String) : Pos :=
  let split := s.splitToList (· = ',')
  {x := split[0]!.toNat!, y := split[1]!.toNat!, z := split[2]!.toNat!}

def appendOrNew (a : α) (o : Option (Array α)) : Array α := match o with
  | none => #[a]
  | some as => as.push a

def appendOrNewO (a : α)  (o : Option (Array α)) : Option (Array α) := appendOrNew a o

def puzzle8 : IO Unit := do
  let inFile ← (fileStream "inputs/8test.txt")

  match inFile.map IO.FS.Stream.lines with
  | none => pure ()
  | some ioarr =>
    let lines ← ioarr
    let mut poss := lines.map Pos.parse!

    -- let mut results : Array PosPair := #[]

    -- its very picky about the locations of these
    -- if h : 1 >= poss.size then panic "results is empty?" else
    -- -- initial fill of the array
    -- for pos in poss do
    --   let mut current := if pos == poss[0] then poss[1] else poss[0]
    --   for next in poss do
    --     if pos == next then continue
    --     if pos ⋯ next < pos ⋯ current then
    --       current := next
    --   results := results.push (pos ⋄ current)
    -- results := results.qsort (·.dist < ·.dist)
    -- IO.println results.size
    -- IO.println (results.map (ToString.toString · ++ "\n"))

    let working := poss
    let manyPoss := working.flatMap λ p1 => (working.map (· ⋄ p1)).filter (·.dist > 0)
    let manyPoss := manyPoss.qsort (·.dist < ·.dist)
    --IO.println s!"manyPoss: {manyPoss.map (ToString.toString · ++ "\n")}"
    let lessPoss := manyPoss.take 20
    let lessPoss := lessPoss.zipIdx
    let lessPoss := lessPoss.filter (·.snd % 2 = 0)
    IO.println s!"lessPoss: {lessPoss.map (ToString.toString · ++ "\n")}"
    -- IO.println fewPos.snd
    --IO.println s!"fst remove snd: {(fewPos.fst.toList.removeAll fewPos.snd.toList).length}"
    --IO.println s!"snd remove fst: {(fewPos.snd.toList.removeAll fewPos.fst.toList).length}"
    let lessPoss := lessPoss.map (·.fst)
    --IO.println fewPos

    let mut networks : Array (Array PosPair) := Array.emptyWithCapacity 400
    let mut keys : HashMap Pos Nat := HashMap.emptyWithCapacity 100

    for posPair in lessPoss do
      if h : keys.contains posPair.a && keys.contains posPair.b then
        IO.println s!"edge case {posPair}"
        -- pick arbitrary key
        let keya := keys[posPair.a]'(by grind)
        let keyb := keys[posPair.a]'(by grind)
        let new := (networks[keyb]?.getD #[])
        networks := networks.modify keya (· ++ new)
        let newFm := new.flatMap λ pp => #[pp.a, pp.b]
        let newZ := newFm.zip (Array.replicate newFm.size keya)
        keys := keys.insertMany newZ
      else if h : keys.contains posPair.a then
        let key := keys[posPair.a]
        networks := networks.modify key (·.push posPair)
        keys := keys.insert posPair.a key
        keys := keys.insert posPair.b key
      else if h : keys.contains posPair.b then
        let key := keys[posPair.b]
        networks := networks.modify key (·.push posPair)
        keys := keys.insert posPair.a key
        keys := keys.insert posPair.b key
      else -- new network to be made
        let key := networks.size
        networks := networks.push #[posPair]
        keys := keys.insert posPair.a key
        keys := keys.insert posPair.b key

    IO.println s!"networks: {(networks.zipIdx.map Prod.swap).map (ToString.toString · ++ "\n")}"
    let t := networks.qsort (·.size > ·.size)
    IO.println s!"nKeys: {keys.toArray.map (ToString.toString · ++ "\n")}"
    IO.println t.size
    let t := t.take 3
    IO.println t
    let t := t.map (·.size)
    IO.println s!"answer is {t} = {t.foldl (· * ·) 1}"


-- p1 180 too low
-- p1 4290 too low
-- p1 43008 too low
#eval puzzle8
end Puzzle8
