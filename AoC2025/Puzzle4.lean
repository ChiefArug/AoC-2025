import AoC2025.Lib
import Lean
open Lib Option

def Vector.get2d {α : Type} {w h : Nat} [Inhabited α]  (x y : Int) (board: Vector (Vector α w) h) : (Option α) := match x.toNat?, y.toNat? with
  | some x, some y => board[x]?.bind (·[y]?)
  | _, _ => none

def Array.padToVector {α : Type} [inst : Inhabited α] {expcLen : Nat} (arr : Array α) : Vector α expcLen :=
  if h : (arr.size = expcLen) then arr.toVector.cast h
  else if h : arr.size < expcLen then (arr.toVector ++ Vector.replicate (expcLen - arr.size) inst.default).cast (by omega)
  -- arr.size > expcLen
  else (arr.toVector.shrink expcLen).cast (by omega)

namespace Puzzle4

inductive Space where
  | paper
  | empty
deriving Repr
open Space

instance : BEq Space where beq := λ a b => match a, b with | paper, paper => true | empty, empty => true | _, _ => false
instance : Inhabited Space where default := empty
def Space.isPaper := (match · with | paper => true | empty => false)

instance : ToString Space where toString := (λ x : Space => if x.isPaper then "paper" else "empty")

def puzzle4 : IO Unit := do
  let file ← (fileStream "inputs/4.txt")
  match file.map IO.FS.Stream.lines with
  | none => pure ()
  | some ioarr =>
    let lines ← ioarr
    let lines := lines.map (·.toList.toArray)
    let aboard := lines.map (·.map (if · = '@' then paper else empty))
    let size := aboard.size -- its square, abuse that fact
    let mut board : Vector (Vector Space size) size := (aboard.map (·.padToVector)).padToVector
    let mut count := 0
    let mut prevCount := count
    while true do
      for xp : x in Vector.range size do
        -- proof that x is an index in board
        have xp := inRange_ltMax xp
        for yp : y in Vector.range size do
          -- proof that y is an index in board
          have yp := inRange_ltMax yp

          let curSquare: Option Space := board.get2d x y
          if !(curSquare.isEqSome paper) then
            continue
          else
            let surrounds := [
              -- Collect the 8 surrounding members
              board.get2d (x-1) (y-1), board.get2d x (y-1), board.get2d (x+1) (y-1),
              board.get2d (x-1)  y                        , board.get2d (x+1)  y,
              board.get2d (x-1) (y+1), board.get2d x (y+1), board.get2d (x+1) (y+1),
            ]
            let surroundingPaper := surrounds.count (some Space.paper)
            --IO.println s!"{x},{y} => paper :{surroundingPaper}"
            -- Count the ones that are empty
            if surroundingPaper < 4 then do
              count := count + 1
              -- we are able to modify it in place because changing it now doesnt change the end result
              let newRow := (board[x]).set y empty
              board := board.set x newRow
              --IO.println s!"changed {x} {y}"
      IO.println s!"everything is {count}. count {count} prevCount {prevCount}"
      if count = prevCount then
        break
      else
        prevCount := count





#eval puzzle4
end Puzzle4
