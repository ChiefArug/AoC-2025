import AoC2025.Lib
import Lean
open Lib Option


def Option.flatMap {α β : Type} (f : α → Option β) (o : Option α) : Option β := match o with
  | none => none
  | some e => f e

def Array.get {α : Type} [Inhabited α] (x y : Int) (board: Array (Array α)) : (Option α) := match x.toNat?, y.toNat? with
  | some x, some y => board[x]?.bind (·[y]?)
  | _, _ => none

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
    let board := lines.map (·.map (if · = '@' then paper else empty))
    let mut count := 0
    for x in Array.range board.size do
      let curRow := board[x]!
      for y in List.range curRow.size do
        let curSquare: Option Space := board.get x y

        if !(curSquare.isEqSome paper) then
          continue
        else
          let surrounds := [
            -- Collect the 8 surrounding members
            board.get (x-1) (y-1), board.get x (y-1), board.get (x+1) (y-1),
            board.get (x-1)  y                      , board.get (x+1)  y,
            board.get (x-1) (y+1), board.get x (y+1), board.get (x+1) (y+1),
          ]
          let surroundingPaper := surrounds.count (some Space.paper)
          --IO.println s!"{x},{y} => paper :{surroundingPaper}"
          -- Count the ones that are empty
          if surroundingPaper < 4 then
            count := count + 1


    IO.println s!"everything is {count}"



#eval puzzle4
end Puzzle4
