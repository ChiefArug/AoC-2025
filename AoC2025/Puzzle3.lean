import AoC2025.Lib
import Lean
open Lib Option

namespace Puzzle3

def twoMax (cur : Char × Char) (char : Char) : Char × Char :=
      if cur.fst < cur.snd then (cur.snd, char) else
      if char < cur.snd then cur else
      (if cur.snd > cur.fst then cur.snd else cur.fst, char)

-- Returns the max two integers
def processLine (line : String) : IO (Char × Char) := do
  let max := line.foldl twoMax ('\u0000', '\u0000')
  IO.println s!"Max is {max} for string {line}"
  pure max

def puzzle3 : IO Unit := do
  let file ← (fileStream "inputs/3.txt")
  match file.map IO.FS.Stream.lines with
  | none => pure ()
  | some ioarr =>
    let lines ← ioarr
    let lines := Array.toList lines
    let lines := lines.map processLine
    let lines ← unwrapIO lines
    let lines := lines.map listFromProd
    let lines := lines.map String.ofList
    let lines := lines.map String.toNat!
    let sum := lines.sum
    IO.println s!"Sum is {sum}"



×

#eval puzzle3
end Puzzle3
