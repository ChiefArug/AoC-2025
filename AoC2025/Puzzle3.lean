import AoC2025.Lib
import Lean
open Lib Option

namespace Puzzle3


def shiftLeft {α : Type} (cur : Vector α 12) (new : α) : (Vector α 12) :=
  #v[cur[1], cur[2], cur[3], cur[4], cur[5], cur[6], cur[7], cur[8], cur[9], cur[10], cur[11], new]



def twelveMax (cur : Vector Char 12) (char : Char) : Vector Char 12 :=
  if cur[0] < cur[1] then   #v[cur[1], cur[2], cur[3], cur[4], cur[5], cur[6], cur[7], cur[8], cur[9], cur[10], cur[11], char] else
  if cur[1] < cur[2] then   #v[cur[0], cur[2], cur[3], cur[4], cur[5], cur[6], cur[7], cur[8], cur[9], cur[10], cur[11], char] else
  if cur[2] < cur[3] then   #v[cur[0], cur[1], cur[3], cur[4], cur[5], cur[6], cur[7], cur[8], cur[9], cur[10], cur[11], char] else
  if cur[3] < cur[4] then   #v[cur[0], cur[1], cur[2], cur[4], cur[5], cur[6], cur[7], cur[8], cur[9], cur[10], cur[11], char] else
  if cur[4] < cur[5] then   #v[cur[0], cur[1], cur[2], cur[3], cur[5], cur[6], cur[7], cur[8], cur[9], cur[10], cur[11], char] else
  if cur[5] < cur[6] then   #v[cur[0], cur[1], cur[2], cur[3], cur[4], cur[6], cur[7], cur[8], cur[9], cur[10], cur[11], char] else
  if cur[6] < cur[7] then   #v[cur[0], cur[1], cur[2], cur[3], cur[4], cur[5], cur[7], cur[8], cur[9], cur[10], cur[11], char] else
  if cur[7] < cur[8] then   #v[cur[0], cur[1], cur[2], cur[3], cur[4], cur[5], cur[6], cur[8], cur[9], cur[10], cur[11], char] else
  if cur[8] < cur[9] then   #v[cur[0], cur[1], cur[2], cur[3], cur[4], cur[5], cur[6], cur[7], cur[9], cur[10], cur[11], char] else
  if cur[9] < cur[10] then  #v[cur[0], cur[1], cur[2], cur[3], cur[4], cur[5], cur[6], cur[7], cur[8], cur[10], cur[11], char] else
  if cur[10] < cur[11] then #v[cur[0], cur[1], cur[2], cur[3], cur[4], cur[5], cur[6], cur[7], cur[8], cur[9],  cur[11], char] else
  if cur[11] < char then    #v[cur[0], cur[1], cur[2], cur[3], cur[4], cur[5], cur[6], cur[7], cur[8], cur[9],  cur[10], char] else
  cur


-- Returns the max two integers
def processLine (line : String) : IO (Vector Char 12) := do
  let max := line.foldl twelveMax (Vector.replicate 12 '\u0000')
  IO.println s!"Max is {max.toList} for string {line}"
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
    let lines := lines.map Vector.toList
    let lines := lines.map String.ofList
    let lines := lines.map String.toNat!
    let sum := lines.sum
    IO.println s!"Sum is {sum}"



#eval puzzle3
end Puzzle3
