import AoC2025.Lib
import Lean
import Std
open Lib Option Std

namespace Puzzle11


def puzzle11 : IO Unit := do
  let inFile ← (fileStream "inputs/11.txt")

  match inFile.map IO.FS.Stream.lines with
  | none => pure ()
  | some ioarr =>
    let lines ← ioarr

    let mut map : HashMap String (List String) := HashMap.emptyWithCapacity 583

    for line in lines do
      let parts := line.splitToList (· = ':')
      let dev := parts[0]!
      let cons := parts[1]!.trim.splitToList (· = ' ')
      map := map.insert dev cons

    --IO.println map.toArray

    let rec walk (n : Nat) (dev : String) : Nat :=
      if dev = "out" then 1 else
      if n = 0 then panic "Recursed too deep!" else
      (map[dev]!.map (walk (n - 1))).sum

    IO.println (walk 1000 "you")

#eval puzzle11
end Puzzle11
