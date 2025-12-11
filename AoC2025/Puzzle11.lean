import AoC2025.Lib
import Lean
import Std
open Lib Option Std

namespace Puzzle11

abbrev MemMap := HashMap (Bool × Bool × String) Nat


partial def loop (map : HashMap String (List String)) (encDac encFft : Bool) (s : String) : StateT (HashMap (Bool × Bool × String) Nat) Id Nat := do match s with
    | "out" => pure (if encDac && encFft then 1 else 0)
    | dev => do
      let memo ← StateT.get
      let key := (encDac, encFft, dev)
      match memo[key]? with
      | some result => pure result
      | none => do
        let mapFunc := loop map (encDac || dev = "dac") (encFft || dev == "fft")
        let result : Nat := (← map[dev]!.mapM mapFunc).sum
        modify fun memo => memo.insert key result
        pure result

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

    let t ← (loop map false false "svr" {})
    IO.println t.fst

#time #eval puzzle11
end Puzzle11
