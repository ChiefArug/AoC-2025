import AoC2025.Lib
import Lean
open Lib Option

namespace Puzzle6

infixr:arg "🤐" => List.zip

inductive Op where
  | mult
  | add

instance : Inhabited Op where default := Op.add

def Op.apply (a b c d : Nat) (op : Op): Nat :=
  match op with
  | mult => a * b * c * d
  | add => a + b + c + d

def Op.of! (c : Char) : Op :=
  match c with
  | '*' => mult
  | '+' => add
  | dunno => panic s!"unknown op {dunno}"

def compute (inp : Nat × Nat × Nat × Nat × Op) : Nat := inp.snd.snd.snd.snd.apply inp.fst inp.snd.fst inp.snd.snd.fst inp.snd.snd.snd.fst

def puzzle6 : IO Unit := do
  let inFile ← (fileStream "inputs/6.txt")

  match inFile.map IO.FS.Stream.lines with
  | none => pure ()
  | some ioarr =>
    let lines ← ioarr
    let fst := ((lines[0]!.splitToList (· = ' ')).filter (!·.isEmpty)).map String.toNat!
    let snd := ((lines[1]!.splitToList (· = ' ')).filter (!·.isEmpty)).map String.toNat!
    let trd := ((lines[2]!.splitToList (· = ' ')).filter (!·.isEmpty)).map String.toNat!
    let frt := ((lines[3]!.splitToList (· = ' ')).filter (!·.isEmpty)).map String.toNat!
    let ops := (lines[4]!.toList.filter (· != ' ')).map Op.of!

    let together := fst 🤐 snd 🤐 trd 🤐 frt 🤐 ops
    let equated := together.map compute
    IO.println equated.sum



#eval puzzle6
end Puzzle6
