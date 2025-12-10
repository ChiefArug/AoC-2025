import AoC2025.Lib
import Lean
open Lib Option

private def takeWhileSomeHelper (xs : List (Option α)) (soFar : List α)  : List α := match xs with
  | (some x) :: xs => takeWhileSomeHelper xs (x :: soFar)
  | _ => soFar

def List.takeWhileSome (xs : List (Option α)) : List α := takeWhileSomeHelper xs []

-- Drops until the predicate returns true. Will also drop the item that returns true for the predicate
def List.dropTillIncl (p : α → Bool) : List α → List α
  | [] => []
  | a :: l => match p a with
    | true  => l
    | false => dropTillIncl p l



namespace Puzzle6

infixr:arg "🤐" => List.zip

inductive Op where
  | mult
  | add

instance : Inhabited Op where default := Op.add
instance : ToString Op where toString := (match · with |.mult => "*" |.add => "+")

def Op.default (op : Op) : Nat := match op with |mult => 1 |add => 0

def Op.apply (a b : Nat) (op : Op) : Nat := match op with |mult => a * b |add => a + b

def Op.applyAll (ns : List Nat) (op : Op) (soFar : Nat := op.default) : Nat :=
  match ns with
  | [] => soFar
  | n :: ns => op.applyAll ns (op.apply soFar n)

def Op.of! (c : Char) : Op :=
  match c with
  | '*' => mult
  | '+' => add
  | dunno => panic s!"unknown op {dunno}"

def compute := λ ((a, b, c, d, op) : Nat × Nat × Nat × Nat × Op ) => op.applyAll [a, b, c, d]

def puzzle6 : IO Unit := do
  let inFile ← (fileStream "inputs/6.txt")

  match inFile.map IO.FS.Stream.lines with
  | none => pure ()
  | some ioarr =>
    let lines ← ioarr

    let fst := lines[0]!.toList.reverse
    let snd := lines[1]!.toList.reverse
    let trd := lines[2]!.toList.reverse
    let frt := lines[3]!.toList.reverse
    let mut ops := ((lines[4]!.toList.filter (· != ' ')).map Op.of!).reverse

    let nums := (fst 🤐 snd 🤐 trd 🤐 frt)
    -- IO.println nums
    let nums := nums.map λ ((a, b, c, d) : Char × Char × Char × Char) => [a,b,c,d]
    let nums := nums.map String.ofList
    -- IO.println nums
    let nums := nums.map String.trim
    -- IO.println nums
    let nums := nums.map String.toNat?
    -- IO.println nums

    let rec reduce (ops : List Op) (nums : List (Option Nat)) (sum : Nat) : IO Nat := do
      let seg := nums.takeWhileSome
      match seg, ops with
        | seg, op :: ops =>
          -- IO.println s!"sum {sum} seg {seg} op {op}"
          let newList := nums.dropTillIncl Option.isNone
          let applied := op.applyAll seg
          -- IO.println s!"{op} of {seg} = {applied}"
          let newSum := sum + applied

          reduce ops newList newSum
        | _, _ =>
          IO.println s!"exiting with {sum}"
          pure sum

    let out ← reduce ops nums 0
    IO.println out


#eval puzzle6
end Puzzle6
