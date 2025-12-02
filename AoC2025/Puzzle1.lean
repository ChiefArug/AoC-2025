import AoC2025.Lib
import Lean
open Lib Option

namespace Puzzle1

inductive TurnDirection where
  | left
  | right
open TurnDirection

abbrev DialPos : Type := Fin 100 -- { n : Nat // n < 100 }
def DialPos.initial : DialPos := 50

open DialPos

def DialPos.ofNat (n : Nat) : DialPos := Fin.ofNat _ n

def DialPos.add (n : Nat) (m : Nat) : DialPos := ofNat (n + m)
def DialPos.reverse (n : Nat) : DialPos := ofNat (100 - n)
def DialPos.inDirection (n : Nat) (dir : TurnDirection) : DialPos := (match dir with
  | TurnDirection.left => reverse n
  | TurnDirection.right => ofNat n
)

structure Turn where
  dir : TurnDirection
  count : DialPos

infix:arg "in" => inDirection
infix:arg "turn" => add


structure DialState where
  dial : DialPos
  zeroes : Nat

def reduce (turns : List Turn) (safe : DialState): IO DialState := do
      match turns with
      | [] => pure safe
      | step :: steps =>
        let turnedDial := safe.dial turn (step.count in step.dir)
        -- IO.println ("After turn: " ++ (Nat.repr turnedDial))
        let isNowZero := if turnedDial = 0 then 1 else 0
        reduce steps {dial := turnedDial, zeroes := safe.zeroes + isNowZero}


def puzzle1 : IO Unit := do
  let file ← fileStream "inputs/1.txt"
  let out ← IO.getStdout
  match file with
  | none => pure ()
  | some s =>
    let lines ← s.lines
    let mapper (s : String) : Turn :={
      dir := if (String.startValidPos s).get! = 'L' then left else right,
      count := ofNat (String.toNat! (s.toSlice.drop 1).copy)
    }

    let turns := (lines.map mapper).toList


    let finalDial ← reduce turns {dial := initial, zeroes := 0}
    out.putStrLn (Nat.repr finalDial.zeroes)
#eval puzzle1
end Puzzle1
