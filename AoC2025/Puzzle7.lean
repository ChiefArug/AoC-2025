import AoC2025.Lib
import Lean
open Lib Option

def Nat.charred (n : Nat) : Char := match n with
  | 0 => '0'
  | 1 => '1'
  | 2 => '2'
  | 3 => '3'
  | 4 => '4'
  | 5 => '5'
  | 6 => '6'
  | 7 => '7'
  | 8 => '8'
  | 9 => '9'
  | o =>
    let o := o - 10
    if o < 25 then Char.ofNat ('a'.toNat + o)
    else
    let o := o - 26
    if o < 25 then Char.ofNat ('A'.toNat + o)
    else '*'


namespace Puzzle7

def puzzle7 : IO Unit := do
  let inFile ← (fileStream "inputs/7.txt")

  match inFile.map IO.FS.Stream.lines with
  | none => pure ()
  | some ioarr =>
    let lines ← ioarr
    let lines := lines.toList
    let entrance := lines[0]!
    let mut remaining := lines.drop 1
    let mut beamPresent := Vector.replicate entrance.length 0
    beamPresent := beamPresent.set! (entrance.find (· = 'S')).byteIdx 1
    let mut splits := 0


    while hr : !remaining.isEmpty do
      let line := remaining[0]!.toList -- cant be bothered proving this. tis true tho
      remaining := remaining.drop 1
      let mut nextBeams := beamPresent
      for hi : i in Vector.range beamPresent.size do
        have hi := inRange_ltMax hi
        if beamPresent[i] != 0 && line[i]! = '^' then do
          if h : i > 0 then do
            nextBeams := nextBeams.set (i - 1) (nextBeams[i - 1] + beamPresent[i])
          if h : i + 1 < nextBeams.size then do
            nextBeams := nextBeams.set (i + 1) (nextBeams[i + 1] + beamPresent[i])
          nextBeams := nextBeams.set i 0
          splits := splits + 1
      beamPresent := nextBeams
      IO.println (String.ofList (beamPresent.toList.map λ x => (if x > 0 then x.charred else '.')))
    let timelines := beamPresent.sum
    IO.println s!"splits {splits} timelines {timelines}"

-- p2 3097 too low
#eval puzzle7
end Puzzle7
