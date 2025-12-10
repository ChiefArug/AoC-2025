import AoC2025.Lib
import Lean
open Lib Option

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
    let mut beamPresent := Vector.replicate entrance.length false
    beamPresent := beamPresent.set! (entrance.find (· = 'S')).byteIdx true
    let mut splits := 0
    let mut timelines := 0


    while hr : !remaining.isEmpty do
      let line := remaining[0]!.toList -- cant be bothered proving this. tis true tho
      remaining := remaining.drop 1
      let mut nextBeams := beamPresent
      for hi : i in Vector.range beamPresent.size do
        have hi := inRange_ltMax hi
        if beamPresent[i] && line[i]! = '^' then do
          if h : i > 0 then do
            if beamPresent[i - 1] || nextBeams[i - 1] then timelines := timelines - 1
            nextBeams := nextBeams.set (i - 1) true
          if h : i + 1 < nextBeams.size then do
            if nextBeams[i + 1] || nextBeams[i + 1] then timelines := timelines - 1
            nextBeams := nextBeams.set (i + 1) true
          nextBeams := nextBeams.set i false
          splits := splits + 1
          timelines := timelines + 1
      beamPresent := nextBeams
      IO.println (String.ofList (beamPresent.toList.map (if · then '|' else '.')))
    IO.println s!"splits {splits} timelines {timelines}"

-- p2 3097 too low
#eval puzzle7
end Puzzle7
