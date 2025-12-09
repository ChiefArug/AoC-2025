namespace Lib

-- From https://lean-lang.org/functional_programming_in_lean/Hello___-World___/Worked-Example___--cat/
def fileStream (filename : System.FilePath) : IO (Option IO.FS.Stream) := do
  let fileExists ← filename.pathExists
  if not fileExists then
    let stderr ← IO.getStderr
    stderr.putStrLn s!"File not found: {filename}"
    pure none
  else
    let handle ← IO.FS.Handle.mk filename IO.FS.Mode.read
    pure (some (IO.FS.Stream.ofHandle handle))

def unwrapIO {α : Type} (list : List (IO α)): IO (List α) := do
  match list with
  | [] => pure []
  | e :: es =>
    let elem ← e
    let rest ← unwrapIO es
    pure (elem :: rest)


end Lib
