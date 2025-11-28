/-
Minimal ASCII PPM (P3) writer for fast CPU-only iteration.
-/

import TetraGrayer.Core.Scalar

namespace TetraGrayer
namespace Image

open Core

structure RGB where
  r : Nat
  g : Nat
  b : Nat
deriving Repr

@[inline] def clamp255 (x : Nat) : Nat := if x > 255 then 255 else x

def writePPM (path : System.FilePath) (w h : Nat) (pix : Nat → Nat → RGB) : IO Unit := do
  -- Ensure parent dir exists.
  let parent := path.parent
  match parent with
  | some dir => do IO.FS.createDirAll dir
  | none => pure ()
  IO.FS.withFile path .write fun handle => do
    handle.putStr s!"P3\n{w} {h}\n255\n"
    for y in [0:h] do
      let mut line := ""
      for x in [0:w] do
        let c := pix x y
        line := line ++ s!"{clamp255 c.r} {clamp255 c.g} {clamp255 c.b} "
      handle.putStr (line ++ "\n")

end Image
end TetraGrayer
