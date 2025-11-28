/-
Minimal ASCII PPM (P3) writer for fast CPU-only iteration.
-/

import TetraGrayer.Core.Scalar

namespace TetraGrayer
namespace Image

open Core

/-- RGB color with UInt8 components (0-255). -/
structure RGB where
  r : UInt8
  g : UInt8
  b : UInt8
deriving Repr, Inhabited, BEq

namespace RGB

/-- Create RGB from Nat values, clamping to 0-255. -/
@[inline] def ofNat (r g b : Nat) : RGB :=
  ⟨r.toUInt8, g.toUInt8, b.toUInt8⟩

/-- Create RGB from Float values in range 0 to 1, scaling to 0-255. -/
@[inline] def ofFloat (r g b : Float) : RGB :=
  let clamp01 := fun x => if x < 0 then 0 else if x > 1 then 1 else x
  let toU8 := fun x => (clamp01 x * 255).toUInt8
  ⟨toU8 r, toU8 g, toU8 b⟩

/-- Black. -/
def black : RGB := ⟨0, 0, 0⟩

/-- White. -/
def white : RGB := ⟨255, 255, 255⟩

/-- Red. -/
def red : RGB := ⟨255, 0, 0⟩

/-- Green. -/
def green : RGB := ⟨0, 255, 0⟩

/-- Blue. -/
def blue : RGB := ⟨0, 0, 255⟩

/-- Yellow. -/
def yellow : RGB := ⟨255, 255, 0⟩

/-- Cyan. -/
def cyan : RGB := ⟨0, 255, 255⟩

/-- PPM format string: "r g b". -/
instance : ToString RGB where
  toString c := s!"{c.r.toNat} {c.g.toNat} {c.b.toNat}"

end RGB

/-- Clamp Nat to 255 (for legacy compatibility). -/
@[inline] def clamp255 (x : Nat) : Nat := if x > 255 then 255 else x

def writePPM (path : System.FilePath) (w h : Nat) (pix : Nat → Nat → RGB) : IO Unit := do
  let parent := path.parent
  match parent with
  | some dir => IO.FS.createDirAll dir
  | none => pure ()
  IO.FS.withFile path .write fun handle => do
    handle.putStr s!"P3\n{w} {h}\n255\n"
    for y in [0:h] do
      let mut line := ""
      for x in [0:w] do
        line := line ++ toString (pix x y) ++ " "
      handle.putStr (line ++ "\n")

end Image
end TetraGrayer
