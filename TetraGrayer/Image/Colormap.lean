import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Vector
import TetraGrayer.Image.PPM

/-! # Spherical-like test colormap

Quadrant coloring and a simple grid approximation (no trig) to visualize ray directions.
-/

namespace TetraGrayer
namespace Image

open Core

/-- Map a 4D direction (uses spatial part) to an RGB color. -/
def sphericalColor (dir : Vec4) : RGB :=
  let x := dir.get ⟨1, by decide⟩
  let y := dir.get ⟨2, by decide⟩
  let z := dir.get ⟨3, by decide⟩
  let r := Float.sqrt (x*x + y*y + z*z)
  let denom := if r < 1e-12 then 1.0 else r
  let nx := x / denom
  let ny := y / denom
  -- Simple grid using thresholds on normalized coords (avoids trig).
  let grid := (Float.abs nx > 0.95) || (Float.abs ny > 0.95)
  let quad := if nx ≥ 0.0 && ny ≥ 0.0 then (220, 50, 50)
              else if nx < 0.0 && ny ≥ 0.0 then (50, 220, 50)
              else if nx < 0.0 && ny < 0.0 then (50, 50, 220)
              else (220, 220, 50)
  let (r8,g8,b8) := if grid then (255,255,255) else quad
  { r := r8, g := g8, b := b8 }

end Image
end TetraGrayer
