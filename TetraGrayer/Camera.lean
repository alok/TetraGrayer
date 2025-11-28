/-
Camera model producing per-pixel initial rays in flat spacetime.
-/

import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Vector

namespace TetraGrayer

open Core

structure Ray where
  pos : Vec4
  mom : Vec4

structure Camera where
  origin    : Vec4
  basisT    : Vec4  -- time direction
  basisOut  : Vec4  -- forward direction
  basisLeft : Vec4
  basisUp   : Vec4
  hFovRad   : ℝ
  width     : Nat
  height    : Nat

namespace Camera

def default (w h : Nat) : Camera :=
  { origin := vec4 0.0 0.0 0.0 0.0
  , basisT := vec4 1.0 0.0 0.0 0.0
  , basisOut := vec4 0.0 0.0 0.0 (-1.0)
  , basisLeft := vec4 0.0 (-1.0) 0.0 0.0
  , basisUp := vec4 0.0 0.0 1.0 0.0
  , hFovRad := deg2rad 60.0
  , width := w
  , height := h }

/-- Compute initial ray for pixel (x,y), 0-based with y downward. -/
def pixelRay (cam : Camera) (x y : Nat) : Ray :=
  let w := cam.width; let h := cam.height
  let fx : ℝ := (Float.ofNat x + 0.5) / Float.ofNat (Nat.max w 1)
  let fy : ℝ := (Float.ofNat y + 0.5) / Float.ofNat (Nat.max h 1)
  -- Approximate tan for small angles: tan(a) ≈ a + a^3/3 (sufficient for small HFOV)
  let a := cam.hFovRad / 2.0
  let tanA := a + (a*a*a) / 3.0
  let u : ℝ := (2.0 * fx - 1.0) * tanA
  let aspect : ℝ := Float.ofNat w / Float.ofNat (Nat.max h 1)
  let v : ℝ := (1.0 - 2.0 * fy) * (tanA / aspect)
  let dir := vadd cam.basisOut (vadd (vsmul u cam.basisLeft) (vsmul v cam.basisUp))
  { pos := cam.origin
  , mom := normalizeSpatial dir }

end Camera

end TetraGrayer
