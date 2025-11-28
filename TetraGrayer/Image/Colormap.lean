/-
Spherical colormap with lat-long grid, matching Bohn et al. (2014) visualization.

Ported from tetra-gray's image.cuh (SphericalColormap).

Draws a latitude-longitude grid at the extraction sphere radius,
with four quadrants colored distinctly. Pixels that don't escape
are painted cyan (fell into black hole or hit max parameter).
-/

import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Clifford
import TetraGrayer.Core.CoordSystems
import TetraGrayer.Core.Particle
import TetraGrayer.Image.PPM

namespace TetraGrayer
namespace Image

open Core

/-- Convert RGB components to a single packed UInt32 for intermediate processing. -/
def rgbToSingle (r g b : Nat) : UInt32 :=
  (r.toUInt32 <<< 16) ||| (g.toUInt32 <<< 8) ||| b.toUInt32

/-- Convert packed UInt32 back to RGB. -/
def singleToRGB (c : UInt32) : RGB :=
  { r := ((c >>> 16) &&& 0xFF).toUInt8
  , g := ((c >>> 8) &&& 0xFF).toUInt8
  , b := (c &&& 0xFF).toUInt8 }

/-- Test if angle falls on a grid stripe.

Returns true if the angle is within stripe_half_width_ratio of
a multiple of stripe_interval.
-/
def testStripe (angle stripeInterval stripeHalfWidthRatio : ℝ) : Bool :=
  let stripeNumber := angle / stripeInterval
  let stripeRemainder := stripeNumber - Float.round stripeNumber
  Float.abs stripeRemainder <= stripeHalfWidthRatio

/-- Spherical colormap: maps escaped particle position to color.

Colors four quadrants based on final position:
- y > 0, z > 0: red
- y < 0, z > 0: green
- y > 0, z < 0: blue
- y < 0, z < 0: yellow

Black grid lines at 10° intervals.
Cyan for particles that didn't escape.
-/
def sphericalColormap (escapeRadius : ℝ) (data : ODEData Particle) : RGB :=
  let pos := data.value.position
  let spherical := sphericalFromCartesian pos

  -- Check if escaped
  if spherical.v1 >= escapeRadius then
    let theta := spherical.v2
    let phi := spherical.v3

    -- Grid parameters: 10° intervals, 0.05 half-width
    let stripeHalfWidth : ℝ := 0.05
    let stripeInterval : ℝ := π / 18.0  -- 10 degrees

    -- Check if on grid line
    if testStripe theta stripeInterval stripeHalfWidth ||
       testStripe phi stripeInterval stripeHalfWidth then
      RGB.black
    else
      -- Quadrant colors based on Cartesian y,z
      let y := pos.v2
      let z := pos.v3
      if y > 0.0 && z > 0.0 then RGB.red
      else if y < 0.0 && z > 0.0 then RGB.green
      else if y > 0.0 && z < 0.0 then RGB.blue
      else RGB.yellow
  else
    -- Didn't escape: cyan
    RGB.cyan

/-- Simpler colormap for flat spacetime testing (based on momentum direction). -/
def flatColormap (dir : CliffordVector) : RGB :=
  let x := dir.v1
  let y := dir.v2
  let z := dir.v3
  let r := Float.sqrt (x*x + y*y + z*z)
  let denom := if r < 1e-12 then 1.0 else r
  let nx := x / denom
  let ny := y / denom
  -- Simple grid using thresholds on normalized coords
  let grid := Float.abs nx > 0.95 || Float.abs ny > 0.95
  if grid then RGB.white
  else if nx >= 0.0 && ny >= 0.0 then RGB.ofNat 220 50 50
  else if nx < 0.0 && ny >= 0.0 then RGB.ofNat 50 220 50
  else if nx < 0.0 && ny < 0.0 then RGB.ofNat 50 50 220
  else RGB.ofNat 220 220 50

end Image
end TetraGrayer
