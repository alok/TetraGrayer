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

/-- Checkerboard pattern on celestial sphere.

Creates a checkerboard of alternating colors based on latitude/longitude.
Number of checks controlled by frequency parameter.

Gravitational lensing distorts the regular pattern dramatically
near black holes, making it very educational for visualizing spacetime curvature.
-/
def checkerboardColormap (escapeRadius : ℝ) (frequency : ℝ := 8.0)
    (color1 : RGB := RGB.white) (color2 : RGB := RGB.ofNat 30 30 30)
    (data : ODEData Particle) : RGB :=
  let pos := data.value.position
  let spherical := sphericalFromCartesian pos

  if spherical.v1 >= escapeRadius then
    let theta := spherical.v2
    let phi := spherical.v3

    -- Checkerboard based on theta and phi
    let thetaIndex := Float.floor (theta * frequency / π)
    let phiIndex := Float.floor (phi * frequency / π)

    -- XOR pattern for checkerboard
    let isEven := (thetaIndex.toUInt64 + phiIndex.toUInt64) % 2 == 0
    if isEven then color1 else color2
  else
    -- Didn't escape: black (fell into black hole)
    RGB.black

/-- Einstein ring pattern - concentric rings for visualizing lensing.

Rings appear as circles in flat space but become distorted arcs
(Einstein rings) around massive objects.
-/
def ringColormap (escapeRadius : ℝ) (numRings : ℝ := 10.0)
    (data : ODEData Particle) : RGB :=
  let pos := data.value.position
  let spherical := sphericalFromCartesian pos

  if spherical.v1 >= escapeRadius then
    let theta := spherical.v2

    -- Rings based on latitude
    let ringIndex := Float.floor (theta * numRings / π)
    let isEven := ringIndex.toUInt64 % 2 == 0

    -- Color by latitude band
    let hue := theta / π  -- 0 at pole, 1 at other pole
    if isEven then
      -- Colored ring
      let r := (255.0 * (1.0 - hue)).toUInt32.toNat
      let b := (255.0 * hue).toUInt32.toNat
      RGB.ofNat r 100 b
    else
      RGB.ofNat 20 20 20  -- Dark ring
  else
    RGB.black

/-- Starfield background with realistic distribution.

Sparse random-looking stars based on position hash.
More stars near galactic plane (z ≈ 0).
-/
def starfieldColormap (escapeRadius : ℝ) (starDensity : ℝ := 0.02)
    (data : ODEData Particle) : RGB :=
  let pos := data.value.position
  let spherical := sphericalFromCartesian pos

  if spherical.v1 >= escapeRadius then
    let theta := spherical.v2
    let phi := spherical.v3

    -- Pseudo-random star placement using trig hash
    let hash1 := Float.sin (theta * 127.1 + phi * 311.7)
    let hash2 := Float.cos (theta * 269.5 + phi * 183.3)
    let starProb := Float.abs (hash1 * hash2)

    -- More stars near equator (galactic plane)
    let galacticBoost := 1.0 + Float.abs (Float.cos theta)
    let threshold := 1.0 - starDensity * galacticBoost

    if starProb > threshold then
      -- Star brightness varies
      let brightness := (150.0 + 105.0 * Float.abs hash1).toUInt32.toNat
      -- Slight color variation
      let colorVar := hash2
      if colorVar > 0.3 then
        RGB.ofNat brightness brightness brightness  -- White star
      else if colorVar > -0.3 then
        RGB.ofNat brightness brightness (brightness - 30)  -- Yellow-ish
      else
        RGB.ofNat (brightness - 30) (brightness - 20) brightness  -- Blue-ish
    else
      RGB.ofNat 2 2 8  -- Very dark blue background
  else
    RGB.black

end Image
end TetraGrayer
