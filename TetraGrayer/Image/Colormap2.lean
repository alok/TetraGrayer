/-
Spherical colormap using sum types for cleaner pattern matching.

This version uses Lean's inductive types instead of nested if-else,
making the color logic more explicit and easier to reason about.
-/

import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Clifford
import TetraGrayer.Core.CoordSystems
import TetraGrayer.Core.Particle
import TetraGrayer.Core.RayResult
import TetraGrayer.Image.PPM

namespace TetraGrayer
namespace Image

open Core

/-- Grid line status (sum type for stripe checking). -/
inductive GridStatus where
  /-- Position falls on a grid stripe -/
  | onGrid
  /-- Position is off-grid, in the given quadrant -/
  | offGrid (quadrant : Quadrant)
deriving Repr, DecidableEq

/-- Check if angle falls on a grid stripe. -/
def checkStripe (angle stripeInterval stripeHalfWidthRatio : ℝ) : Bool :=
  let stripeNumber := angle / stripeInterval
  let stripeRemainder := stripeNumber - Float.round stripeNumber
  Float.abs stripeRemainder <= stripeHalfWidthRatio

/-- Determine grid status from spherical coordinates. -/
def gridStatus (pos : CliffordVector) (stripeInterval stripeHalfWidth : ℝ) : GridStatus :=
  let spherical := sphericalFromCartesian pos
  let theta := spherical.v2
  let phi := spherical.v3
  if checkStripe theta stripeInterval stripeHalfWidth ||
     checkStripe phi stripeInterval stripeHalfWidth then
    .onGrid
  else
    .offGrid (Quadrant.fromYZ pos.v2 pos.v3)

/-- Color assignment by quadrant (clean pattern match). -/
def quadrantColor : Quadrant → RGB
  | .northEast => RGB.red
  | .northWest => RGB.green
  | .southEast => RGB.blue
  | .southWest => RGB.yellow

/-- Color for grid status (clean pattern match). -/
def gridStatusColor : GridStatus → RGB
  | .onGrid => RGB.black
  | .offGrid q => quadrantColor q

/-- Color by termination reason (sum type → color mapping). -/
def terminationColor : TerminationReason → RGB
  | .escaped _ => RGB.white  -- Will be overridden by grid color
  | .absorbed => RGB.cyan    -- Fell into black hole
  | .timedOut _ => RGB.cyan  -- Didn't escape in time
  | .blueshifted _ => RGB.cyan  -- Extreme blueshift (near horizon)
  | .maxSteps => RGB.cyan    -- Integration timeout

/-- Spherical colormap using RayResult (clean pattern matching).

This version eliminates the boolean flag checking of the C++ version.
The sum type makes all cases explicit.
-/
def sphericalColormapV2 (_escapeRadius : ℝ) (result : ParticleResult) : RGB :=
  match result with
  | .propagating _ =>
    -- Still propagating shouldn't happen at coloring time, treat as absorbed
    RGB.cyan
  | .terminated data reason =>
    match reason with
    | .escaped _ =>
      let stripeInterval : ℝ := π / 18.0  -- 10 degrees
      let stripeHalfWidth : ℝ := 0.05
      gridStatusColor (gridStatus data.value.position stripeInterval stripeHalfWidth)
    | other => terminationColor other

/-- Alternative: Point-free composition style for colormap. -/
def mkColormap (escapeRadius : ℝ) : ParticleResult → RGB :=
  sphericalColormapV2 escapeRadius

/-- Gradient colormap based on affine parameter (useful for debugging). -/
def paramGradient (maxParam : ℝ) : ParticleResult → RGB := fun result =>
  let data := result.data
  let t := if data.param / maxParam > 1.0 then 1.0 else data.param / maxParam
  -- Blue to red gradient
  RGB.ofFloat t 0.0 (1.0 - t)

/-- Colormap showing termination reasons with distinct colors. -/
def debugColormap : ParticleResult → RGB := fun result =>
  match result with
  | .propagating _ => RGB.ofNat 128 128 128  -- Gray for unexpected
  | .terminated _ reason =>
    match reason with
    | .escaped _ => RGB.green
    | .absorbed => RGB.red
    | .timedOut _ => RGB.blue
    | .blueshifted _ => RGB.yellow
    | .maxSteps => RGB.cyan

end Image
end TetraGrayer
