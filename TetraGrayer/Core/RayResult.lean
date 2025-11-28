/-
Sum types for ray tracing outcomes.

Unlike C++ which uses boolean flags or magic values, Lean's inductive types
let us represent ray termination conditions precisely and exhaustively.
-/

import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Clifford
import TetraGrayer.Core.Particle

namespace TetraGrayer
namespace Core

/-- Termination reason for a ray.

This is a classic sum type impossible in C++ without std::variant.
Pattern matching ensures we handle all cases.
-/
inductive TerminationReason where
  /-- Ray escaped to extraction sphere -/
  | escaped (radius : ℝ)
  /-- Ray fell into black hole horizon -/
  | absorbed
  /-- Affine parameter exceeded maximum -/
  | timedOut (param : ℝ)
  /-- Step ratio exceeded (extreme blueshift near horizon) -/
  | blueshifted (ratio : ℝ)
  /-- Maximum integration steps reached -/
  | maxSteps
deriving Repr, Inhabited

/-- Ray tracing result: either still propagating or terminated.

This replaces the C++ pattern of returning a particle with flags.
-/
inductive RayResult where
  /-- Ray still propagating, can take more steps -/
  | propagating (data : ODEData Particle)
  /-- Ray terminated with final state and reason -/
  | terminated (data : ODEData Particle) (reason : TerminationReason)
deriving Repr

namespace RayResult

/-- Extract the particle data regardless of termination. -/
def data : RayResult → ODEData Particle
  | propagating d => d
  | terminated d _ => d

/-- Check if ray has terminated. -/
def isTerminated : RayResult → Bool
  | propagating _ => false
  | terminated _ _ => true

/-- Check if ray escaped (for coloring). -/
def didEscape : RayResult → Bool
  | terminated _ (.escaped _) => true
  | _ => false

/-- Map over the underlying data (functor-like). -/
def map (f : ODEData Particle → ODEData Particle) : RayResult → RayResult
  | propagating d => propagating (f d)
  | terminated d r => terminated (f d) r

/-- Get termination reason if terminated. -/
def reason? : RayResult → Option TerminationReason
  | propagating _ => none
  | terminated _ r => some r

end RayResult

/-- Quadrant for spherical colormap (sum type for color selection). -/
inductive Quadrant where
  /-- y ≥ 0, z ≥ 0: red -/
  | northEast
  /-- y < 0, z ≥ 0: green -/
  | northWest
  /-- y ≥ 0, z < 0: blue -/
  | southEast
  /-- y < 0, z < 0: yellow -/
  | southWest
deriving Repr, DecidableEq

namespace Quadrant

/-- Determine quadrant from Cartesian y, z coordinates. -/
def fromYZ (y z : ℝ) : Quadrant :=
  if y ≥ 0 then
    if z ≥ 0 then .northEast else .southEast
  else
    if z ≥ 0 then .northWest else .southWest

end Quadrant

/-- Pixel location with dependent type ensuring bounds. -/
structure Pixel (width height : Nat) where
  /-- Horizontal coordinate (0 to width-1) -/
  x : Fin width
  /-- Vertical coordinate (0 to height-1) -/
  y : Fin height
deriving Repr

namespace Pixel

/-- Linear index from 2D coordinates. -/
def toIndex {w h : Nat} (p : Pixel w h) : Nat :=
  p.y.val * w + p.x.val

/-- Create pixel from Nats with proof of bounds. -/
def ofNat? (w h : Nat) (x y : Nat) : Option (Pixel w h) :=
  if hx : x < w then
    if hy : y < h then
      some ⟨⟨x, hx⟩, ⟨y, hy⟩⟩
    else none
  else none

end Pixel

end Core
end TetraGrayer
