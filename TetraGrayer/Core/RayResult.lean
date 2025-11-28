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

/-- Generic ray tracing result: either still propagating or terminated.

Parametric over the data type `α` to enable Functor instance.
-/
inductive RayResult (α : Type) where
  /-- Ray still propagating, can take more steps -/
  | propagating (data : α)
  /-- Ray terminated with final state and reason -/
  | terminated (data : α) (reason : TerminationReason)
deriving Repr

/-- Specialized ray result for particle integration. -/
abbrev ParticleResult := RayResult (ODEData Particle)

namespace RayResult

/-- Extract the data regardless of termination. -/
def data : RayResult α → α
  | propagating d => d
  | terminated d _ => d

/-- Check if ray has terminated. -/
def isTerminated : RayResult α → Bool
  | propagating _ => false
  | terminated _ _ => true

/-- Check if ray escaped (for coloring). -/
def didEscape : RayResult α → Bool
  | terminated _ (.escaped _) => true
  | _ => false

/-- Get termination reason if terminated. -/
def reason? : RayResult α → Option TerminationReason
  | propagating _ => none
  | terminated _ r => some r

/-- Functor instance: map over the underlying data. -/
instance : Functor RayResult where
  map f
    | propagating d => propagating (f d)
    | terminated d r => terminated (f d) r

/-- LawfulFunctor: map preserves identity. -/
instance : LawfulFunctor RayResult where
  map_const := rfl
  id_map := fun x => by cases x <;> rfl
  comp_map := fun f g x => by cases x <;> rfl

/-- Pure: wrap a value as propagating (no termination). -/
protected def pure (a : α) : RayResult α := propagating a

/-- Seq: apply a wrapped function, preserving termination from either side.

If either side is terminated, the result is terminated (first termination wins).
-/
protected def seq : RayResult (α → β) → (Unit → RayResult α) → RayResult β
  | propagating f, a => f <$> a ()
  | terminated f r, a =>
    match a () with
    | propagating x => terminated (f x) r
    | terminated x _ => terminated (f x) r  -- preserve first termination

/-- Applicative instance for RayResult.

Termination is "contagious" - if any computation terminates, the result is terminated.
-/
instance : Applicative RayResult where
  pure := RayResult.pure
  seq := RayResult.seq

/-- Bind: chain computations, propagating termination.

Semantics:
- If propagating, apply the function normally
- If terminated, apply function but mark result as terminated (preserves termination)

This allows monadic composition while tracking termination status.
-/
protected def bind : RayResult α → (α → RayResult β) → RayResult β
  | propagating d, f => f d
  | terminated d r, f =>
    match f d with
    | propagating d' => terminated d' r    -- preserve original termination
    | terminated d' r' => terminated d' r' -- use newer termination reason

/-- Monad instance for RayResult.

Termination propagates through the chain - once terminated, subsequent
computations still run but their results are marked as terminated.
-/
instance : Monad RayResult where
  bind := RayResult.bind

/-- Apply a function only if propagating, otherwise keep terminated state unchanged. -/
def andThen (result : RayResult α) (f : α → RayResult α) : RayResult α :=
  match result with
  | propagating d => f d
  | terminated d r => terminated d r

/-- Strict short-circuit bind: don't apply function if already terminated. -/
def bindStrict : RayResult α → (α → RayResult β) → Option (RayResult β)
  | propagating d, f => some (f d)
  | terminated _ _, _ => none

/-- Combine two results, short-circuiting on first termination. -/
def combine (r1 r2 : RayResult α) (f : α → α → α) : RayResult α :=
  match r1, r2 with
  | propagating d1, propagating d2 => propagating (f d1 d2)
  | terminated d r, _ => terminated d r
  | _, terminated d r => terminated d r

/-- Lift an Option into RayResult with default value for None case. -/
def ofOption (default : α) : Option α → RayResult α
  | some a => propagating a
  | none => terminated default .absorbed

/-- Check if still propagating (not terminated). -/
def isPropagating : RayResult α → Bool
  | propagating _ => true
  | terminated _ _ => false

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
