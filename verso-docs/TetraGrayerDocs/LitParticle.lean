/-
Literate Lean documentation for TetraGrayer Particle types.

This file uses Verso's literate programming features - module docstrings
become rendered Markdown, and code is syntax-highlighted.
-/

set_option doc.verso true

/-!
# Particle State

A photon in general relativity is described by its position and momentum
in spacetime. TetraGrayer represents this using Clifford algebra vectors.

The fundamental type is a 4-vector representing position or momentum in
spacetime with signature (-,+,+,+).
-/

/-- A 4-vector in Minkowski spacetime. -/
structure CliffordVector where
  /-- Time component -/
  t : Float
  /-- x spatial component -/
  x : Float
  /-- y spatial component -/
  y : Float
  /-- z spatial component -/
  z : Float
  deriving Repr, Inhabited

namespace CliffordVector

/-- Minkowski inner product with signature (-,+,+,+). -/
def dot (u v : CliffordVector) : Float :=
  -u.t * v.t + u.x * v.x + u.y * v.y + u.z * v.z

/-- Scalar multiplication. -/
def smul (s : Float) (v : CliffordVector) : CliffordVector :=
  ⟨s * v.t, s * v.x, s * v.y, s * v.z⟩

/-- Vector addition. -/
def add (u v : CliffordVector) : CliffordVector :=
  ⟨u.t + v.t, u.x + v.x, u.y + v.y, u.z + v.z⟩

instance : Add CliffordVector := ⟨add⟩
instance : HMul Float CliffordVector CliffordVector := ⟨smul⟩

end CliffordVector

/-!
# Example Vectors

Here are some example 4-vectors used in the raytracer:
-/

/-- Example 4-position vector at the origin. -/
def origin : CliffordVector := ⟨0, 0, 0, 0⟩

/-- Example 4-momentum for an outward-going photon. -/
def photonMomentum : CliffordVector := ⟨1, 0, 0, 1⟩

/-!
# Null Vectors

A photon's 4-momentum must be a null vector (zero Minkowski norm).
The inner product is computed as: p·p = -p₀² + p₁² + p₂² + p₃² = 0
-/

/-- Compute the norm of the photon momentum (should be 0 for null vector). -/
def photonNorm : Float := photonMomentum.dot photonMomentum

/-!
# Particle Structure

The Particle type bundles position and momentum together for RK4 integration:
-/

/-- A particle in spacetime with position and momentum. -/
structure Particle where
  /-- 4-position in spacetime -/
  pos : CliffordVector
  /-- 4-momentum (null for photons) -/
  mom : CliffordVector
  deriving Repr, Inhabited

/-- A photon at the origin moving in the z direction. -/
def examplePhoton : Particle := ⟨origin, photonMomentum⟩

/-!
# Vector Space Operations

The particle supports vector space operations for use in RK4 integration:
scalar multiplication and addition.
-/

namespace Particle

/-- Scalar multiplication for RK4. -/
def smul (s : Float) (p : Particle) : Particle :=
  ⟨s * p.pos, s * p.mom⟩

/-- Addition for RK4. -/
def add (p q : Particle) : Particle :=
  ⟨p.pos + q.pos, p.mom + q.mom⟩

instance : Add Particle := ⟨add⟩
instance : HMul Float Particle Particle := ⟨smul⟩

end Particle

/-!
# RK4 Integration

The raytracer uses 4th-order Runge-Kutta to integrate the geodesic equation.
The geodesic equation describes how a particle moves along the shortest path
in curved spacetime, accounting for gravitational effects.

This is implemented by treating Particle as a vector space and providing
a right-hand-side function that computes the derivative at each step.
-/
