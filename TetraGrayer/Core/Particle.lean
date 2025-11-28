import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Clifford

/-!
# Particle (photon) state and ODE data structures

Ported from tetra-gray's particle.cuh and integrator.cuh.
-/

namespace TetraGrayer
namespace Core

/-- A particle (photon) with 4-position and 4-momentum. -/
structure Particle where
  /-- Spacetime position (t, x, y, z) -/
  position : CliffordVector
  /-- 4-momentum (E, px, py, pz) -/
  momentum : CliffordVector
deriving Repr, Inhabited

namespace Particle

/-- Zero particle (both position and momentum zero). -/
def zero : Particle :=
  ⟨CliffordVector.zero, CliffordVector.zero⟩

/-- Create particle from position and momentum. -/
def ofPosMom (pos mom : CliffordVector) : Particle := ⟨pos, mom⟩

/-- Scalar multiply both position and momentum. -/
def smul (s : ℝ) (p : Particle) : Particle :=
  ⟨s * p.position, s * p.momentum⟩

/-- Add two particles componentwise. -/
def add (a b : Particle) : Particle :=
  ⟨a.position + b.position, a.momentum + b.momentum⟩

/-- Negate both position and momentum. -/
def neg (p : Particle) : Particle :=
  ⟨-p.position, -p.momentum⟩

/-- Subtract particles componentwise. -/
def sub (a b : Particle) : Particle :=
  ⟨a.position - b.position, a.momentum - b.momentum⟩

instance : Zero Particle := ⟨zero⟩
instance : Add Particle := ⟨add⟩
instance : Sub Particle := ⟨sub⟩
instance : Neg Particle := ⟨neg⟩
instance : HMul ℝ Particle Particle := ⟨smul⟩
instance : HMul Particle ℝ Particle := ⟨fun p s => smul s p⟩
instance : SMul ℝ Particle := ⟨smul⟩

/-- Time component of position. -/
def t (p : Particle) : ℝ := p.position.v0

/-- Spatial x component of position. -/
def x (p : Particle) : ℝ := p.position.v1

/-- Spatial y component of position. -/
def y (p : Particle) : ℝ := p.position.v2

/-- Spatial z component of position. -/
def z (p : Particle) : ℝ := p.position.v3

/-- Energy component of momentum. -/
def energy (p : Particle) : ℝ := p.momentum.v0

/-- Compute the Minkowski norm squared of the momentum (should be 0 for photons). -/
def momentumNormSq (p : Particle) : ℝ :=
  vectorDot p.momentum p.momentum

/-- Check if particle is approximately null (photon-like). -/
def isNull (p : Particle) (tol : ℝ := 1e-9) : Bool :=
  approxZero (momentumNormSq p) tol

end Particle

/-- ODE data wrapping a value with parameter and step size.

Generic over the evolving type A. For ray tracing, A = Particle.
-/
structure ODEData (A : Type) where
  /-- The current state being evolved -/
  value : A
  /-- Current parameter value (affine parameter λ for photons) -/
  param : ℝ
  /-- Current step size -/
  dparam : ℝ
deriving Repr

namespace ODEData

/-- Create ODE data. -/
def ofData (v : A) (t dt : ℝ) : ODEData A := ⟨v, t, dt⟩

/-- Map over the value while preserving param and dparam. -/
def mapValue (f : A → B) (ode : ODEData A) : ODEData B :=
  ⟨f ode.value, ode.param, ode.dparam⟩

/-- Functor instance for ODEData. -/
instance : Functor ODEData where
  map := mapValue

/-- Update just the value. -/
def withValue (ode : ODEData A) (v : A) : ODEData A :=
  ⟨v, ode.param, ode.dparam⟩

/-- Update just the param. -/
def withParam (ode : ODEData A) (p : ℝ) : ODEData A :=
  ⟨ode.value, p, ode.dparam⟩

/-- Advance param by dparam. -/
def step (ode : ODEData A) : ODEData A :=
  ⟨ode.value, ode.param + ode.dparam, ode.dparam⟩

/-- Scale dparam by a factor. -/
def scaleDparam (ode : ODEData A) (s : ℝ) : ODEData A :=
  ⟨ode.value, ode.param, ode.dparam * s⟩

end ODEData

/-- Alias for particle ODE data. -/
abbrev ParticleData := ODEData Particle

end Core
end TetraGrayer
