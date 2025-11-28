/-
Particle (photon) state and ODE data structures.

Ported from tetra-gray's particle.cuh and integrator.cuh.
-/

import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Clifford

namespace TetraGrayer
namespace Core

/-- A particle (photon) with 4-position and 4-momentum. -/
structure Particle where
  position : CliffordVector
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

instance : HMul ℝ Particle Particle := ⟨smul⟩
instance : HMul Particle ℝ Particle := ⟨fun p s => smul s p⟩
instance : Add Particle := ⟨add⟩

end Particle

/-- ODE data wrapping a value with parameter and step size.

Generic over the evolving type A. For ray tracing, A = Particle.
- value: the current state
- param: current parameter value (affine parameter λ for photons)
- dparam: current step size
-/
structure ODEData (A : Type) where
  value : A
  param : ℝ
  dparam : ℝ
deriving Repr

namespace ODEData

/-- Create ODE data. -/
def ofData (v : A) (t dt : ℝ) : ODEData A := ⟨v, t, dt⟩

end ODEData

/-- Alias for particle ODE data. -/
abbrev ParticleData := ODEData Particle

end Core
end TetraGrayer
