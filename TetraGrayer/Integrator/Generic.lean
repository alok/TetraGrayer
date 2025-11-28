/-
Generic ODE integrator with RK4 and support for adaptive stepping.

Ported from tetra-gray's integrator.cuh and stepsize.cuh.

This module uses proper functional patterns:
- Pure tail-recursive integration loops
- Functor/Applicative structure for RK4
- Clean separation of concerns
-/

import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Clifford
import TetraGrayer.Core.CoordSystems
import TetraGrayer.Core.Particle

namespace TetraGrayer
namespace Integrator

open Core

-- ============================================================================
-- RK4 as a higher-order combinator
-- ============================================================================

/-- RK4 coefficients as a dependent type for documentation -/
structure RK4Coeffs where
  c1 : ℝ := 1.0 / 6.0
  c2 : ℝ := 1.0 / 3.0
  c3 : ℝ := 1.0 / 3.0
  c4 : ℝ := 1.0 / 6.0

/-- RK4 step for generic ODE: dy/dt = f(y, t).

This is a pure function implementing the classical 4th-order Runge-Kutta method.
The type signature captures the algebraic requirements: we need addition and
scalar multiplication on the state space A.

Parameters:
- f: vector field (state, parameter) → derivative
- dt: step size
- y: current state
- t: current parameter value

Returns: new state after one RK4 step
-/
def rk4Step [Add A] [HMul ℝ A A] (f : A → ℝ → A) (dt : ℝ) (y : A) (t : ℝ) : A :=
  let k1 := f y t
  let k2 := f (y + (dt / 2.0) * k1) (t + dt / 2.0)
  let k3 := f (y + (dt / 2.0) * k2) (t + dt / 2.0)
  let k4 := f (y + dt * k3) (t + dt)
  y + (dt / 6.0) * (k1 + 2.0 * k2 + 2.0 * k3 + k4)

/-- Lift RK4 to operate on ODEData wrapper -/
def rk4StepODE [Add A] [HMul ℝ A A] (f : A → ℝ → A) (ode : ODEData A) : ODEData A :=
  let newValue := rk4Step f ode.dparam ode.value ode.param
  ⟨newValue, ode.param + ode.dparam, ode.dparam⟩

-- ============================================================================
-- Parameterized RK4 for spacetimes with extra parameters
-- ============================================================================

/-- RK4 step with an extra parameter (e.g., black hole spin).

This curries the extra parameter into the RHS, making it compatible with
standard RK4 integration.
-/
def rk4StepParametric (f : Particle → ℝ → ℝ → Particle) (extraParam : ℝ)
    (ode : ODEData Particle) : ODEData Particle :=
  -- Curry the extra parameter to get a standard RHS
  rk4StepODE (fun p t => f p t extraParam) ode

-- ============================================================================
-- Stop conditions as predicates
-- ============================================================================

/-- Predicate: has particle escaped beyond given radius? -/
def escaped (radius : ℝ) (data : ODEData Particle) : Bool :=
  let r := (sphericalFromCartesian data.value.position).v1
  r >= radius

/-- Predicate: has affine parameter exceeded maximum? -/
def paramExceeded (maxParam : ℝ) (data : ODEData Particle) : Bool :=
  data.param >= maxParam

/-- Predicate: has step ratio exceeded maximum? (blueshift detection) -/
def stepRatioExceeded (maxRatio dparam0 : ℝ) (data : ODEData Particle) : Bool :=
  dparam0 / data.dparam >= maxRatio

/-- Combined stop condition type alias -/
abbrev StopCondition := ODEData Particle → Bool

/-- Combine stop conditions with disjunction -/
def orStop (p q : StopCondition) : StopCondition := fun d => p d || q d

instance : OrOp StopCondition := ⟨orStop⟩

-- ============================================================================
-- Proposition-based predicates with Decidable instances
-- ============================================================================

/-- Proposition: particle has escaped beyond radius. -/
def Escaped (radius : ℝ) (data : ODEData Particle) : Prop :=
  (sphericalFromCartesian data.value.position).v1 ≥ radius

/-- Decidable instance for Escaped. -/
instance (radius : ℝ) (data : ODEData Particle) : Decidable (Escaped radius data) :=
  if h : (sphericalFromCartesian data.value.position).v1 ≥ radius
  then isTrue h
  else isFalse h

/-- Proposition: affine parameter exceeded. -/
def ParamExceeded (maxParam : ℝ) (data : ODEData Particle) : Prop :=
  data.param ≥ maxParam

/-- Decidable instance for ParamExceeded. -/
instance (maxParam : ℝ) (data : ODEData Particle) : Decidable (ParamExceeded maxParam data) :=
  if h : data.param ≥ maxParam then isTrue h else isFalse h

/-- Proposition: step ratio exceeded (blueshift). -/
def StepRatioExceeded (maxRatio dparam0 : ℝ) (data : ODEData Particle) : Prop :=
  dparam0 / data.dparam ≥ maxRatio

/-- Decidable instance for StepRatioExceeded. -/
instance (maxRatio dparam0 : ℝ) (data : ODEData Particle) : Decidable (StepRatioExceeded maxRatio dparam0 data) :=
  if h : dparam0 / data.dparam ≥ maxRatio then isTrue h else isFalse h

/-- Proposition: ray should terminate. -/
def ShouldTerminate (escapeRadius maxParam maxRatio dparam0 : ℝ) (data : ODEData Particle) : Prop :=
  Escaped escapeRadius data ∨ ParamExceeded maxParam data ∨ StepRatioExceeded maxRatio dparam0 data

/-- Decidable instance for ShouldTerminate. -/
instance (escapeRadius maxParam maxRatio dparam0 : ℝ) (data : ODEData Particle) :
    Decidable (ShouldTerminate escapeRadius maxParam maxRatio dparam0 data) :=
  instDecidableOr  -- Uses decidability of Or with decidable components

-- ============================================================================
-- Dynamic stepsize adjustment
-- ============================================================================

/-- Adjust step size based on time component of momentum.

In curved spacetime, photons blueshift approaching black holes,
requiring smaller coordinate steps for the same proper distance.
We scale dparam inversely with |p^t|.
-/
def adjustStepsize (dparam0 : ℝ) (data : ODEData Particle) : ODEData Particle :=
  let pt := Float.abs data.value.momentum.v0
  let ratio := if pt < 1e-10 then 1e-10 else pt
  ⟨data.value, data.param, dparam0 / ratio⟩

-- ============================================================================
-- Pure tail-recursive integration
-- ============================================================================

/-- Tail-recursive integration loop for flat spacetime.

Uses structural recursion on fuel (maxSteps) for termination proof.
This is a pure function with no mutable state.
-/
def integrateFlat (rhs : Particle → ℝ → Particle)
    (stop : StopCondition) : Nat → ODEData Particle → ODEData Particle
  | 0, ode => ode
  | n + 1, ode =>
    if stop ode then ode
    else integrateFlat rhs stop n (rk4StepODE rhs ode)

/-- Tail-recursive integration loop for curved spacetime with adaptive stepping.

Each iteration:
1. Check stop condition
2. Take RK4 step
3. Adjust stepsize based on blueshift
-/
def integrateAdaptive (rhs : Particle → ℝ → ℝ → Particle) (extraParam dparam0 : ℝ)
    (stop : StopCondition) : Nat → ODEData Particle → ODEData Particle
  | 0, ode => ode
  | n + 1, ode =>
    if stop ode then ode
    else
      let stepped := rk4StepParametric rhs extraParam ode
      let adjusted := adjustStepsize dparam0 stepped
      integrateAdaptive rhs extraParam dparam0 stop n adjusted

-- ============================================================================
-- High-level integration interface
-- ============================================================================

/-- Integration configuration for flat spacetime -/
structure FlatConfig where
  extractRadius : ℝ
  maxParam : ℝ
  maxSteps : Nat

/-- Integration configuration for Doran (Kerr) spacetime -/
structure DoranConfig where
  spinParam : ℝ
  extractRadius : ℝ
  maxParam : ℝ
  maxStepRatio : ℝ
  maxSteps : Nat
  dparam0 : ℝ

/-- Integrate in flat spacetime -/
def runFlatIntegration (cfg : FlatConfig) (rhs : Particle → ℝ → Particle)
    (initial : ODEData Particle) : ODEData Particle :=
  let stop := escaped cfg.extractRadius ||| paramExceeded cfg.maxParam
  integrateFlat rhs stop cfg.maxSteps initial

/-- Integrate in Doran spacetime with adaptive stepping -/
def runDoranIntegration (cfg : DoranConfig) (rhs : Particle → ℝ → ℝ → Particle)
    (initial : ODEData Particle) : ODEData Particle :=
  let stop := escaped cfg.extractRadius |||
              paramExceeded cfg.maxParam |||
              stepRatioExceeded cfg.maxStepRatio cfg.dparam0
  integrateAdaptive rhs cfg.spinParam cfg.dparam0 stop cfg.maxSteps initial

-- ============================================================================
-- Legacy interface for compatibility
-- ============================================================================

/-- Integrate particle ODE until stop condition (simple version). -/
def integrateParticleSimple (rhs : Particle → ℝ → Particle)
    (stop : ODEData Particle → Bool) (maxSteps : Nat)
    (ode0 : ODEData Particle) : ODEData Particle :=
  integrateFlat rhs stop maxSteps ode0

/-- Integrate particle ODE with adaptive stepsize and extra parameter. -/
def integrateParticleAdaptive (rhs : Particle → ℝ → ℝ → Particle)
    (extraParam dparam0 escapeRad maxParam maxStepRatio : ℝ) (maxSteps : Nat)
    (ode0 : ODEData Particle) : ODEData Particle :=
  let stop := escaped escapeRad ||| paramExceeded maxParam |||
              stepRatioExceeded maxStepRatio dparam0
  integrateAdaptive rhs extraParam dparam0 stop maxSteps ode0

-- Legacy aliases
def escapedRadius := escaped
def shouldStopDoran (escapeRad maxParam maxStepRatio dparam0 : ℝ) : StopCondition :=
  escaped escapeRad ||| paramExceeded maxParam ||| stepRatioExceeded maxStepRatio dparam0

end Integrator
end TetraGrayer
