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

/-- RK4 coefficients as a dependent type for documentation. -/
structure RK4Coeffs where
  /-- Weight for k1 (default 1/6). -/
  c1 : ℝ := 1.0 / 6.0
  /-- Weight for k2 (default 1/3). -/
  c2 : ℝ := 1.0 / 3.0
  /-- Weight for k3 (default 1/3). -/
  c3 : ℝ := 1.0 / 3.0
  /-- Weight for k4 (default 1/6). -/
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
@[inline, specialize] def rk4Step [Add A] [HMul ℝ A A] (f : A → ℝ → A) (dt : ℝ) (y : A) (t : ℝ) : A :=
  let k1 := f y t
  let k2 := f (y + (dt / 2.0) * k1) (t + dt / 2.0)
  let k3 := f (y + (dt / 2.0) * k2) (t + dt / 2.0)
  let k4 := f (y + dt * k3) (t + dt)
  y + (dt / 6.0) * (k1 + 2.0 * k2 + 2.0 * k3 + k4)

/-- Specialized RK4 step for Particle. -/
@[inline, specialize] def rk4StepParticle (f : Particle → ℝ → Particle) (dt : ℝ) (y : Particle) (t : ℝ) : Particle :=
  let k1 := f y t
  let halfDt := dt / 2.0
  let k2 := f (y + halfDt • k1) (t + halfDt)
  let k3 := f (y + halfDt • k2) (t + halfDt)
  let k4 := f (y + dt • k3) (t + dt)
  y + (dt / 6.0) • (k1 + 2.0 • k2 + 2.0 • k3 + k4)

/-- Fused RK4 step for Particle - attempt to minimize intermediate allocations.

Computes the final combination in one pass by directly computing each component
without creating intermediate Particle objects.

**Benchmark result (1280x720 Doran render):**
- Standard: 73.9s
- Fused: 77.0s
- Speedup: 0.96x (4% slower)

**Conclusion:** Manual loop fusion does NOT help and slightly hurts performance.
The Lean 4 compiler's existing optimizations are sufficient. The bottleneck is
in the RHS function (closure creation) and geodesic computation, not in add/smul.
-/
@[inline, specialize] def rk4StepParticleFused (f : Particle → ℝ → Particle) (dt : ℝ) (y : Particle) (t : ℝ) : Particle :=
  let k1 := f y t
  let halfDt := dt / 2.0
  let k2 := f (Particle.add y (Particle.smul halfDt k1)) (t + halfDt)
  let k3 := f (Particle.add y (Particle.smul halfDt k2)) (t + halfDt)
  let k4 := f (Particle.add y (Particle.smul dt k3)) (t + dt)
  -- Fused combination: y + (dt/6)*(k1 + 2*k2 + 2*k3 + k4)
  -- Weights: w = dt/6, w2 = dt/3 = 2*w
  let w := dt / 6.0
  let w2 := dt / 3.0
  ⟨ CliffordVector.mk4
      (y.position.v0 + w * k1.position.v0 + w2 * k2.position.v0 + w2 * k3.position.v0 + w * k4.position.v0)
      (y.position.v1 + w * k1.position.v1 + w2 * k2.position.v1 + w2 * k3.position.v1 + w * k4.position.v1)
      (y.position.v2 + w * k1.position.v2 + w2 * k2.position.v2 + w2 * k3.position.v2 + w * k4.position.v2)
      (y.position.v3 + w * k1.position.v3 + w2 * k2.position.v3 + w2 * k3.position.v3 + w * k4.position.v3)
  , CliffordVector.mk4
      (y.momentum.v0 + w * k1.momentum.v0 + w2 * k2.momentum.v0 + w2 * k3.momentum.v0 + w * k4.momentum.v0)
      (y.momentum.v1 + w * k1.momentum.v1 + w2 * k2.momentum.v1 + w2 * k3.momentum.v1 + w * k4.momentum.v1)
      (y.momentum.v2 + w * k1.momentum.v2 + w2 * k2.momentum.v2 + w2 * k3.momentum.v2 + w * k4.momentum.v2)
      (y.momentum.v3 + w * k1.momentum.v3 + w2 * k2.momentum.v3 + w2 * k3.momentum.v3 + w * k4.momentum.v3)
  ⟩

/-- Lift RK4 to operate on ODEData wrapper -/
@[inline, specialize] def rk4StepODE [Add A] [HMul ℝ A A] (f : A → ℝ → A) (ode : ODEData A) : ODEData A :=
  let newValue := rk4Step f ode.dparam ode.value ode.param
  ⟨newValue, ode.param + ode.dparam, ode.dparam⟩

/-- Specialized RK4 ODE step for Particle - uses direct operations. -/
@[inline, specialize] def rk4StepParticleODE (f : Particle → ℝ → Particle) (ode : ODEData Particle) : ODEData Particle :=
  let newValue := rk4StepParticle f ode.dparam ode.value ode.param
  ⟨newValue, ode.param + ode.dparam, ode.dparam⟩

/-- Fused RK4 ODE step for Particle - minimizes intermediate allocations. -/
@[inline, specialize] def rk4StepParticleODEFused (f : Particle → ℝ → Particle) (ode : ODEData Particle) : ODEData Particle :=
  let newValue := rk4StepParticleFused f ode.dparam ode.value ode.param
  ⟨newValue, ode.param + ode.dparam, ode.dparam⟩

-- ============================================================================
-- Parameterized RK4 for spacetimes with extra parameters
-- ============================================================================

/-- RK4 step with an extra parameter (e.g., black hole spin).

This curries the extra parameter into the RHS, making it compatible with
standard RK4 integration.

Note: A/B testing showed typeclass-based version with `@[inline, specialize]`
matches specialized version performance. The compiler handles typeclass
specialization well. The specialized version is kept for reference.
-/
@[inline] def rk4StepParametric (f : Particle → ℝ → ℝ → Particle) (extraParam : ℝ)
    (ode : ODEData Particle) : ODEData Particle :=
  -- Curry the extra parameter to get a standard RHS
  -- Using specialized version - see note above
  rk4StepParticleODE (fun p t => f p t extraParam) ode

/-- RK4 step using typeclass-based generic version (for comparison). -/
@[inline] def rk4StepParametricGeneric (f : Particle → ℝ → ℝ → Particle) (extraParam : ℝ)
    (ode : ODEData Particle) : ODEData Particle :=
  rk4StepODE (fun p t => f p t extraParam) ode

/-- Fused RK4 step with extra parameter - minimizes intermediate allocations. -/
@[inline] def rk4StepParametricFused (f : Particle → ℝ → ℝ → Particle) (extraParam : ℝ)
    (ode : ODEData Particle) : ODEData Particle :=
  rk4StepParticleODEFused (fun p t => f p t extraParam) ode

-- ============================================================================
-- Stop conditions as predicates
-- ============================================================================

/-- Predicate: has particle escaped beyond given radius? -/
@[inline] def escaped (radius : ℝ) (data : ODEData Particle) : Bool :=
  let r := (sphericalFromCartesian data.value.position).v1
  r >= radius

/-- Predicate: has affine parameter exceeded maximum? -/
@[inline] def paramExceeded (maxParam : ℝ) (data : ODEData Particle) : Bool :=
  data.param >= maxParam

/-- Predicate: has step ratio exceeded maximum? (blueshift detection) -/
@[inline] def stepRatioExceeded (maxRatio dparam0 : ℝ) (data : ODEData Particle) : Bool :=
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
@[inline] def adjustStepsize (dparam0 : ℝ) (data : ODEData Particle) : ODEData Particle :=
  let pt := Float.abs data.value.momentum.v0
  let ratio := if pt < 1e-10 then 1e-10 else pt
  ⟨data.value, data.param, dparam0 / ratio⟩

-- ============================================================================
-- Pure tail-recursive integration
-- ============================================================================

/-- Tail-recursive integration loop for flat spacetime.

Uses structural recursion on fuel (maxSteps) for termination proof.
This is a pure function with no mutable state.
Uses specialized Particle RK4 for performance.
-/
def integrateFlat (rhs : Particle → ℝ → Particle)
    (stop : StopCondition) : Nat → ODEData Particle → ODEData Particle
  | 0, ode => ode
  | n + 1, ode =>
    if stop ode then ode
    else integrateFlat rhs stop n (rk4StepParticleODE rhs ode)

/-- Tail-recursive integration loop for curved spacetime with adaptive stepping.

Each iteration:
1. Check stop condition
2. Take RK4 step
3. Adjust stepsize based on blueshift
-/
@[specialize] def integrateAdaptive (rhs : Particle → ℝ → ℝ → Particle) (extraParam dparam0 : ℝ)
    (stop : StopCondition) : Nat → ODEData Particle → ODEData Particle
  | 0, ode => ode
  | n + 1, ode =>
    if stop ode then ode
    else
      let stepped := rk4StepParametric rhs extraParam ode
      let adjusted := adjustStepsize dparam0 stepped
      integrateAdaptive rhs extraParam dparam0 stop n adjusted

/-- Version using typeclass-based RK4 for A/B testing. -/
@[specialize] def integrateAdaptiveGeneric (rhs : Particle → ℝ → ℝ → Particle) (extraParam dparam0 : ℝ)
    (stop : StopCondition) : Nat → ODEData Particle → ODEData Particle
  | 0, ode => ode
  | n + 1, ode =>
    if stop ode then ode
    else
      let stepped := rk4StepParametricGeneric rhs extraParam ode
      let adjusted := adjustStepsize dparam0 stepped
      integrateAdaptiveGeneric rhs extraParam dparam0 stop n adjusted

/-- Fused integration loop - minimizes intermediate Particle allocations in RK4 combination. -/
@[specialize] def integrateAdaptiveFused (rhs : Particle → ℝ → ℝ → Particle) (extraParam dparam0 : ℝ)
    (stop : StopCondition) : Nat → ODEData Particle → ODEData Particle
  | 0, ode => ode
  | n + 1, ode =>
    if stop ode then ode
    else
      let stepped := rk4StepParametricFused rhs extraParam ode
      let adjusted := adjustStepsize dparam0 stepped
      integrateAdaptiveFused rhs extraParam dparam0 stop n adjusted

/-- Debug RK4 step with sharing checks on k1,k2,k3,k4.

**Verified:** Running with `tetragrayer debug` shows NO sharing messages,
confirming all values are used linearly. Refcounting is NOT a bottleneck.
The performance is dominated by the geodesic computation (doranRHS) itself.
-/
@[inline, specialize] def rk4StepParticleDebug (f : Particle → ℝ → Particle) (dt : ℝ) (y : Particle) (t : ℝ) : Particle :=
  let y := dbgTraceIfShared "y shared in RK4" y
  let k1 := f y t
  let k1 := dbgTraceIfShared "k1 shared after f(y,t)" k1
  let halfDt := dt / 2.0
  let k2 := f (y + halfDt • k1) (t + halfDt)
  let k2 := dbgTraceIfShared "k2 shared after f(y+dt/2*k1)" k2
  let k3 := f (y + halfDt • k2) (t + halfDt)
  let k3 := dbgTraceIfShared "k3 shared after f(y+dt/2*k2)" k3
  let k4 := f (y + dt • k3) (t + dt)
  let k4 := dbgTraceIfShared "k4 shared after f(y+dt*k3)" k4
  y + (dt / 6.0) • (k1 + 2.0 • k2 + 2.0 • k3 + k4)

/-- Debug RK4 ODE step. -/
@[inline, specialize] def rk4StepParticleODEDebug (f : Particle → ℝ → Particle) (ode : ODEData Particle) : ODEData Particle :=
  let newValue := rk4StepParticleDebug f ode.dparam ode.value ode.param
  ⟨newValue, ode.param + ode.dparam, ode.dparam⟩

/-- Debug parametric RK4 step. -/
@[inline] def rk4StepParametricDebug (f : Particle → ℝ → ℝ → Particle) (extraParam : ℝ)
    (ode : ODEData Particle) : ODEData Particle :=
  rk4StepParticleODEDebug (fun p t => f p t extraParam) ode

/-- Debug version with sharing checks using `dbgTraceIfShared`.

Run with this to see if ODEData or Particle values are unexpectedly shared
(refcount > 1), which would cause extra allocations.
-/
@[specialize] def integrateAdaptiveDebug (rhs : Particle → ℝ → ℝ → Particle) (extraParam dparam0 : ℝ)
    (stop : StopCondition) : Nat → ODEData Particle → ODEData Particle
  | 0, ode => ode
  | n + 1, ode =>
    let ode := dbgTraceIfShared "ode shared at loop start" ode
    if stop ode then ode
    else
      let stepped := rk4StepParametricDebug rhs extraParam ode
      let stepped := dbgTraceIfShared "stepped shared after RK4" stepped
      let adjusted := adjustStepsize dparam0 stepped
      let adjusted := dbgTraceIfShared "adjusted shared after stepsize" adjusted
      integrateAdaptiveDebug rhs extraParam dparam0 stop n adjusted

-- ============================================================================
-- High-level integration interface
-- ============================================================================

/-- Integration configuration for flat spacetime. -/
structure FlatConfig where
  /-- Escape radius for ray termination. -/
  extractRadius : ℝ
  /-- Maximum affine parameter value. -/
  maxParam : ℝ
  /-- Maximum integration steps. -/
  maxSteps : Nat

/-- Integration configuration for Doran (Kerr) spacetime. -/
structure DoranConfig where
  /-- Kerr spin parameter (0 to 1). -/
  spinParam : ℝ
  /-- Escape radius for ray termination. -/
  extractRadius : ℝ
  /-- Maximum affine parameter value. -/
  maxParam : ℝ
  /-- Maximum step ratio (blueshift cutoff). -/
  maxStepRatio : ℝ
  /-- Maximum integration steps. -/
  maxSteps : Nat
  /-- Initial step size. -/
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

/-- Integrate in Doran spacetime using typeclass-based RK4 (for A/B testing). -/
def runDoranIntegrationGeneric (cfg : DoranConfig) (rhs : Particle → ℝ → ℝ → Particle)
    (initial : ODEData Particle) : ODEData Particle :=
  let stop := escaped cfg.extractRadius |||
              paramExceeded cfg.maxParam |||
              stepRatioExceeded cfg.maxStepRatio cfg.dparam0
  integrateAdaptiveGeneric rhs cfg.spinParam cfg.dparam0 stop cfg.maxSteps initial

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

/-- Version using typeclass-based RK4 for A/B testing.

Benchmark results show NO significant overhead compared to specialized version
when using `@[inline, specialize]` on the generic rk4Step. The Lean 4 compiler
handles typeclass specialization well in this case.

Note: Stack allocation and further unboxing optimizations are planned for
future compiler versions. Current performance is dominated by:
1. Closure creation for the RHS function (unavoidable with higher-order style)
2. Reference counting overhead
3. Object allocation for intermediate Particle/ODEData values
-/
def integrateParticleAdaptiveGeneric (rhs : Particle → ℝ → ℝ → Particle)
    (extraParam dparam0 escapeRad maxParam maxStepRatio : ℝ) (maxSteps : Nat)
    (ode0 : ODEData Particle) : ODEData Particle :=
  let stop := escaped escapeRad ||| paramExceeded maxParam |||
              stepRatioExceeded maxStepRatio dparam0
  integrateAdaptiveGeneric rhs extraParam dparam0 stop maxSteps ode0

/-- Fused integration - minimizes intermediate Particle allocations.

Uses direct component access for the RK4 combination step instead of
creating intermediate Particle objects for add/smul operations.
This should reduce GC pressure and potentially improve cache locality.
-/
def integrateParticleAdaptiveFused (rhs : Particle → ℝ → ℝ → Particle)
    (extraParam dparam0 escapeRad maxParam maxStepRatio : ℝ) (maxSteps : Nat)
    (ode0 : ODEData Particle) : ODEData Particle :=
  let stop := escaped escapeRad ||| paramExceeded maxParam |||
              stepRatioExceeded maxStepRatio dparam0
  integrateAdaptiveFused rhs extraParam dparam0 stop maxSteps ode0

/-- Debug integration to check for unexpected sharing. -/
def integrateParticleAdaptiveDebug (rhs : Particle → ℝ → ℝ → Particle)
    (extraParam dparam0 escapeRad maxParam maxStepRatio : ℝ) (maxSteps : Nat)
    (ode0 : ODEData Particle) : ODEData Particle :=
  let stop := escaped escapeRad ||| paramExceeded maxParam |||
              stepRatioExceeded maxStepRatio dparam0
  integrateAdaptiveDebug rhs extraParam dparam0 stop maxSteps ode0

-- Legacy aliases
/-- Legacy alias for escaped. -/
def escapedRadius := escaped
/-- Legacy stop condition for Doran integration. -/
def shouldStopDoran (escapeRad maxParam maxStepRatio dparam0 : ℝ) : StopCondition :=
  escaped escapeRad ||| paramExceeded maxParam ||| stepRatioExceeded maxStepRatio dparam0

end Integrator
end TetraGrayer
