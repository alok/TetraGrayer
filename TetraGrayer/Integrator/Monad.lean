/-
Monadic integration framework for ray tracing.

Uses a custom `RayM` monad that encapsulates:
- Current particle state
- Stop condition checking
- Adaptive step size adjustment

This is more elegant than the C++ approach of threading state manually.
-/

import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Clifford
import TetraGrayer.Core.CoordSystems
import TetraGrayer.Core.Particle
import TetraGrayer.Core.RayResult

namespace TetraGrayer
namespace Integrator

open Core

/-- Configuration for the ray integrator. -/
structure RayConfig where
  /-- Radius at which rays are considered escaped -/
  escapeRadius : ℝ
  /-- Maximum affine parameter -/
  maxParam : ℝ
  /-- Maximum step ratio (for blueshift detection) -/
  maxStepRatio : ℝ
  /-- Initial step size -/
  dparam0 : ℝ
  /-- Extra parameter (e.g., black hole spin) -/
  extraParam : ℝ := 0.0
deriving Repr

/-- State for the ray monad. -/
structure RayState where
  /-- Current ODE data -/
  ode : ODEData Particle
  /-- Steps taken so far -/
  steps : Nat
  /-- Configuration -/
  config : RayConfig
deriving Repr

/-- The Ray monad: Reader + State + short-circuiting on termination.

This cleanly separates concerns:
- Reader: immutable configuration
- State: mutable particle state
- Early return: termination conditions
-/
abbrev RayM := ReaderT RayConfig (StateT (ODEData Particle × Nat) (Except TerminationReason))

namespace RayM

/-- Get current particle data. -/
def getODE : RayM (ODEData Particle) := do
  let (ode, _) ← get
  pure ode

/-- Set current particle data. -/
def setODE (ode : ODEData Particle) : RayM Unit := do
  let (_, steps) ← get
  set (ode, steps)

/-- Increment step counter and get current count. -/
def incSteps : RayM Nat := do
  let (ode, steps) ← get
  set (ode, steps + 1)
  pure (steps + 1)

/-- Get configuration. -/
def getConfig : RayM RayConfig := read

/-- Terminate with given reason (short-circuit). -/
def terminate (reason : TerminationReason) : RayM α :=
  throw reason

/-- Check escape condition. -/
def checkEscape : RayM Unit := do
  let ode ← getODE
  let cfg ← getConfig
  let r := (sphericalFromCartesian ode.value.position).v1
  if r >= cfg.escapeRadius then
    terminate (.escaped r)

/-- Check parameter timeout. -/
def checkParam : RayM Unit := do
  let ode ← getODE
  let cfg ← getConfig
  if ode.param >= cfg.maxParam then
    terminate (.timedOut ode.param)

/-- Check blueshift condition. -/
def checkBlueshift : RayM Unit := do
  let ode ← getODE
  let cfg ← getConfig
  let ratio := cfg.dparam0 / ode.dparam
  if ratio >= cfg.maxStepRatio then
    terminate (.blueshifted ratio)

/-- Run all stop condition checks. -/
def checkStopConditions : RayM Unit := do
  checkEscape
  checkParam
  checkBlueshift

/-- Run the monad with initial state. -/
def run (cfg : RayConfig) (ode : ODEData Particle) (m : RayM α) : ParticleResult :=
  match m cfg |>.run (ode, 0) with
  | .ok (_, (finalOde, _)) => .propagating finalOde
  | .error reason =>
    -- Need to get final state - run again to get it
    let finalOde := match m cfg |>.run (ode, 0) with
      | .ok (_, (d, _)) => d
      | .error _ => ode  -- Use initial if error early
    .terminated finalOde reason

end RayM

/-- RK4 step lifted to RayM monad. -/
def rk4StepM (rhs : Particle → ℝ → ℝ → Particle) : RayM Unit := do
  let cfg ← RayM.getConfig
  let ode ← RayM.getODE
  let f := fun p t => rhs p t cfg.extraParam
  let dt := ode.dparam
  let y := ode.value
  let t := ode.param
  let k1 := f y t
  let k2 := f (y + (dt / 2.0) * k1) (t + dt / 2.0)
  let k3 := f (y + (dt / 2.0) * k2) (t + dt / 2.0)
  let k4 := f (y + dt * k3) (t + dt)
  let newValue := y + (dt / 6.0) * (k1 + 2.0 * k2 + 2.0 * k3 + k4)
  RayM.setODE ⟨newValue, ode.param + dt, ode.dparam⟩

/-- Adjust step size based on blueshift (lifted to monad). -/
def adjustStepsizeM : RayM Unit := do
  let cfg ← RayM.getConfig
  let ode ← RayM.getODE
  let pt := Float.abs ode.value.momentum.v0
  let ratio := if pt < 1e-10 then 1e-10 else pt
  RayM.setODE ⟨ode.value, ode.param, cfg.dparam0 / ratio⟩

/-- Single integration step in the monad. -/
def stepM (rhs : Particle → ℝ → ℝ → Particle) : RayM Unit := do
  RayM.checkStopConditions
  rk4StepM rhs
  adjustStepsizeM
  let _ ← RayM.incSteps

/-- Integrate for n steps using the monad.

Note: Uses partial because we can't easily prove termination
with the monadic short-circuit. In practice, always terminates
because checkStopConditions throws.
-/
partial def integrateM (rhs : Particle → ℝ → ℝ → Particle) (maxSteps : Nat) : RayM Unit := do
  for _ in [0:maxSteps] do
    stepM rhs

/-- High-level integration API using the monad. -/
def integrate (cfg : RayConfig) (rhs : Particle → ℝ → ℝ → Particle)
    (maxSteps : Nat) (initial : ODEData Particle) : ParticleResult :=
  RayM.run cfg initial (integrateM rhs maxSteps)

/-- Simpler integration for flat spacetime using the monad (no blueshift adjustment). -/
def integrateFlatM (escapeRadius maxParam : ℝ) (rhs : Particle → ℝ → Particle)
    (maxSteps : Nat) (initial : ODEData Particle) : ParticleResult :=
  let cfg : RayConfig := {
    escapeRadius := escapeRadius
    maxParam := maxParam
    maxStepRatio := 1e10  -- Effectively disabled
    dparam0 := initial.dparam
    extraParam := 0.0
  }
  -- Adapt 2-arg RHS to 3-arg
  let rhs3 := fun p t _ => rhs p t
  integrate cfg rhs3 maxSteps initial

end Integrator
end TetraGrayer
