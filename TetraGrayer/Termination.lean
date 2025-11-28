import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Vector
import TetraGrayer.Integrator.RK4

/-! # Termination checks & loop control

Simple fixed-step loop and stop criteria; adaptive stepsize will be added later.
-/

namespace TetraGrayer
namespace Termination

open Core Integrator

/-- Limits for the integration loop. -/
structure Limits where
  maxSteps  : Nat := 1_000
  maxParam  : ℝ := 1_000.0
  escapeRad : ℝ := 1.0e6

/-- Has the spatial position escaped beyond radius `r`? -/
def escaped (s : State) (r : ℝ) : Bool :=
  let x := s.pos.get ⟨1, by decide⟩
  let y := s.pos.get ⟨2, by decide⟩
  let z := s.pos.get ⟨3, by decide⟩
  let rr := x*x + y*y + z*z
  rr > r*r

/-- Has the evolution parameter exceeded the limit? -/
def paramExceeded (s : State) (lim : Limits) : Bool := s.param > lim.maxParam

/-- Should the evolution stop given the current state and step? -/
def shouldStop (s : State) (step : Nat) (lim : Limits) : Bool :=
  step ≥ lim.maxSteps || paramExceeded s lim || escaped s lim.escapeRad

/-- Iterate using a fixed step until a stop condition is met or maxSteps reached. -/
def integrateUntil (rhs : RHS) (h : ℝ) (lim : Limits) (s0 : State) : State :=
  Id.run do
    let mut s := s0
    let mut done := false
    for i in [0:lim.maxSteps] do
      if done || shouldStop s i lim then
        done := true
      else
        s := rk4StepLegacy rhs h s
    s

end Termination
end TetraGrayer
