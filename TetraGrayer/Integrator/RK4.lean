import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Vector

/-! # RK4 integrator

Fourth-order Runge–Kutta stepper over a photon state (position, momentum, parameter).
-/

namespace TetraGrayer
namespace Integrator

open Core

/-- Photon state carried through integration. -/
structure State where
  pos   : Vec4
  mom   : Vec4
  param : ℝ

/-- Right-hand side derivative function. -/
abbrev RHS := State → State

@[inline] def vadd4 (a b : Core.Vec4) : Core.Vec4 := Core.vadd a b
@[inline] def vsmul4 (s : Core.ℝ) (v : Core.Vec4) : Core.Vec4 := Core.vsmul s v

/-- Add two states componentwise. -/
@[inline] def sadd (a b : State) : State :=
  { pos := vadd4 a.pos b.pos, mom := vadd4 a.mom b.mom, param := a.param + b.param }

/-- Scale a state. -/
@[inline] def ssmul (s : ℝ) (a : State) : State :=
  { pos := vsmul4 s a.pos, mom := vsmul4 s a.mom, param := s * a.param }

/-- Perform one RK4 step (legacy version for flat spacetime). -/
def rk4StepLegacy (rhs : RHS) (h : ℝ) (s : State) : State :=
  let k1 := rhs s
  let k2 := rhs (sadd s (ssmul (h * 0.5) k1))
  let k3 := rhs (sadd s (ssmul (h * 0.5) k2))
  let k4 := rhs (sadd s (ssmul h k3))
  sadd s (ssmul (h / 6.0) (sadd k1 (sadd (ssmul 2.0 k2) (sadd (ssmul 2.0 k3) k4))))

end Integrator
end TetraGrayer
