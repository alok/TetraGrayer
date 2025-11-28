/-
Flat spacetime RHS: d/dλ pos = mom, d/dλ mom = 0.
-/

import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Vector
import TetraGrayer.Integrator.RK4

namespace TetraGrayer
namespace Spacetimes

open Core Integrator

def flatRHS : RHS := fun s =>
  { pos := s.mom
  , mom := vsmul 0.0 s.mom
  , param := 1.0 }

end Spacetimes
end TetraGrayer
