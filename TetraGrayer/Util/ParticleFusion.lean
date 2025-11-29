/-
Particle operation fusion.

Fused operations that avoid intermediate allocations.

**Experimental results:** Benchmarking showed that manual loop fusion does NOT
provide a speedup. The fused RK4 combination was actually 4% slower than the
standard implementation. This suggests:
1. The Lean 4 compiler already optimizes away most intermediate allocations
2. The real bottleneck is in closure creation for RHS functions and geodesic computation
3. Manual fusion increases code size without reducing allocations

These utilities are kept for reference but should NOT be used in production code.
-/
import TetraGrayer.Core.Particle

namespace TetraGrayer.Util

open Core

/-- Fused add-then-smul: `s * (a + b)` computed in one pass. -/
@[inline] def Particle.addThenSmul (s : ℝ) (a b : Particle) : Particle :=
  ⟨ CliffordVector.mk4
      (s * (a.position.v0 + b.position.v0))
      (s * (a.position.v1 + b.position.v1))
      (s * (a.position.v2 + b.position.v2))
      (s * (a.position.v3 + b.position.v3))
  , CliffordVector.mk4
      (s * (a.momentum.v0 + b.momentum.v0))
      (s * (a.momentum.v1 + b.momentum.v1))
      (s * (a.momentum.v2 + b.momentum.v2))
      (s * (a.momentum.v3 + b.momentum.v3))
  ⟩

/-- Fused smul-then-add: `(s * a) + b` computed in one pass. -/
@[inline] def Particle.smulThenAdd (s : ℝ) (a b : Particle) : Particle :=
  ⟨ CliffordVector.mk4
      (s * a.position.v0 + b.position.v0)
      (s * a.position.v1 + b.position.v1)
      (s * a.position.v2 + b.position.v2)
      (s * a.position.v3 + b.position.v3)
  , CliffordVector.mk4
      (s * a.momentum.v0 + b.momentum.v0)
      (s * a.momentum.v1 + b.momentum.v1)
      (s * a.momentum.v2 + b.momentum.v2)
      (s * a.momentum.v3 + b.momentum.v3)
  ⟩

/-- Fused weighted sum of four particles. -/
@[inline] def Particle.weightedSum4 (c1 c2 c3 c4 : ℝ) (p1 p2 p3 p4 : Particle) : Particle :=
  ⟨ CliffordVector.mk4
      (c1 * p1.position.v0 + c2 * p2.position.v0 + c3 * p3.position.v0 + c4 * p4.position.v0)
      (c1 * p1.position.v1 + c2 * p2.position.v1 + c3 * p3.position.v1 + c4 * p4.position.v1)
      (c1 * p1.position.v2 + c2 * p2.position.v2 + c3 * p3.position.v2 + c4 * p4.position.v2)
      (c1 * p1.position.v3 + c2 * p2.position.v3 + c3 * p3.position.v3 + c4 * p4.position.v3)
  , CliffordVector.mk4
      (c1 * p1.momentum.v0 + c2 * p2.momentum.v0 + c3 * p3.momentum.v0 + c4 * p4.momentum.v0)
      (c1 * p1.momentum.v1 + c2 * p2.momentum.v1 + c3 * p3.momentum.v1 + c4 * p4.momentum.v1)
      (c1 * p1.momentum.v2 + c2 * p2.momentum.v2 + c3 * p3.momentum.v2 + c4 * p4.momentum.v2)
      (c1 * p1.momentum.v3 + c2 * p2.momentum.v3 + c3 * p3.momentum.v3 + c4 * p4.momentum.v3)
  ⟩

/-- RK4 combination using fused weighted sum. -/
@[inline] def Particle.rk4Combine (dt : ℝ) (y k1 k2 k3 k4 : Particle) : Particle :=
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

end TetraGrayer.Util
