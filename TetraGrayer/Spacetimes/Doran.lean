/-
Doran metric for spinning (Kerr) black holes using tetrad/Clifford algebra formulation.

Ported from tetra-gray's rhs.cuh.

These equations are detailed in section 14.7.3 of Doran and Lasenby's
"Geometric Algebra for Physicists".

The geodesic equation in Cartesian coordinates:
  x' = h(k)       (position gauge)
  k' = -ω(k)|k    (rotation gauge, no derivatives of basis vectors)
-/

import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Clifford
import TetraGrayer.Core.CoordSystems
import TetraGrayer.Core.Particle

namespace TetraGrayer
namespace Spacetimes

open Core

-- ============================================================================
-- Doran metric helper functions
-- ============================================================================

/-- Compute Doran's β angle from spheroidal coordinates.

β = atanh(sin(ν) / cosh(μ))
-/
@[inline] def doranBeta (coshMu sinNu : ℝ) : ℝ :=
  Float.atanh (sinNu / coshMu)

/-- Compute Doran's velocity vector v.

v = t̂ cosh(β) + φ̂ sinh(β)

where t̂ is the time basis vector and φ̂ is the azimuthal basis vector.
-/
@[inline] def doranVectorV (beta _sinPhi _cosPhi : ℝ) (that phihat : CliffordVector) : CliffordVector :=
  Float.cosh beta * that + Float.sinh beta * phihat

/-- Position gauge h: transforms vectors from tetrad frame to coordinate frame. -/
@[inline] def doranPositionGauge (sinhMu : ℝ) (emuhat : CliffordVector) (a : ℝ)
    (doranV vecArg : CliffordVector) : CliffordVector :=
  let rootFactor := Float.sqrt (2.0 * sinhMu / a / (1.0 + sinhMu * sinhMu))
  let dotProduct := vectorDot vecArg doranV
  vecArg + rootFactor * dotProduct * emuhat

/-- Rotation gauge omega: returns the bivector-valued connection for geodesic equation. -/
@[inline] def doranRotationGauge (sinhMu cosNu : ℝ)
    (muhat nuhat phihat that : CliffordVector)
    (beta : ℝ) (doranV : CliffordVector) (a : ℝ)
    (vecArg : CliffordVector) : Bivector :=
  let alpha := -Float.sqrt (2.0 * sinhMu / (a * (sinhMu * sinhMu + cosNu * cosNu)))

  let argDotMu := vectorDot vecArg muhat
  let argDotNu := vectorDot vecArg nuhat
  let argDotPhi := vectorDot vecArg phihat

  -- μ̂∧v term
  let muterm := vectorWedge muhat doranV
  -- ν̂∧v term
  let nuterm := vectorWedge nuhat doranV
  -- φ̂∧t̂ term
  let phiterm := vectorWedge phihat that

  -- Complex denominator factors
  let commonScalar := a * sinhMu
  let commonPseudo := a * cosNu
  let commonDenom := commonScalar * commonScalar + commonPseudo * commonPseudo

  -- μ term coefficients (double denominator)
  let muScalar := (commonScalar * commonScalar - commonPseudo * commonPseudo) /
                  (commonDenom * commonDenom)
  let muPseudo := (2.0 * commonScalar * commonPseudo) /
                  (commonDenom * commonDenom)

  -- Combine terms with appropriate scaling
  let muContrib :=
    let scaled := muScalar * muterm
    let dual := muPseudo * bivectorDual muterm
    (1.0 / alpha) * argDotMu / (commonDenom * commonDenom) * (scaled + dual)

  let nuContrib :=
    let scaled := commonScalar * nuterm
    let dual := commonPseudo * bivectorDual nuterm
    (-alpha) * argDotNu / commonDenom * (scaled + dual)

  let phiContrib :=
    let scaled := commonScalar * phiterm
    let dual := commonPseudo * bivectorDual phiterm
    (-alpha / Float.cosh beta) * argDotPhi / commonDenom * (scaled + dual)

  muContrib + nuContrib + phiContrib

-- ============================================================================
-- Flat spacetime RHS (trivial case for testing)
-- ============================================================================

/-- Flat spacetime RHS: dx/dλ = p, dp/dλ = 0. -/
@[inline] def flatRHS (data : Particle) (_ : ℝ) : Particle :=
  Particle.ofPosMom data.momentum CliffordVector.zero

-- ============================================================================
-- Doran (Kerr) spacetime RHS
-- ============================================================================

/-- Doran spacetime RHS for spinning black hole geodesics.

Parameters:
- data: current particle state (position, momentum)
- affineParam: current affine parameter value (unused in RHS)
- a: scale factor (= spin parameter)

Returns derivative of particle state.
-/
@[inline] def doranRHS (data : Particle) (_ : ℝ) (a : ℝ) : Particle :=
  let coords := spheroidalFromCartesian a data.position
  let μ := coords.v1
  let ν := coords.v2
  let φ := coords.v3

  -- Combined trig calls (6 → 3 calls in theory, but still 6 under the hood)
  let (sinhμ, coshμ) := sinhcosh μ
  let (sinν, cosν) := sincos ν
  let (sinφ, cosφ) := sincos φ

  -- Spheroidal basis vectors
  let ê_μ := spheroidalBasisEmu sinhμ coshμ sinν cosν sinφ cosφ
  let ê_ν := spheroidalBasisEnu sinhμ coshμ sinν cosν sinφ cosφ
  let ê_φ := spheroidalBasisPhi sinφ cosφ
  let ê_t := spheroidalBasisT

  let β := doranBeta coshμ sinν
  let V := doranVectorV β sinφ cosφ ê_t ê_φ

  -- x' = h(k), k' = -ω(k)|k
  let x' := doranPositionGauge sinhμ ê_μ a V data.momentum
  let ω := doranRotationGauge sinhμ cosν ê_μ ê_ν ê_φ ê_t β V a data.momentum
  let k' := CliffordVector.neg (bivectorDotVector ω data.momentum)

  ⟨x', k'⟩

end Spacetimes
end TetraGrayer
