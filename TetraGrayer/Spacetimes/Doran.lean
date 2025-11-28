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
def doranBeta (coshMu sinNu : ℝ) : ℝ :=
  Float.atanh (sinNu / coshMu)

/-- Compute Doran's velocity vector v.

v = t̂ cosh(β) + φ̂ sinh(β)

where t̂ is the time basis vector and φ̂ is the azimuthal basis vector.
-/
def doranVectorV (beta sinPhi cosPhi : ℝ) (that phihat : CliffordVector) : CliffordVector :=
  Float.cosh beta * that + Float.sinh beta * phihat

/-- Position gauge h: transforms vectors from tetrad frame to coordinate frame. -/
def doranPositionGauge (sinhMu : ℝ) (emuhat : CliffordVector) (a : ℝ)
    (doranV vecArg : CliffordVector) : CliffordVector :=
  let rootFactor := Float.sqrt (2.0 * sinhMu / a / (1.0 + sinhMu * sinhMu))
  let dotProduct := vectorDot vecArg doranV
  vecArg + rootFactor * dotProduct * emuhat

/-- Rotation gauge omega: returns the bivector-valued connection for geodesic equation. -/
def doranRotationGauge (sinhMu cosNu : ℝ)
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
def flatRHS (data : Particle) (_ : ℝ) : Particle :=
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
def doranRHS (data : Particle) (_ : ℝ) (a : ℝ) : Particle :=
  let spheroidalCoords := spheroidalFromCartesian a data.position

  let mu := spheroidalCoords.v1
  let nu := spheroidalCoords.v2
  let phi := spheroidalCoords.v3

  let sinhMu := Float.sinh mu
  let coshMu := Float.cosh mu
  let sinNu := Float.sin nu
  let cosNu := Float.cos nu
  let sinPhi := Float.sin phi
  let cosPhi := Float.cos phi

  -- Compute spheroidal basis vectors in Cartesian coords
  let muhat := spheroidalBasisEmu sinhMu coshMu sinNu cosNu sinPhi cosPhi
  let nuhat := spheroidalBasisEnu sinhMu coshMu sinNu cosNu sinPhi cosPhi
  let phihat := spheroidalBasisPhi sinPhi cosPhi
  let that := spheroidalBasisT

  let beta := doranBeta coshMu sinNu
  let doranV := doranVectorV beta sinPhi cosPhi that phihat

  -- Position derivative: x' = h(k)
  let posRHS := doranPositionGauge sinhMu muhat a doranV data.momentum

  -- Momentum derivative: k' = -ω(k)|k
  let omega := doranRotationGauge sinhMu cosNu muhat nuhat phihat that beta doranV a data.momentum
  let momRHS := CliffordVector.neg (bivectorDotVector omega data.momentum)

  Particle.ofPosMom posRHS momRHS

end Spacetimes
end TetraGrayer
