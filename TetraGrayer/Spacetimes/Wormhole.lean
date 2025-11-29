/-
Ellis wormhole (Ellis drainhole) - a traversable wormhole spacetime.

The Ellis wormhole has the metric:
  ds² = -dt² + dl² + (l² + a²)(dθ² + sin²θ dφ²)

where:
- l is proper radial distance (can be negative, goes through throat)
- a is the throat radius
- θ, φ are standard spherical angles

This creates a "tunnel" connecting two asymptotically flat regions.
Light can travel through the throat and emerge on the other side.

Reference: H.G. Ellis, "Ether flow through a drainhole" (1973)
-/

import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Clifford
import TetraGrayer.Core.CoordSystems
import TetraGrayer.Core.Particle

namespace TetraGrayer
namespace Spacetimes

open Core

/-- Wormhole parameters. -/
structure WormholeParams where
  /-- Throat radius (minimum radius) -/
  throatRadius : ℝ := 1.0
  /-- Extraction sphere radius -/
  extractRadius : ℝ := 50.0
deriving Repr, Inhabited

namespace WormholeParams

/-- Standard wormhole with throat radius 1. -/
def standard : WormholeParams := { throatRadius := 1.0 }

/-- Wide wormhole with larger throat. -/
def wide : WormholeParams := { throatRadius := 3.0 }

/-- Narrow wormhole with small throat. -/
def narrow : WormholeParams := { throatRadius := 0.5 }

end WormholeParams

/-- Convert Cartesian position to Ellis coordinates (l, θ, φ).

The l coordinate is defined such that r² = l² + a² where r is the
standard Schwarzschild-like radial coordinate.
-/
def ellisFromCartesian (a : ℝ) (pos : CliffordVector) : CliffordVector :=
  let x := pos.v1
  let y := pos.v2
  let z := pos.v3
  let rSquared := x * x + y * y + z * z
  let r := Float.sqrt rSquared

  -- l² = r² - a², but we need to handle the sign
  -- l > 0 on one side of throat, l < 0 on the other
  -- Use z sign to determine which "universe"
  let lSquared := rSquared - a * a
  let lAbs := Float.sqrt (max 0.0 lSquared)
  let l := if z >= 0 then lAbs else -lAbs

  let theta := Float.acos (z / max r 1e-10)
  let phi := Float.atan2 y x

  CliffordVector.mk4 pos.v0 l theta phi

/-- Compute the effective radius r = sqrt(l² + a²) at a point. -/
def ellisEffectiveRadius (a l : ℝ) : ℝ :=
  Float.sqrt (l * l + a * a)

/-- Ellis wormhole geodesic RHS.

The geodesic equation involves second derivatives of l, theta, phi
with coupling through the throat geometry factor l/(l^2 + a^2).

Parameters:
- data: current particle state
- param: affine parameter (unused in RHS)
- a: throat radius

Returns derivative of particle state.
-/
def ellisRHS (data : Particle) (_ : ℝ) (a : ℝ) : Particle :=
  let pos := data.position
  let mom := data.momentum

  -- Current Cartesian position
  let x := pos.v1
  let y := pos.v2
  let z := pos.v3
  let rSq := x * x + y * y + z * z
  let r := Float.sqrt rSq

  -- Effective coordinate quantities
  let rho := Float.sqrt (x * x + y * y)  -- cylindrical radius
  let lSq := rSq - a * a
  let lAbs := Float.sqrt (max 0.0 lSq)
  let l := if z >= 0 then lAbs else -lAbs
  let rEff := ellisEffectiveRadius a l  -- sqrt(l² + a²)

  -- Momentum components
  let px := mom.v1
  let py := mom.v2
  let pz := mom.v3

  -- In flat Ellis coordinates, position changes with momentum directly
  -- dx/dλ = p (position gauge is identity for Ellis in Cartesian approx)
  let posRHS := mom

  -- Geodesic acceleration terms
  -- The Christoffel symbols for Ellis metric give curvature near throat
  if rEff < 1e-10 then
    -- At throat center, avoid division by zero
    Particle.ofPosMom posRHS CliffordVector.zero
  else
    -- Radial component of momentum
    let pr := (x * px + y * py + z * pz) / max r 1e-10

    -- Tangential components
    let pTheta := (z * (x * px + y * py) - rho * rho * pz) / (r * rho * r + 1e-10)
    let pPhi := (-y * px + x * py) / (rho * rho + 1e-10)

    -- Geodesic equation terms
    -- Key term: l/(l² + a²) * [radial vs tangential momentum balance]
    let factor := l / (rEff * rEff)

    -- For light: the throat acts as a lens
    -- Acceleration toward/away from throat axis
    let tangentialSq := pTheta * pTheta + Float.sin (Float.acos (z / max r 1e-10)) ^ 2 * pPhi * pPhi

    -- Simplified geodesic acceleration
    let accelFactor := factor * (pr * pr - tangentialSq * rEff * rEff)

    -- Convert back to Cartesian acceleration
    let ax := -accelFactor * x / max r 1e-10
    let ay := -accelFactor * y / max r 1e-10
    let az := -accelFactor * z / max r 1e-10

    let momRHS := CliffordVector.mk4 0.0 ax ay az

    Particle.ofPosMom posRHS momRHS

/-- Simplified Ellis wormhole RHS using embedding approach.

The Ellis metric can be thought of as a surface of revolution embedded
in higher-dimensional flat space. Light travels on geodesics of this surface.

This simplified version captures the essential lensing effect without
full geodesic integration.
-/
def ellisRHSSimple (data : Particle) (_ : ℝ) (a : ℝ) : Particle :=
  let pos := data.position
  let mom := data.momentum

  let x := pos.v1
  let y := pos.v2
  let z := pos.v3
  let rSq := x * x + y * y + z * z
  let _r := Float.sqrt rSq  -- Used for debugging

  -- Position changes with momentum
  let posRHS := mom

  -- Near the throat, light bends toward the axis
  -- The strength depends on how close to throat minimum
  let rEff := Float.sqrt (max (rSq - a * a) (a * a * 0.01))

  -- Bending factor: stronger near throat
  let bendStrength := a * a / (rEff * rEff * rEff + a * a * a)

  -- Bend toward throat axis (z-axis in our convention)
  -- Actually bend in the radial direction toward throat
  let rhoSq := x * x + y * y
  let rho := Float.sqrt rhoSq

  -- Radial momentum component
  let pRho := (x * mom.v1 + y * mom.v2) / max rho 1e-10

  -- Azimuthal acceleration (conservation of angular momentum modified by curvature)
  let ax := -bendStrength * x * (1.0 - pRho * pRho)
  let ay := -bendStrength * y * (1.0 - pRho * pRho)
  let az := 0.0  -- z-momentum conserved in this simplified model

  let momRHS := CliffordVector.mk4 0.0 ax ay az

  Particle.ofPosMom posRHS momRHS

/-- Check if particle has passed through the wormhole throat.

Returns true if particle has reached the "other side" (z changed sign
while r was small enough to be near throat).
-/
def passedThroughThroat (currentPos prevPos : CliffordVector) (a : ℝ) : Bool :=
  let zNow := currentPos.v3
  let zPrev := prevPos.v3
  let rNow := Float.sqrt (currentPos.v1^2 + currentPos.v2^2 + currentPos.v3^2)

  -- Check if z changed sign while near throat
  zNow * zPrev < 0.0 && rNow < a * 3.0

end Spacetimes
end TetraGrayer
