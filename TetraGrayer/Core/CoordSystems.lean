/-
Coordinate system transformations: Cartesian ↔ Spherical ↔ Spheroidal.

Ported from tetra-gray's coord_systems.cuh.

Note: Our spheroidal coordinates differ from Wikipedia's oblate spheroidal.
Our angle nu runs [0,π] like spherical theta.
-/

import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Clifford

namespace TetraGrayer
namespace Core

/-- Copy sign: returns |x| with the sign of y. -/
@[inline] def copysign (x y : ℝ) : ℝ :=
  let absX := Float.abs x
  if y >= 0.0 then absX else -absX

/-- Maximum of two floats. -/
@[inline] def fmax (x y : ℝ) : ℝ := if x >= y then x else y

/-- Convert Cartesian position (t,x,y,z) to spherical (t,r,θ,φ). -/
@[inline] def sphericalFromCartesian (pos : CliffordVector) : CliffordVector :=
  let t := pos.v0
  let x := pos.v1
  let y := pos.v2
  let z := pos.v3
  let r := Float.sqrt (x*x + y*y + z*z)
  let theta := if r > 0.0 then Float.acos (z / r) else 0.0
  let phi := Float.atan2 y x
  CliffordVector.mk4 t r theta phi

/-- Convert Cartesian position to oblate spheroidal coordinates (t, μ, ν, φ). -/
@[inline] def spheroidalFromCartesian (a : ℝ) (pos : CliffordVector) : CliffordVector :=
  let t := pos.v0
  let x := pos.v1
  let y := pos.v2
  let z := pos.v3
  let phi := Float.atan2 y x
  let rho := Float.sqrt (x*x + y*y)
  let d1 := Float.sqrt ((rho + a) * (rho + a) + z*z)
  let d2 := Float.sqrt ((rho - a) * (rho - a) + z*z)
  let coshMu := (d1 + d2) / (2.0 * a)
  let mu := Float.acosh coshMu
  let cos2Nu := 1.0 - (d1 - d2) * (d1 - d2) / (4.0 * a * a)
  let cosNuSign := copysign (Float.sqrt (fmax cos2Nu 0.0)) z
  let nu := Float.acos cosNuSign
  CliffordVector.mk4 t mu nu phi

/-- Spheroidal basis vector (mu direction) in Cartesian components. -/
@[inline] def spheroidalBasisEmu (sinhMu coshMu sinNu cosNu sinPhi cosPhi : ℝ) : CliffordVector :=
  let denom := Float.sqrt (sinhMu * sinhMu + cosNu * cosNu)
  let invD := 1.0 / denom
  CliffordVector.mk4 0.0 (sinhMu * sinNu * cosPhi * invD) (sinhMu * sinNu * sinPhi * invD) (coshMu * cosNu * invD)

/-- Spheroidal basis vector (nu direction) in Cartesian components. -/
@[inline] def spheroidalBasisEnu (sinhMu coshMu sinNu cosNu sinPhi cosPhi : ℝ) : CliffordVector :=
  let denom := Float.sqrt (sinhMu * sinhMu + cosNu * cosNu)
  let invD := 1.0 / denom
  CliffordVector.mk4 0.0 (coshMu * cosNu * cosPhi * invD) (coshMu * cosNu * sinPhi * invD) (-sinhMu * sinNu * invD)

/-- Spheroidal basis vector (phi direction) in Cartesian components. -/
@[inline] def spheroidalBasisPhi (sinPhi cosPhi : ℝ) : CliffordVector :=
  CliffordVector.mk4 0.0 (-sinPhi) cosPhi 0.0

/-- Spheroidal basis vector (time direction). -/
@[inline] def spheroidalBasisT : CliffordVector :=
  CliffordVector.mk4 1.0 0.0 0.0 0.0

end Core
end TetraGrayer
