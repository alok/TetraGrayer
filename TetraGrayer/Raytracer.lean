import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Vector
import TetraGrayer.Camera
import TetraGrayer.Integrator.RK4
import TetraGrayer.Termination
import TetraGrayer.Image.PPM
import TetraGrayer.Image.Colormap
import TetraGrayer.Spacetimes.Flat

/-! # Raytracer

Ties together camera, integrator, termination, and colormap to render a PPM.
-/

namespace TetraGrayer

open Core Integrator Termination Image Spacetimes

/-- Render a flat-spacetime image using the given camera and step/limits. -/
def renderFlat (out : System.FilePath) (cam : Camera) (h : ℝ) (lim : Limits) : IO Unit := do
  let w := cam.width; let hgt := cam.height
  let rhs := flatRHS
  let pixel := fun (x y : Nat) =>
    let ray := Camera.pixelRay cam x y
    let s0 : State := { pos := ray.pos, mom := ray.mom, param := 0.0 }
    let sF := integrateUntil rhs h lim s0
    sphericalColor sF.mom
  writePPM out w hgt pixel

end TetraGrayer
