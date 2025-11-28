/-
Unified raytracer supporting flat and Doran (Kerr) spacetimes.

Ported from tetra-gray's raytracer.cuh.
-/

import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Clifford
import TetraGrayer.Core.CoordSystems
import TetraGrayer.Core.Particle
import TetraGrayer.Camera2
import TetraGrayer.Integrator.Generic
import TetraGrayer.Spacetimes.Doran
import TetraGrayer.Image.PPM
import TetraGrayer.Image.Colormap2

namespace TetraGrayer

open Core Integrator Spacetimes Image

/-- Parameters for Doran black hole raytracing. -/
structure DoranParams where
  spinParam : ℝ := 0.5      -- black hole spin a (0 = Schwarzschild)
  extractRadius : ℝ := 50.0 -- radius at which rays are considered escaped
  maxParam : ℝ := 500.0     -- max affine parameter before timeout
  maxStepRatio : ℝ := 40.0  -- max stepsize ratio for blueshift cutoff
  maxSteps : Nat := 100000  -- max integration steps
deriving Repr

/-- Parameters for flat spacetime raytracing. -/
structure FlatParams where
  extractRadius : ℝ := 50.0
  maxParam : ℝ := 200.0
  maxSteps : Nat := 10000
deriving Repr

/-- Render a Doran (Kerr) black hole image.

This is the main entry point for black hole visualization.
-/
def renderDoran (out : System.FilePath) (cam : CameraParams) (params : DoranParams) : IO Unit := do
  let w := cam.width
  let h := cam.height

  IO.println s!"Rendering Doran black hole to {out.toString}"
  IO.println s!"  Size: {w}x{h}, spin a={params.spinParam}"
  IO.println s!"  Extract radius: {params.extractRadius}, max param: {params.maxParam}"

  let pixel := fun (x y : Nat) =>
    let pixelIdx := y * w + x
    let ode0 := pixelInitialData cam pixelIdx

    -- Integrate with adaptive stepsize
    let odeF := integrateParticleAdaptive
      doranRHS
      params.spinParam
      cam.dparam
      params.extractRadius
      params.maxParam
      params.maxStepRatio
      params.maxSteps
      ode0

    sphericalColormap params.extractRadius odeF

  writePPM out w h pixel
  IO.println "Done."

/-- Render a flat spacetime image for testing/comparison.

Uses the new Clifford-based camera with proper rotor orientation.
In flat spacetime, rays travel in straight lines but we still use
the spherical colormap based on final position.
-/
def renderFlat2 (out : System.FilePath) (cam : CameraParams) (params : FlatParams) : IO Unit := do
  let w := cam.width
  let h := cam.height

  IO.println s!"Rendering flat spacetime to {out.toString}"
  IO.println s!"  Size: {w}x{h}"

  let pixel := fun (x y : Nat) =>
    let pixelIdx := y * w + x
    let ode0 := pixelInitialData cam pixelIdx

    -- Integrate with simple fixed step
    let odeF := integrateParticleSimple
      flatRHS
      (fun d => escaped params.extractRadius d || paramExceeded params.maxParam d)
      params.maxSteps
      ode0

    -- Use spherical colormap based on final position (same as Doran)
    sphericalColormap params.extractRadius odeF

  writePPM out w h pixel
  IO.println "Done."

/-- Quick test render: small Doran image. -/
def testDoranSmall : IO Unit := do
  let cam := { CameraParams.default 320 180 with
    position := CliffordVector.mk4 0.0 20.0 0.0 0.0
    dparam := 0.05 }
  let params := DoranParams.mk 0.5 50.0 500.0 40.0 50000
  renderDoran "artifacts/doran-test.ppm" cam params

/-- Quick test render: small flat image. -/
def testFlatSmall : IO Unit := do
  let cam := { CameraParams.default 320 180 with
    position := CliffordVector.mk4 0.0 20.0 0.0 0.0
    dparam := 0.05 }
  let params := FlatParams.mk 50.0 200.0 10000
  renderFlat2 "artifacts/flat-test.ppm" cam params

end TetraGrayer
