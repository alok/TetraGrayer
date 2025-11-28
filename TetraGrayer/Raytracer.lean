/-
Unified raytracer supporting flat and Doran (Kerr) spacetimes.

Ported from tetra-gray's raytracer.cuh.

Features:
- Parallel rendering using Lean 4's task system
- Chunked execution for optimal cache locality
- Support for both flat and curved spacetimes
-/

import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Clifford
import TetraGrayer.Core.CoordSystems
import TetraGrayer.Core.Particle
import TetraGrayer.Camera
import TetraGrayer.Integrator.Generic
import TetraGrayer.Spacetimes.Doran
import TetraGrayer.Image.PPM
import TetraGrayer.Image.Colormap
import TetraGrayer.Render.Parallel

namespace TetraGrayer

open Core Integrator Spacetimes Image Render

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

/-- Compute single pixel color for Doran spacetime. Pure function. -/
def doranPixel (cam : CameraParams) (params : DoranParams) (x y : Nat) : RGB :=
  let pixelIdx := y * cam.width + x
  let ode0 := pixelInitialData cam pixelIdx
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

/-- Compute single pixel color for flat spacetime. Pure function. -/
def flatPixel (cam : CameraParams) (params : FlatParams) (x y : Nat) : RGB :=
  let pixelIdx := y * cam.width + x
  let ode0 := pixelInitialData cam pixelIdx
  let odeF := integrateParticleSimple
    flatRHS
    (fun d => escaped params.extractRadius d || paramExceeded params.maxParam d)
    params.maxSteps
    ode0
  sphericalColormap params.extractRadius odeF

/-- Render a Doran (Kerr) black hole image in parallel.

This is the main entry point for black hole visualization.
Uses parallel chunk rendering for optimal performance.
-/
def renderDoran (out : System.FilePath) (cam : CameraParams) (params : DoranParams)
    (numChunks : Nat := 8) : IO Unit := do
  IO.println s!"Rendering Doran black hole to {out.toString}"
  IO.println s!"  Size: {cam.width}x{cam.height}, spin a={params.spinParam}"
  IO.println s!"  Extract radius: {params.extractRadius}, max param: {params.maxParam}"
  IO.println s!"  Parallel chunks: {numChunks}"

  let cfg : RenderConfig := {
    width := cam.width
    height := cam.height
    numChunks := numChunks
    showProgress := false
  }

  renderParallel out cfg (doranPixel cam params)
  IO.println "Done."

/-- Render a flat spacetime image in parallel.

Uses the Clifford-based camera with proper rotor orientation.
In flat spacetime, rays travel in straight lines but we still use
the spherical colormap based on final position.
-/
def renderFlat (out : System.FilePath) (cam : CameraParams) (params : FlatParams)
    (numChunks : Nat := 8) : IO Unit := do
  IO.println s!"Rendering flat spacetime to {out.toString}"
  IO.println s!"  Size: {cam.width}x{cam.height}"
  IO.println s!"  Parallel chunks: {numChunks}"

  let cfg : RenderConfig := {
    width := cam.width
    height := cam.height
    numChunks := numChunks
    showProgress := false
  }

  renderParallel out cfg (flatPixel cam params)
  IO.println "Done."

/-- Sequential (non-parallel) flat render for comparison/debugging. -/
def renderFlatSequential (out : System.FilePath) (cam : CameraParams) (params : FlatParams)
    : IO Unit := do
  IO.println s!"Rendering flat spacetime (sequential) to {out.toString}"
  IO.println s!"  Size: {cam.width}x{cam.height}"

  writePPM out cam.width cam.height (flatPixel cam params)
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
  renderFlat "artifacts/flat-test.ppm" cam params

end TetraGrayer
