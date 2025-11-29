/-
Unified raytracer supporting multiple spacetimes and colormaps.

Ported from tetra-gray's raytracer.cuh with extensions.

Features:
- Parallel rendering using Lean 4's task system
- Chunked execution for optimal cache locality
- Support for flat, Kerr (Doran), and wormhole spacetimes
- Multiple colormaps: spherical, checkerboard, starfield, accretion disk
-/

import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Clifford
import TetraGrayer.Core.CoordSystems
import TetraGrayer.Core.Particle
import TetraGrayer.Camera
import TetraGrayer.Integrator.Generic
import TetraGrayer.Spacetimes.Doran
import TetraGrayer.Spacetimes.Wormhole
import TetraGrayer.Image.PPM
import TetraGrayer.Image.Colormap
import TetraGrayer.Image.AccretionDisk
import TetraGrayer.Render.Parallel

namespace TetraGrayer

open Core Integrator Spacetimes Image Render

/-- Parameters for Doran black hole raytracing. -/
structure DoranParams where
  /-- Black hole spin parameter a (0 = Schwarzschild, 1 = extremal Kerr) -/
  spinParam : ℝ := 0.5
  /-- Radius at which rays are considered escaped -/
  extractRadius : ℝ := 50.0
  /-- Maximum affine parameter before timeout -/
  maxParam : ℝ := 500.0
  /-- Maximum stepsize ratio for blueshift cutoff -/
  maxStepRatio : ℝ := 40.0
  /-- Maximum integration steps -/
  maxSteps : Nat := 100000
deriving Repr, Inhabited

namespace DoranParams
/-- Schwarzschild black hole (non-rotating). -/
def schwarzschild : DoranParams := { spinParam := 0.0 }
/-- Fast-spinning Kerr black hole. -/
def fastSpin : DoranParams := { spinParam := 0.9 }
/-- High resolution settings for final renders. -/
def highQuality : DoranParams := { maxSteps := 500000, maxParam := 1000.0 }
end DoranParams

/-- Parameters for flat spacetime raytracing. -/
structure FlatParams where
  /-- Radius at which rays are considered escaped -/
  extractRadius : ℝ := 50.0
  /-- Maximum affine parameter -/
  maxParam : ℝ := 200.0
  /-- Maximum integration steps -/
  maxSteps : Nat := 10000
deriving Repr, Inhabited

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

/-- Fused version of doranPixel for A/B benchmarking.

Uses the fused RK4 combination step to minimize intermediate Particle allocations.
-/
def doranPixelFused (cam : CameraParams) (params : DoranParams) (x y : Nat) : RGB :=
  let pixelIdx := y * cam.width + x
  let ode0 := pixelInitialData cam pixelIdx
  let odeF := integrateParticleAdaptiveFused
    doranRHS
    params.spinParam
    cam.dparam
    params.extractRadius
    params.maxParam
    params.maxStepRatio
    params.maxSteps
    ode0
  sphericalColormap params.extractRadius odeF

/-- Debug version with sharing checks. Prints if values are unexpectedly shared. -/
def doranPixelDebug (cam : CameraParams) (params : DoranParams) (x y : Nat) : RGB :=
  let pixelIdx := y * cam.width + x
  let ode0 := pixelInitialData cam pixelIdx
  let odeF := integrateParticleAdaptiveDebug
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

/-- Fused version of renderDoran for A/B benchmarking.

Uses fused RK4 combination to minimize intermediate allocations.
-/
def renderDoranFused (out : System.FilePath) (cam : CameraParams) (params : DoranParams)
    (numChunks : Nat := 8) : IO Unit := do
  IO.println s!"Rendering Doran (fused) to {out.toString}"
  IO.println s!"  Size: {cam.width}x{cam.height}, spin a={params.spinParam}"
  IO.println s!"  Parallel chunks: {numChunks}"

  let cfg : RenderConfig := {
    width := cam.width
    height := cam.height
    numChunks := numChunks
    showProgress := false
  }

  renderParallel out cfg (doranPixelFused cam params)
  IO.println "Done."

/-- Debug render: single pixel with sharing checks.

Renders just one pixel to check for unexpected sharing in the integration loop.
If no output appears, values are being used linearly (good!).
If "shared" messages appear, there's unexpected refcounting overhead.
-/
def renderDoranDebugSinglePixel (cam : CameraParams) (params : DoranParams) : IO Unit := do
  IO.println "=== Debug: Checking for unexpected sharing ==="
  IO.println "Running single pixel integration with dbgTraceIfShared..."
  let _ := doranPixelDebug cam params (cam.width / 2) (cam.height / 2)
  IO.println "Done. If no 'shared' messages appeared above, values are linear."

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

-- ============================================================================
-- Checkerboard renders (shows gravitational lensing dramatically)
-- ============================================================================

/-- Doran black hole with checkerboard background. -/
def doranCheckerboardPixel (cam : CameraParams) (params : DoranParams) (x y : Nat) : RGB :=
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
  checkerboardColormap params.extractRadius 12.0 RGB.white (RGB.ofNat 40 40 40) odeF

/-- Render Doran black hole with checkerboard background. -/
def renderDoranCheckerboard (out : System.FilePath) (cam : CameraParams) (params : DoranParams)
    (numChunks : Nat := 8) : IO Unit := do
  IO.println s!"Rendering Doran with checkerboard to {out.toString}"
  IO.println s!"  Size: {cam.width}x{cam.height}, spin a={params.spinParam}"

  let cfg : RenderConfig := {
    width := cam.width, height := cam.height, numChunks := numChunks, showProgress := false }

  renderParallel out cfg (doranCheckerboardPixel cam params)
  IO.println "Done."

-- ============================================================================
-- Starfield renders (realistic-looking background)
-- ============================================================================

/-- Doran black hole with starfield background. -/
def doranStarfieldPixel (cam : CameraParams) (params : DoranParams) (x y : Nat) : RGB :=
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
  starfieldColormap params.extractRadius 0.015 odeF

/-- Render Doran black hole with starfield background. -/
def renderDoranStarfield (out : System.FilePath) (cam : CameraParams) (params : DoranParams)
    (numChunks : Nat := 8) : IO Unit := do
  IO.println s!"Rendering Doran with starfield to {out.toString}"
  IO.println s!"  Size: {cam.width}x{cam.height}, spin a={params.spinParam}"

  let cfg : RenderConfig := {
    width := cam.width, height := cam.height, numChunks := numChunks, showProgress := false }

  renderParallel out cfg (doranStarfieldPixel cam params)
  IO.println "Done."

-- ============================================================================
-- Accretion disk renders
-- ============================================================================

/-- Doran black hole with accretion disk. -/
def doranDiskPixel (cam : CameraParams) (params : DoranParams) (disk : DiskParams)
    (x y : Nat) : RGB :=
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
  simpleDiskColormap disk params.extractRadius odeF

/-- Render Doran black hole with accretion disk. -/
def renderDoranWithDisk (out : System.FilePath) (cam : CameraParams) (params : DoranParams)
    (disk : DiskParams := DiskParams.schwarzschild) (numChunks : Nat := 8) : IO Unit := do
  IO.println s!"Rendering Doran with accretion disk to {out.toString}"
  IO.println s!"  Size: {cam.width}x{cam.height}, spin a={params.spinParam}"
  IO.println s!"  Disk: inner={disk.innerRadius}, outer={disk.outerRadius}"

  let cfg : RenderConfig := {
    width := cam.width, height := cam.height, numChunks := numChunks, showProgress := false }

  renderParallel out cfg (doranDiskPixel cam params disk)
  IO.println "Done."

-- ============================================================================
-- Wormhole renders
-- ============================================================================

/-- Ellis wormhole pixel renderer. -/
def wormholePixel (cam : CameraParams) (wh : WormholeParams) (maxSteps : Nat) (x y : Nat) : RGB :=
  let pixelIdx := y * cam.width + x
  let ode0 := pixelInitialData cam pixelIdx
  let odeF := integrateParticleSimple
    (fun p t => ellisRHSSimple p t wh.throatRadius)
    (fun d => escaped wh.extractRadius d || paramExceeded 300.0 d)
    maxSteps
    ode0
  -- Use checkerboard to show wormhole distortion
  checkerboardColormap wh.extractRadius 10.0 (RGB.ofNat 255 200 100) (RGB.ofNat 100 50 150) odeF

/-- Render Ellis wormhole. -/
def renderWormhole (out : System.FilePath) (cam : CameraParams)
    (wh : WormholeParams := WormholeParams.standard)
    (numChunks : Nat := 8) : IO Unit := do
  IO.println s!"Rendering Ellis wormhole to {out.toString}"
  IO.println s!"  Size: {cam.width}x{cam.height}, throat radius={wh.throatRadius}"

  let cfg : RenderConfig := {
    width := cam.width, height := cam.height, numChunks := numChunks, showProgress := false }

  renderParallel out cfg (wormholePixel cam wh 20000)
  IO.println "Done."

-- ============================================================================
-- Demo renders
-- ============================================================================

/-- Render all demo images. -/
def renderAllDemos : IO Unit := do
  let cam := { CameraParams.default 640 360 with
    position := CliffordVector.mk4 0.0 25.0 0.0 5.0  -- slightly above equatorial plane
    dparam := 0.05 }

  let params := DoranParams.mk 0.7 50.0 500.0 40.0 80000

  IO.println "=== TetraGrayer Demo Renders ==="
  IO.println ""

  -- Standard quadrant colormap
  IO.println "[1/5] Standard Doran render..."
  renderDoran "artifacts/demo-doran-standard.ppm" cam params

  -- Checkerboard shows lensing
  IO.println "[2/5] Checkerboard background..."
  renderDoranCheckerboard "artifacts/demo-doran-checker.ppm" cam params

  -- Starfield looks realistic
  IO.println "[3/5] Starfield background..."
  renderDoranStarfield "artifacts/demo-doran-stars.ppm" cam params

  -- Accretion disk
  IO.println "[4/5] Accretion disk..."
  let disk := { DiskParams.schwarzschild with innerRadius := 4.0, outerRadius := 18.0 }
  renderDoranWithDisk "artifacts/demo-doran-disk.ppm" cam params disk

  -- Wormhole
  IO.println "[5/5] Ellis wormhole..."
  let whCam := { cam with position := CliffordVector.mk4 0.0 15.0 0.0 0.0 }
  renderWormhole "artifacts/demo-wormhole.ppm" whCam

  IO.println ""
  IO.println "All demos complete! Check artifacts/ directory."

end TetraGrayer
