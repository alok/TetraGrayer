import TetraGrayer.Camera
import TetraGrayer.Raytracer
import TetraGrayer.Core.Clifford
import TetraGrayer.Render.Metal

open TetraGrayer
open Core Render

/-- Render full-resolution flat spacetime image matching upstream flat.cu -/
def renderFlatFull : IO Unit := do
  let cam := { CameraParams.default 1280 720 with
    position := CliffordVector.mk4 0.0 20.0 0.0 0.0
    hFov := π / 2.0
    dparam := 0.05 }
  let params := FlatParams.mk 50.0 200.0 10000
  renderFlat "artifacts/flat-full.ppm" cam params

/-- Render full-resolution Doran image matching upstream doran.cu -/
def renderDoranFull : IO Unit := do
  let cam := { CameraParams.default 1280 720 with
    position := CliffordVector.mk4 0.0 20.0 0.0 0.0
    hFov := π / 2.0
    dparam := 0.05 }
  let params := DoranParams.mk 0.5 50.0 500.0 40.0 100000
  renderDoran "artifacts/doran-full.ppm" cam params 720  -- One chunk per row for maximum load balancing

/-- Render using Metal GPU for maximum performance. -/
def renderMetalFull : IO Unit := do
  let camPos := CliffordVector.mk4 0.0 20.0 0.0 0.0
  Render.renderDoranMetal "artifacts/doran-metal.ppm"
    1280 720        -- width, height
    0.5             -- spinParam
    50.0            -- extractRadius
    500.0           -- maxParam
    40.0            -- maxStepRatio
    100000          -- maxSteps
    0.05            -- dparam0
    camPos
    (π / 2.0)       -- hFov

/-- Benchmark comparing standard vs fused RK4 -/
def benchmarkFusion : IO Unit := do
  let cam := { CameraParams.default 1280 720 with
    position := CliffordVector.mk4 0.0 20.0 0.0 0.0
    hFov := π / 2.0
    dparam := 0.05 }
  let params := DoranParams.mk 0.5 50.0 500.0 40.0 100000

  IO.println "=== Loop Fusion Benchmark ==="
  IO.println ""
  IO.println "Running standard version..."
  let startStd ← IO.monoNanosNow
  renderDoran "artifacts/bench-standard.ppm" cam params
  let endStd ← IO.monoNanosNow
  let stdTime := (endStd - startStd).toFloat / 1e9

  IO.println ""
  IO.println "Running fused version..."
  let startFused ← IO.monoNanosNow
  renderDoranFused "artifacts/bench-fused.ppm" cam params
  let endFused ← IO.monoNanosNow
  let fusedTime := (endFused - startFused).toFloat / 1e9

  IO.println ""
  IO.println "=== Results ==="
  IO.println s!"  Standard: {stdTime} s"
  IO.println s!"  Fused:    {fusedTime} s"
  let speedup := stdTime / fusedTime
  IO.println s!"  Speedup:  {speedup}x"

/-- Main entry point: render both flat and Doran spacetime images.

Usage:
  .lake/build/bin/tetragrayer           -- full render suite
  .lake/build/bin/tetragrayer flat      -- flat only (for comparison test)
  .lake/build/bin/tetragrayer test      -- small test renders
  .lake/build/bin/tetragrayer demos     -- all demo renders (disk, checker, stars, wormhole)
  .lake/build/bin/tetragrayer bench     -- A/B benchmark: standard vs fused RK4
-/
def main (args : List String) : IO Unit := do
  -- Upstream flat.cu parity params:
  -- width=1280, height=720, hFOV=π/2, origin=(t=0,x=20,y=0,z=0)
  -- step=0.05, escape=50, maxParam=200

  IO.println "TetraGrayer: General Relativistic Ray Tracer"
  IO.println "============================================"

  let mode := args.head?.getD "all"

  match mode with
  | "flat" =>
    -- Just flat render for quick comparison test
    IO.println "\nRendering flat spacetime (1280x720)..."
    renderFlatFull
    IO.println "Done! Output: artifacts/flat-full.ppm"

  | "doran" =>
    -- Just Doran render
    IO.println "\nRendering Doran black hole (1280x720)..."
    renderDoranFull
    IO.println "Done! Output: artifacts/doran-full.ppm"

  | "test" =>
    -- Small test renders only
    IO.println "\n[1/2] Small flat test (320x180)..."
    testFlatSmall
    IO.println "\n[2/2] Small Doran test (320x180)..."
    testDoranSmall
    IO.println "\nTest renders complete!"

  | "demos" =>
    -- All demo renders with different colormaps/spacetimes
    IO.println "\nRendering all demo images..."
    renderAllDemos

  | "bench" =>
    -- A/B benchmark for loop fusion
    benchmarkFusion

  | "metal" =>
    -- Render using Metal GPU
    IO.println "\nRendering on Metal GPU (1280x720)..."
    renderMetalFull

  | "debug" =>
    -- Check for unexpected sharing in integration loop
    -- Use small maxSteps to avoid flooding output
    let cam := { CameraParams.default 1280 720 with
      position := CliffordVector.mk4 0.0 20.0 0.0 0.0
      hFov := π / 2.0
      dparam := 0.05 }
    let params := DoranParams.mk 0.5 50.0 500.0 40.0 10  -- Only 10 steps!
    renderDoranDebugSinglePixel cam params

  | _ =>
    -- Full render suite
    IO.println "\n[1/4] Full flat render (1280x720)..."
    renderFlatFull

    IO.println "\n[2/4] Small flat test (320x180)..."
    testFlatSmall

    IO.println "\n[3/4] Small Doran test (320x180)..."
    testDoranSmall

    IO.println "\n[4/4] Full Doran render (1280x720)..."
    renderDoranFull

    IO.println "\nAll renders complete!"
    IO.println "Output files:"
    IO.println "  artifacts/flat-full.ppm (1280x720, comparison with upstream)"
    IO.println "  artifacts/flat-test.ppm (320x180, flat spacetime)"
    IO.println "  artifacts/doran-test.ppm (320x180, quick test)"
    IO.println "  artifacts/doran-full.ppm (1280x720, full render)"
