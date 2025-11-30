import TetraGrayer.Camera
import TetraGrayer.Raytracer
import TetraGrayer.Core.Clifford
import TetraGrayer.Render.Metal
import TetraGrayer.Render.MetalFFI

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

/-- Render using idiomatic Metal FFI with dependent types. -/
def renderMetalIdiomatic : IO Unit := do
  IO.println "  Using idiomatic Lean FFI with dependent types..."
  IO.println s!"  Metal available: {Metal.isAvailable}"

  if !Metal.isAvailable then
    IO.eprintln "  Metal GPU not available on this system"
    return

  -- Configure with dependent types (spin and FOV are now constrained)
  let config : Metal.RenderConfig := {
    dims := Metal.Dims.hd  -- Statically 1280x720 with proof of positivity
    doran := { Metal.DoranConfig.moderate with  -- spin = 0.5 (proven ∈ [0,1])
      fov := Metal.FOV.wide  -- 90° (proven ∈ (0,π))
      extractRadius := 50.0
      maxParam := 500.0
      maxStepRatio := 40.0
      maxSteps := 100000
      dparam0 := 0.05
    }
    camera := { Metal.CameraPos.atDistance 20.0 with t := 0.0 }
  }

  IO.println s!"  Resolution: {config.dims.width}x{config.dims.height}"
  IO.println s!"  Total pixels: {config.dims.pixels}"
  IO.println s!"  Spin parameter: {config.doran.spin.val} (proven 0 ≤ a ≤ 1)"
  IO.println s!"  FOV: {config.doran.fov.val} rad (proven 0 < fov < π)"

  -- Render with full type safety
  let start ← IO.monoNanosNow
  let result ← Metal.render config
  let elapsed ← IO.monoNanosNow
  let duration := (elapsed - start).toFloat / 1e9

  match result with
  | .ok img =>
    -- img.data.size = 4 * img.dims.pixels is proven at type level!
    IO.println s!"  Rendered in {duration} s"
    IO.println s!"  Image buffer: {img.data.size} bytes (verified: 4 * {img.dims.pixels})"
    img.writePPM "artifacts/doran-metal-ffi.ppm"
    IO.println "  Output: artifacts/doran-metal-ffi.ppm"
  | .unavailable =>
    IO.eprintln "  Metal unavailable"
  | .initFailed code =>
    IO.eprintln s!"  Metal init failed with code {code}"
  | .renderFailed code =>
    IO.eprintln s!"  Metal render failed with code {code}"

/-- Benchmark comparing CPU vs GPU rendering. -/
def benchmarkCpuGpu : IO Unit := do
  IO.println "=== CPU vs GPU Benchmark ==="
  IO.println ""

  let cam := { CameraParams.default 640 360 with
    position := CliffordVector.mk4 0.0 20.0 0.0 0.0
    hFov := π / 2.0
    dparam := 0.05 }
  let params := DoranParams.mk 0.5 50.0 500.0 40.0 50000

  -- CPU Render
  IO.println "Running CPU render (640x360)..."
  let startCpu ← IO.monoNanosNow
  renderDoran "artifacts/bench-cpu.ppm" cam params
  let endCpu ← IO.monoNanosNow
  let cpuTime := (endCpu - startCpu).toFloat / 1e9

  -- GPU Render
  IO.println "Running GPU render (640x360)..."
  let config : Metal.RenderConfig := {
    dims := { width := 640, height := 360, width_pos := by decide, height_pos := by decide }
    doran := { Metal.DoranConfig.moderate with
      extractRadius := 50.0
      maxParam := 500.0
      maxStepRatio := 40.0
      maxSteps := 50000
      dparam0 := 0.05
    }
    camera := { Metal.CameraPos.atDistance 20.0 with t := 0.0 }
  }

  let startGpu ← IO.monoNanosNow
  let result ← Metal.render config
  let endGpu ← IO.monoNanosNow
  let gpuTime := (endGpu - startGpu).toFloat / 1e9

  match result with
  | .ok img =>
    img.writePPM "artifacts/bench-gpu.ppm"
  | _ =>
    IO.eprintln "  GPU render failed"

  IO.println ""
  IO.println "=== Results ==="
  IO.println s!"  CPU: {cpuTime} s"
  IO.println s!"  GPU: {gpuTime} s"
  let speedup := cpuTime / gpuTime
  IO.println s!"  Speedup: {speedup}x"

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

  | "cpu-gpu" =>
    -- CPU vs GPU benchmark
    benchmarkCpuGpu

  | "metal" =>
    -- Render using Metal GPU
    IO.println "\nRendering on Metal GPU (1280x720)..."
    renderMetalFull

  | "metal-ffi" =>
    -- Render using idiomatic Metal FFI with dependent types
    IO.println "\nRendering on Metal GPU with idiomatic Lean FFI..."
    renderMetalIdiomatic

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
