import TetraGrayer.Camera
import TetraGrayer.Raytracer
import TetraGrayer.Core.Clifford

open TetraGrayer
open Core

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
  renderDoran "artifacts/doran-full.ppm" cam params

/-- Main entry point: render both flat and Doran spacetime images.

Usage:
  .lake/build/bin/tetragrayer           -- full render suite
  .lake/build/bin/tetragrayer flat      -- flat only (for comparison test)
  .lake/build/bin/tetragrayer test      -- small test renders
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
