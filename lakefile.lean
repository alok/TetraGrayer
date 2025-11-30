import Lake
open System Lake DSL

package «TetraGrayer» where
  version := v!"0.1.0"
  leanOptions := #[
    ⟨`pp.unicode.fun, true⟩,
    ⟨`autoImplicit, true⟩,
    ⟨`relaxedAutoImplicit, false⟩,
    ⟨`linter.missingDocs, true⟩,
    ⟨`doc.verso, true⟩
  ]
  -- Release build: enables C compiler optimizations
  precompileModules := true
  buildType := .release
  -- C compiler optimization flags (passed to leanc -> clang/gcc)
  -- -march=native enables CPU-specific optimizations (AVX, etc.)
  -- -flto enables link-time optimization for cross-module inlining
  moreLeancArgs := #["-O3", "-ffast-math", "-march=native", "-funroll-loops"]
  -- Link with Metal dylib for GPU acceleration
  -- The dylib is built separately to avoid Lean sysroot conflicts with macOS frameworks
  -- -rpath sets the runtime library search path to find libmetal_raytracer.dylib
  moreLinkArgs := #[
    "-O3", "-ffast-math", "-march=native", "-flto",
    "-L./ffi/metal",
    "-lmetal_raytracer",
    "-Wl,-rpath,@executable_path/../../../ffi/metal",
    "-Wl,-rpath,./ffi/metal"
  ]

-- Verso docs are in verso-docs/ subdirectory with its own toolchain
-- (VersoManual has build issues with data files, VersoBlog works)

-- ProofWidgets disabled: version incompatibility with Lean 4.25
-- require proofwidgets from git
--   "https://github.com/leanprover-community/ProofWidgets4" @ "v0.0.52"

-- FFI C library for fast trig
target ffi.o pkg : FilePath := do
  let oFile := pkg.buildDir / "c" / "trig.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "trig.c"
  let weakArgs := #["-I", (← getLeanIncludeDir).toString, "-O3", "-ffast-math"]
  buildO oFile srcJob weakArgs #["-fPIC"] "cc" getLeanTrace

-- Metal FFI wrapper (links against prebuilt Metal dylib)
-- Note: The Metal dylib (libmetal_raytracer.dylib) must be built first with:
--   make -C ffi/metal
-- This is required because Lean's sysroot conflicts with macOS framework linking.
target metal_ffi.o pkg : FilePath := do
  let oFile := pkg.buildDir / "c" / "metal_ffi.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "metal" / "lean_metal_ffi.c"
  let metalDir := pkg.dir / "ffi" / "metal"
  let weakArgs := #[
    "-I", (← getLeanIncludeDir).toString,
    "-I", metalDir.toString,
    "-O3", "-ffast-math"
  ]
  buildO oFile srcJob weakArgs #["-fPIC"] "cc" getLeanTrace

extern_lib libleanffi pkg := do
  let ffiO ← ffi.o.fetch
  let metalO ← metal_ffi.o.fetch
  let name := nameToStaticLib "leanffi"
  buildStaticLib (pkg.staticLibDir / name) #[ffiO, metalO]

@[default_target]
lean_lib TetraGrayer where
  globs := #[.andSubmodules `TetraGrayer]

lean_exe tetragrayer where
  root := `Main

/-- Run flat spacetime render and compare against upstream reference.

Usage: lake run test
-/
script test do
  -- Build first
  let build ← IO.Process.output {
    cmd := "lake"
    args := #["build"]
  }
  if build.exitCode != 0 then
    IO.eprintln s!"Build failed:\n{build.stderr}"
    return 1

  IO.println "Running flat spacetime render..."
  let render ← IO.Process.output {
    cmd := "./.lake/build/bin/tetragrayer"
    args := #["flat"]
  }
  IO.println render.stdout
  if render.exitCode != 0 then
    IO.eprintln s!"Render failed:\n{render.stderr}"
    return 1

  -- Check if reference image exists
  let refPath := "external/tetra-gray/images/flat.png"
  let refExists ← System.FilePath.pathExists refPath
  if !refExists then
    IO.eprintln s!"Reference image not found: {refPath}"
    IO.eprintln "Skipping comparison test."
    return 0

  -- Check if ImageMagick is available
  let magickCheck ← IO.Process.output {
    cmd := "which"
    args := #["magick"]
  }
  if magickCheck.exitCode != 0 then
    IO.eprintln "ImageMagick not found. Skipping pixel comparison."
    IO.eprintln "Install with: brew install imagemagick"
    return 0

  IO.println "Converting Lean output to PNG..."
  let convert ← IO.Process.output {
    cmd := "magick"
    args := #["artifacts/flat-full.ppm", "/tmp/lean-flat.png"]
  }
  if convert.exitCode != 0 then
    IO.eprintln s!"Conversion failed:\n{convert.stderr}"
    return 1

  IO.println "Comparing against upstream reference..."
  let compare ← IO.Process.output {
    cmd := "magick"
    args := #["compare", "-metric", "AE",
              "/tmp/lean-flat.png", refPath,
              "/tmp/diff.png"]
  }
  -- Note: ImageMagick returns exit code 1 if images differ at all
  -- The diff count is in stderr

  -- Parse the diff count (extract leading integer)
  let diffOutput := compare.stderr.trim
  let diffCount := diffOutput.takeWhile Char.isDigit
  let diffNum := diffCount.toNat!

  let totalPixels : Nat := 1280 * 720  -- 921600
  let maxAllowed : Nat := 1000  -- < 0.11% tolerance

  IO.println s!"Total pixels: {totalPixels}"
  IO.println s!"Differing pixels: {diffNum}"

  if diffNum == 0 then
    IO.println "\n✓ PASS: Images are bitwise identical!"
    return 0
  else if diffNum < maxAllowed then
    let pct := (diffNum.toFloat * 100.0) / totalPixels.toFloat
    IO.println s!"Difference: {pct}%"
    IO.println "\n✓ PASS: Images match within tolerance (< 0.11% difference)"
    IO.println "  (Minor differences due to floating-point rounding between GPU/CPU)"
    return 0
  else
    let pct := (diffNum.toFloat * 100.0) / totalPixels.toFloat
    IO.println s!"Difference: {pct}%"
    IO.println "\n✗ FAIL: Images differ significantly"
    IO.println "  Check /tmp/diff.png for visual diff"
    return 1

/-- Render all test images (small sizes for quick testing). -/
script render_test do
  let build ← IO.Process.output {
    cmd := "lake"
    args := #["build"]
  }
  if build.exitCode != 0 then
    IO.eprintln s!"Build failed:\n{build.stderr}"
    return 1

  IO.println "Running test renders..."
  let render ← IO.Process.spawn {
    cmd := "./.lake/build/bin/tetragrayer"
    args := #["test"]
    stdout := .inherit
    stderr := .inherit
  }
  let exitCode ← render.wait
  return exitCode.toNat.toUInt32

/-- Render full resolution flat and Doran images. -/
script render_full do
  let build ← IO.Process.output {
    cmd := "lake"
    args := #["build"]
  }
  if build.exitCode != 0 then
    IO.eprintln s!"Build failed:\n{build.stderr}"
    return 1

  IO.println "Running full renders..."
  let render ← IO.Process.spawn {
    cmd := "./.lake/build/bin/tetragrayer"
    args := #["all"]
    stdout := .inherit
    stderr := .inherit
  }
  let exitCode ← render.wait
  return exitCode.toNat.toUInt32

/-- Benchmark raytracing performance using hyperfine.

Usage: lake run bench [runs]
  runs: number of benchmark runs (default: 3)

Benchmarks a small 160x90 render for quick iteration.
-/
script bench (args) do
  -- Build first with release optimizations
  let build ← IO.Process.output {
    cmd := "lake"
    args := #["build"]
  }
  if build.exitCode != 0 then
    IO.eprintln s!"Build failed:\n{build.stderr}"
    return 1

  -- Check if hyperfine is available
  let hfCheck ← IO.Process.output {
    cmd := "which"
    args := #["hyperfine"]
  }
  if hfCheck.exitCode != 0 then
    IO.eprintln "hyperfine not found. Install with: cargo install hyperfine"
    return 1

  let runs := args.head?.getD "3"

  IO.println s!"Benchmarking small render ({runs} runs)..."
  IO.println "Build type: release, C flags: -O3 -ffast-math"
  IO.println ""

  let bench ← IO.Process.spawn {
    cmd := "hyperfine"
    args := #[
      "--warmup", "1",
      "--runs", runs,
      "--export-markdown", "benchmark.md",
      "./.lake/build/bin/tetragrayer test"
    ]
    stdout := .inherit
    stderr := .inherit
  }
  let exitCode ← bench.wait
  return exitCode.toNat.toUInt32

/-- Quick single-run timing for iteration.

Usage: lake run time [mode]
  mode: test, flat, doran (default: test)
-/
script time (args) do
  let build ← IO.Process.output {
    cmd := "lake"
    args := #["build"]
  }
  if build.exitCode != 0 then
    IO.eprintln s!"Build failed:\n{build.stderr}"
    return 1

  let mode := args.head?.getD "test"
  IO.println s!"Timing '{mode}' mode..."

  let start ← IO.monoMsNow
  let render ← IO.Process.output {
    cmd := "./.lake/build/bin/tetragrayer"
    args := #[mode]
  }
  let elapsed ← IO.monoMsNow
  let duration := elapsed - start

  IO.println render.stdout
  if render.exitCode != 0 then
    IO.eprintln s!"Render failed:\n{render.stderr}"
    return 1

  IO.println s!"\n⏱  Elapsed: {duration} ms ({duration.toFloat / 1000.0} s)"
  return 0
