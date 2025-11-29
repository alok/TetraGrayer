/-
Parallel rendering infrastructure using Lean 4's task system.

Provides chunked parallel execution for raytracing with configurable
parallelism and progress reporting.
-/

import TetraGrayer.Core.Scalar
import TetraGrayer.Image.PPM

namespace TetraGrayer
namespace Render

open Core Image

/-- Render configuration. -/
structure RenderConfig where
  /-- Image width in pixels. -/
  width : Nat
  /-- Image height in pixels. -/
  height : Nat
  /-- Number of parallel chunks (typically equals CPU cores). -/
  numChunks : Nat := 16
  /-- Show progress during rendering. -/
  showProgress : Bool := true
deriving Repr

/-- A chunk of rows to render. -/
structure Chunk where
  /-- First row index (inclusive). -/
  startRow : Nat
  /-- Last row index (exclusive). -/
  endRow : Nat
  /-- Image width. -/
  width : Nat
deriving Repr

/-- Divide the image into horizontal chunks for parallel processing. -/
def makeChunks (cfg : RenderConfig) : Array Chunk :=
  let h := cfg.height
  let n := min cfg.numChunks h  -- Can't have more chunks than rows
  let baseRows := h / n
  let extra := h % n
  Id.run do
    let mut chunks := #[]
    let mut row := 0
    for i in [0:n] do
      let thisChunkRows := baseRows + (if i < extra then 1 else 0)
      chunks := chunks.push { startRow := row, endRow := row + thisChunkRows, width := cfg.width }
      row := row + thisChunkRows
    return chunks

/-- Render a single chunk, returning an array of RGB values in row-major order.
The noinline attribute prevents optimizer from hoisting this out of task closures. -/
@[noinline] def renderChunk (chunk : Chunk) (pixel : Nat → Nat → RGB) : IO (Array RGB) := do
  let mut result := #[]
  for y in [chunk.startRow : chunk.endRow] do
    for x in [0 : chunk.width] do
      result := result.push (pixel x y)
  return result

/-- Render all chunks in parallel using IO.asTask.

Each chunk is rendered in a separate task with dedicated priority for parallelism.
Returns array of (chunkIndex, pixels) pairs.
-/
def renderChunksParallel (chunks : Array Chunk) (pixel : Nat → Nat → RGB)
    : IO (Array (Nat × Array RGB)) := do
  -- Spawn tasks for each chunk
  let tasks ← chunks.mapIdxM fun i chunk => do
    let idx : Nat := i
    IO.asTask (prio := .dedicated) do
      let pixels ← renderChunk chunk pixel
      return (idx, pixels)

  -- Wait for all tasks and collect results
  let mut results : Array (Nat × Array RGB) := #[]
  for task in tasks do
    let result ← IO.wait task
    match result with
    | .ok r => results := results.push r
    | .error e => throw (IO.Error.userError s!"Chunk rendering failed: {e}")
  return results

/-- Write pre-rendered pixels to PPM file. -/
def writePixelsPPM (path : System.FilePath) (w h : Nat) (pixels : Array RGB) : IO Unit := do
  let parent := path.parent
  match parent with
  | some dir => IO.FS.createDirAll dir
  | none => pure ()

  -- Use getD for safe access with default black pixel
  IO.FS.withFile path .write fun handle => do
    handle.putStr s!"P3\n{w} {h}\n255\n"
    for row in [:h] do
      let mut line := ""
      for col in [:w] do
        let idx := row * w + col
        let c := pixels.getD idx RGB.black
        line := line ++ s!"{c.r.toNat} {c.g.toNat} {c.b.toNat} "
      handle.putStr (line ++ "\n")

/-- Render image in parallel and write to PPM.

This is the main parallel rendering entry point. It:
1. Divides the image into horizontal chunks
2. Renders each chunk in parallel using dedicated threads
3. Assembles results in order
4. Writes to PPM file
-/
def renderParallel (path : System.FilePath) (cfg : RenderConfig)
    (pixel : Nat → Nat → RGB) : IO Unit := do
  let chunks := makeChunks cfg

  if cfg.showProgress then
    IO.println s!"Rendering {cfg.width}x{cfg.height} with {chunks.size} parallel chunks..."

  -- Render all chunks in parallel
  let results ← renderChunksParallel chunks pixel

  -- Sort by chunk index and concatenate
  let sorted := results.qsort (fun a b => a.1 < b.1)
  let mut allPixels : Array RGB := #[]
  for (_, pixels) in sorted do
    allPixels := allPixels ++ pixels

  -- Write to file
  writePixelsPPM path cfg.width cfg.height allPixels

  if cfg.showProgress then
    IO.println s!"Written to {path}"

/-- Convenience wrapper that takes width/height directly. -/
def renderParallelSimple (path : System.FilePath) (w h : Nat)
    (pixel : Nat → Nat → RGB) : IO Unit :=
  renderParallel path { width := w, height := h } pixel

end Render
end TetraGrayer
