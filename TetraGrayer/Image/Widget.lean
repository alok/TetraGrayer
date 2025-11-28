/-
Image preview utilities for development.

Provides helpers for viewing rendered images:
- Text-based ASCII art preview in terminal
- PPM file info reader
- Image statistics

Note: Full ProofWidgets support requires compatible Lean version.
For now, provides terminal-based previews.
-/

import TetraGrayer.Image.PPM
import TetraGrayer.Core.RayResult

namespace TetraGrayer
namespace Image

open Core

/-- Image data for preview. -/
structure ImageData where
  /-- Image width in pixels -/
  width : Nat
  /-- Image height in pixels -/
  height : Nat
  /-- Pixel data in row-major order -/
  pixels : Array RGB
deriving Inhabited

namespace ImageData

/-- Create image data from pixel function. -/
def ofPixelFn (w h : Nat) (f : Nat → Nat → RGB) : ImageData :=
  let pixels := Id.run do
    let mut arr := Array.mkEmpty (w * h)
    for y in [0:h] do
      for x in [0:w] do
        arr := arr.push (f x y)
    pure arr
  ⟨w, h, pixels⟩

/-- Get pixel at (x, y). -/
def getPixel (img : ImageData) (x y : Nat) : RGB :=
  let idx := y * img.width + x
  img.pixels.getD idx RGB.black

/-- Scale down image for preview (nearest-neighbor). -/
def scale (img : ImageData) (factor : Nat) : ImageData :=
  if factor ≤ 1 then img else
    let newW := img.width / factor
    let newH := img.height / factor
    ofPixelFn newW newH fun x y =>
      img.getPixel (x * factor) (y * factor)

/-- Convert pixel to ASCII character based on brightness. -/
def pixelToChar (px : RGB) : Char :=
  let brightness := (px.r.toNat + px.g.toNat + px.b.toNat) / 3
  let chars := " .:-=+*#%@"
  let idx := brightness * (chars.length - 1) / 255
  chars.data[idx]?.getD ' '

/-- Generate ASCII art preview of image. -/
def toAscii (img : ImageData) (maxWidth : Nat := 80) : String :=
  let scaleFactor := max 1 (img.width / maxWidth)
  let scaled := img.scale scaleFactor
  Id.run do
    let mut result := ""
    for y in [0:scaled.height] do
      for x in [0:scaled.width] do
        result := result.push (pixelToChar (scaled.getPixel x y))
      result := result ++ "\n"
    pure result

/-- Compute histogram of pixel values (for debugging). -/
def histogram (img : ImageData) : Array Nat := Id.run do
  let mut hist := Array.replicate 256 0
  for px in img.pixels do
    let brightness := (px.r.toNat + px.g.toNat + px.b.toNat) / 3
    hist := hist.set! brightness (hist[brightness]! + 1)
  pure hist

/-- Image statistics. -/
structure Stats where
  /-- Image width -/
  width : Nat
  /-- Image height -/
  height : Nat
  /-- Total pixel count -/
  totalPixels : Nat
  /-- Minimum brightness value (0-255) -/
  minBrightness : Nat
  /-- Maximum brightness value (0-255) -/
  maxBrightness : Nat
  /-- Average brightness -/
  avgBrightness : Float
deriving Repr

/-- Compute image statistics. -/
def stats (img : ImageData) : Stats := Id.run do
  let mut minB := 255
  let mut maxB := 0
  let mut sumB : Nat := 0
  for px in img.pixels do
    let b := (px.r.toNat + px.g.toNat + px.b.toNat) / 3
    minB := min minB b
    maxB := max maxB b
    sumB := sumB + b
  let avgB := if img.pixels.size > 0
    then sumB.toFloat / img.pixels.size.toFloat
    else 0.0
  pure { width := img.width
       , height := img.height
       , totalPixels := img.pixels.size
       , minBrightness := minB
       , maxBrightness := maxB
       , avgBrightness := avgB }

end ImageData

/-- Read and display PPM file info. -/
def readPPMInfo (path : System.FilePath) : IO String := do
  let pathExists ← path.pathExists
  if !pathExists then
    return s!"File not found: {path}"
  let content ← IO.FS.readFile path
  let lines := content.splitOn "\n"
  if lines.length < 3 then
    return "Invalid PPM: too few lines"
  let magic := lines[0]!
  if magic != "P3" then
    return s!"Invalid PPM: expected P3, got {magic}"
  let dims := lines[1]!.splitOn " " |>.filter (· != "")
  if dims.length < 2 then
    return "Invalid PPM: can't parse dimensions"
  let w := dims[0]!
  let h := dims[1]!
  return s!"PPM Image: {w}x{h} pixels (ASCII format)"

/-- Preview PPM file as ASCII art. -/
def previewPPM (path : System.FilePath) (maxWidth : Nat := 60) : IO String := do
  let pathExists ← path.pathExists
  if !pathExists then
    return s!"File not found: {path}"
  -- Parse PPM (simplified, assumes well-formed P3)
  let content ← IO.FS.readFile path
  let lines := content.splitOn "\n"
  if lines.length < 4 then
    return "Invalid PPM"
  let dims := lines[1]!.splitOn " " |>.filter (· != "")
  let w := dims[0]!.toNat!
  let h := dims[1]!.toNat!
  -- Parse pixel data
  let pixelStr := String.intercalate " " (lines.drop 3)
  let values := (pixelStr.splitOn " " |>.filter (· != "") |>.map String.toNat!).toArray
  let pixels := Id.run do
    let mut arr : Array RGB := #[]
    let mut i := 0
    while i + 2 < values.size do
      arr := arr.push (RGB.ofNat values[i]! values[i+1]! values[i+2]!)
      i := i + 3
    pure arr
  let img := ImageData.mk w h pixels
  return img.toAscii maxWidth

end Image
end TetraGrayer
