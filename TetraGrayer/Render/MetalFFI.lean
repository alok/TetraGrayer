-- Metal GPU FFI - Idiomatic Lean Interface
-- Provides type-safe, dependently-typed interface to Metal GPU raytracing.
-- Architecture: Lean Types → C Wrappers → Objective-C → Metal Shader

import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Clifford
import TetraGrayer.Image.PPM

namespace TetraGrayer
namespace Render
namespace Metal

open Core Image

-- ============================================================================
-- Dependent Types for Image Dimensions
-- ============================================================================

/-- Image dimensions with proof that both are positive. -/
structure Dims where
  width : Nat
  height : Nat
  width_pos : 0 < width := by decide
  height_pos : 0 < height := by decide
deriving Repr

namespace Dims

/-- Total pixel count. -/
def pixels (d : Dims) : Nat := d.width * d.height

/-- Aspect ratio. -/
def aspectRatio (d : Dims) : Float :=
  Float.ofNat d.width / Float.ofNat d.height

/-- Standard resolutions. -/
def preview : Dims := ⟨320, 180, by decide, by decide⟩
def hd : Dims := ⟨1280, 720, by decide, by decide⟩
def fullHd : Dims := ⟨1920, 1080, by decide, by decide⟩
def uhd4k : Dims := ⟨3840, 2160, by decide, by decide⟩

/-- Create custom dimensions (with runtime check). -/
def mk? (w h : Nat) : Option Dims :=
  if hw : 0 < w then
    if hh : 0 < h then some ⟨w, h, hw, hh⟩
    else none
  else none

end Dims

-- ============================================================================
-- Constrained Physical Parameters
-- ============================================================================

/-- Spin parameter for Kerr black hole, constrained to 0 ≤ a ≤ 1.
    0 = Schwarzschild (non-rotating), 1 = extremal Kerr (maximally rotating).
-/
structure SpinParam where
  /-- The spin value a/M where M is the black hole mass. -/
  val : Float
  /-- Spin must be non-negative. -/
  nonneg : 0 ≤ val := by native_decide
  /-- Spin must not exceed extremal limit. -/
  le_one : val ≤ 1 := by native_decide
deriving Repr

namespace SpinParam

/-- Create spin parameter with runtime validation. -/
def mk? (v : Float) : Option SpinParam :=
  if h1 : 0 ≤ v then
    if h2 : v ≤ 1 then some ⟨v, h1, h2⟩
    else none
  else none

/-- Schwarzschild (non-rotating). -/
def schwarzschild : SpinParam := ⟨0.0, by native_decide, by native_decide⟩

/-- Moderate spin (a = 0.5). -/
def moderate : SpinParam := ⟨0.5, by native_decide, by native_decide⟩

/-- Fast spin (a = 0.9). -/
def fast : SpinParam := ⟨0.9, by native_decide, by native_decide⟩

/-- Near-extremal (a = 0.99). -/
def extremal : SpinParam := ⟨0.99, by native_decide, by native_decide⟩

instance : Inhabited SpinParam := ⟨schwarzschild⟩

end SpinParam

/-- Field of view in radians, constrained to (0, π).
    Typical values: π/6 (30°), π/3 (60°), π/2 (90°).
-/
structure FOV where
  /-- Field of view in radians. -/
  val : Float
  /-- FOV must be positive. -/
  pos : 0 < val := by native_decide
  /-- FOV must be less than π (180°). -/
  lt_pi : val < π := by native_decide
deriving Repr

namespace FOV

/-- Create FOV with runtime validation. -/
def mk? (v : Float) : Option FOV :=
  if h1 : 0 < v then
    if h2 : v < π then some ⟨v, h1, h2⟩
    else none
  else none

/-- Narrow FOV (30°). -/
def narrow : FOV := ⟨π / 6, by native_decide, by native_decide⟩

/-- Standard FOV (60°). -/
def standard : FOV := ⟨π / 3, by native_decide, by native_decide⟩

/-- Wide FOV (90°). -/
def wide : FOV := ⟨π / 2, by native_decide, by native_decide⟩

/-- Ultra-wide FOV (120°). -/
def ultraWide : FOV := ⟨2 * π / 3, by native_decide, by native_decide⟩

instance : Inhabited FOV := ⟨standard⟩

end FOV

-- ============================================================================
-- Camera Position
-- ============================================================================

/-- Camera position in spacetime coordinates (t, x, y, z). -/
structure CameraPos where
  t : Float := 0.0
  x : Float := 30.0
  y : Float := 0.0
  z : Float := 0.0
deriving Repr, Inhabited

namespace CameraPos

/-- Camera at given distance along +x axis. -/
def atDistance (d : Float) : CameraPos := { x := d }

/-- Camera slightly above equatorial plane. -/
def elevated (d h : Float) : CameraPos := { x := d, z := h }

/-- Convert to CliffordVector. -/
def toClifford (c : CameraPos) : CliffordVector :=
  CliffordVector.mk4 c.t c.x c.y c.z

end CameraPos

-- ============================================================================
-- Render Configuration
-- ============================================================================

/-- Physical parameters for Doran (Kerr) black hole.
    Uses constrained types for spin and FOV to ensure physical validity.
-/
structure DoranConfig where
  /-- Spin parameter with proof of 0 ≤ a ≤ 1 bounds. -/
  spin : SpinParam
  /-- Horizontal field of view with proof of 0 < fov < π bounds. -/
  fov : FOV := FOV.standard
  /-- Escape radius for ray termination. -/
  extractRadius : Float := 100.0
  /-- Maximum affine parameter. -/
  maxParam : Float := 500.0
  /-- Maximum step ratio (blueshift cutoff). -/
  maxStepRatio : Float := 20.0
  /-- Maximum integration steps. -/
  maxSteps : Nat := 10000
  /-- Initial step size. -/
  dparam0 : Float := 0.05
deriving Repr

namespace DoranConfig

/-- Get spin value for FFI. -/
def spinParam (c : DoranConfig) : Float := c.spin.val

/-- Get FOV value for FFI. -/
def hFov (c : DoranConfig) : Float := c.fov.val

/-- Schwarzschild (non-rotating) black hole. -/
def schwarzschild : DoranConfig := { spin := SpinParam.schwarzschild }

/-- Moderate spin. -/
def moderate : DoranConfig := { spin := SpinParam.moderate }

/-- Fast-spinning Kerr black hole. -/
def fastSpin : DoranConfig := { spin := SpinParam.fast }

/-- Near-extremal spin. -/
def extremal : DoranConfig := { spin := SpinParam.extremal }

instance : Inhabited DoranConfig := ⟨schwarzschild⟩

end DoranConfig

/-- Complete render configuration. -/
structure RenderConfig where
  dims : Dims
  doran : DoranConfig
  camera : CameraPos := CameraPos.atDistance 30.0
deriving Repr

namespace RenderConfig

/-- HD resolution with given Doran config. -/
def hd (d : DoranConfig) : RenderConfig :=
  { dims := Dims.hd, doran := d }

/-- Preview resolution for quick iteration. -/
def preview (d : DoranConfig) : RenderConfig :=
  { dims := Dims.preview, doran := d }

/-- Full HD with moderate spin. -/
def standard : RenderConfig :=
  { dims := Dims.fullHd, doran := DoranConfig.moderate }

end RenderConfig

-- ============================================================================
-- Type-Level Pixel Format
-- ============================================================================

/-- Pixel format at the type level. -/
inductive PixelFormat where
  /-- RGBA format (4 bytes per pixel). -/
  | rgba
  /-- BGRA format (4 bytes per pixel, common in GPU APIs). -/
  | bgra
  /-- RGB format (3 bytes per pixel, no alpha). -/
  | rgb
deriving Repr, DecidableEq, Inhabited

namespace PixelFormat

/-- Bytes per pixel for this format. -/
def bytesPerPixel : PixelFormat → Nat
  | rgba => 4
  | bgra => 4
  | rgb => 3

/-- Human-readable name. -/
def name : PixelFormat → String
  | rgba => "RGBA"
  | bgra => "BGRA"
  | rgb => "RGB"

end PixelFormat

-- ============================================================================
-- Metal Image Result
-- ============================================================================

/-- Image buffer with type-level pixel format.

The buffer has exactly `format.bytesPerPixel * dims.pixels` bytes,
with pixels stored in row-major order.
-/
structure Image (format : PixelFormat) where
  /-- Image dimensions. -/
  dims : Dims
  /-- Raw pixel data. -/
  data : ByteArray
  /-- Proof that data has correct size for format. -/
  size_eq : data.size = format.bytesPerPixel * dims.pixels

/-- RGBA image (the format Metal produces). -/
abbrev MetalImage := Image PixelFormat.rgba

/-- RGB image (for PPM output). -/
abbrev RGBImage := Image PixelFormat.rgb

namespace MetalImage

/-- RGBA pixel value. -/
structure RGBA where
  /-- Red channel. -/
  r : UInt8
  /-- Green channel. -/
  g : UInt8
  /-- Blue channel. -/
  b : UInt8
  /-- Alpha channel. -/
  a : UInt8
deriving Repr, Inhabited

/-- Convert Fin-indexed position to linear index. -/
@[inline]
def linearIdx (img : MetalImage) (x : Fin img.dims.width) (y : Fin img.dims.height) : Nat :=
  (y.val * img.dims.width + x.val) * 4

/-- Get pixel at (x, y) using Fin indices. Bounds-safe by construction. -/
@[inline]
def getPixelFin (img : MetalImage) (x : Fin img.dims.width) (y : Fin img.dims.height) : RGBA :=
  let idx := linearIdx img x y
  -- Bounds are proven by: x < width, y < height, size_eq : data.size = 4 * width * height
  -- idx = (y * width + x) * 4 < (height * width) * 4 = data.size
  { r := img.data.get! idx
    g := img.data.get! (idx + 1)
    b := img.data.get! (idx + 2)
    a := img.data.get! (idx + 3) }

/-- Get pixel at (x, y) with explicit bounds proofs. -/
@[inline]
def getPixel (img : MetalImage) (x y : Nat)
    (hx : x < img.dims.width) (hy : y < img.dims.height) : RGBA :=
  getPixelFin img ⟨x, hx⟩ ⟨y, hy⟩

/-- Unchecked pixel access - caller must ensure bounds. -/
@[inline]
def getPixel! (img : MetalImage) (x y : Nat) : RGBA :=
  let idx := (y * img.dims.width + x) * 4
  { r := img.data.get! idx
    g := img.data.get! (idx + 1)
    b := img.data.get! (idx + 2)
    a := img.data.get! (idx + 3) }

/-- Get pixel at (x, y) - legacy tuple interface. -/
def getPixelTuple (img : MetalImage) (x y : Nat)
    (hx : x < img.dims.width) (hy : y < img.dims.height) : UInt8 × UInt8 × UInt8 × UInt8 :=
  let p := getPixel img x y hx hy
  (p.r, p.g, p.b, p.a)

/-- Convert to RGB array for PPM output. -/
def toRGBArray (img : MetalImage) : Array RGB := Id.run do
  let mut result := Array.mkEmpty img.dims.pixels
  for i in [:img.dims.pixels] do
    let idx := i * 4
    let r := img.data.get! idx
    let g := img.data.get! (idx + 1)
    let b := img.data.get! (idx + 2)
    result := result.push ⟨r, g, b⟩
  return result

/-- Convert RGBA image to RGB ByteArray (drops alpha channel). -/
def toRGBBytes (img : MetalImage) : ByteArray := Id.run do
  let mut bytes := ByteArray.mkEmpty (img.dims.pixels * 3)
  for i in [:img.dims.pixels] do
    let idx := i * 4
    bytes := bytes.push (img.data.get! idx)      -- R
    bytes := bytes.push (img.data.get! (idx + 1)) -- G
    bytes := bytes.push (img.data.get! (idx + 2)) -- B
  return bytes

/-- Write to PPM file. -/
def writePPM (img : MetalImage) (path : System.FilePath) : IO Unit := do
  let pixels := img.toRGBArray
  let pixelFn := fun x y => pixels[y * img.dims.width + x]!
  Image.writePPM path img.dims.width img.dims.height pixelFn

/-- Get format name. -/
def formatName (_ : MetalImage) : String := PixelFormat.rgba.name

end MetalImage

-- ============================================================================
-- Error Handling
-- ============================================================================

/-- Metal operation result. -/
inductive Result (α : Type) where
  | ok : α → Result α
  | unavailable : Result α
  | initFailed : Int → Result α
  | renderFailed : Int → Result α
deriving Repr

namespace Result

/-- Map over success value. -/
def map {α β : Type} (f : α → β) : Result α → Result β
  | ok a => ok (f a)
  | unavailable => unavailable
  | initFailed code => initFailed code
  | renderFailed code => renderFailed code

/-- Convert to Option. -/
def toOption : Result α → Option α
  | ok a => some a
  | _ => none

/-- Convert to IO with error message. -/
def toIO (r : Result α) (context : String := "Metal") : IO α :=
  match r with
  | ok a => pure a
  | unavailable => throw <| IO.Error.userError s!"{context}: Metal GPU not available"
  | initFailed code => throw <| IO.Error.userError s!"{context}: initialization failed (code {code})"
  | renderFailed code => throw <| IO.Error.userError s!"{context}: render failed (code {code})"

end Result

-- ============================================================================
-- FFI Declarations
-- ============================================================================

/-- Check if Metal is available on this system. -/
@[extern "lean_metal_available"]
opaque metalAvailable : Unit → Bool

/-- Initialize Metal device and shader. Must be called before rendering. -/
@[extern "lean_metal_init"]
opaque metalInit : Unit → IO Int

/-- Cleanup Metal resources. -/
@[extern "lean_metal_cleanup"]
opaque metalCleanup : Unit → IO Unit

/-- Raw render function. Returns ByteArray of RGBA pixels.
    Caller must ensure proper initialization and cleanup.
-/
@[extern "lean_metal_render"]
opaque metalRenderRaw :
  (width : UInt32) → (height : UInt32) →
  (spinParam : Float) → (extractRadius : Float) →
  (maxParam : Float) → (maxStepRatio : Float) →
  (maxSteps : UInt32) → (dparam0 : Float) →
  (camT : Float) → (camX : Float) → (camY : Float) → (camZ : Float) →
  (hFov : Float) →
  IO (Int × ByteArray)

-- ============================================================================
-- Resource-Safe Metal Context
-- ============================================================================

/-- Opaque handle representing an initialized Metal context.
    This type exists only to track initialization state at the type level.
-/
private opaque MetalContextImpl : Type
/-- Proof that Metal has been initialized. -/
def MetalContext : Type := MetalContextImpl

/-- Token proving Metal is initialized. Dropped on cleanup. -/
structure MetalToken where
  private mk ::
  /-- Phantom field to prevent external construction. -/
  private initialized : Unit

/-- Bracket pattern for safe Metal resource management.
    Ensures cleanup runs even if the action throws.
    Usage: withMetal (fun tok => renderWithToken tok config)
-/
def withMetal {α : Type} (action : MetalToken → IO α) : IO (Result α) := do
  if !metalAvailable () then
    return .unavailable

  let initResult ← metalInit ()
  if initResult != 0 then
    return .initFailed initResult

  -- Create token proving initialization
  let token : MetalToken := ⟨()⟩

  try
    let result ← action token
    return .ok result
  finally
    metalCleanup ()

/-- Raw render that requires a MetalToken proof of initialization. -/
def renderWithToken (token : MetalToken) (config : RenderConfig) : IO (Result MetalImage) := do
  -- Token proves Metal is initialized, no need to re-check
  let _ := token  -- Use token to satisfy exhaustive check

  let (renderResult, data) ← metalRenderRaw
    config.dims.width.toUInt32
    config.dims.height.toUInt32
    config.doran.spinParam
    config.doran.extractRadius
    config.doran.maxParam
    config.doran.maxStepRatio
    config.doran.maxSteps.toUInt32
    config.doran.dparam0
    config.camera.t
    config.camera.x
    config.camera.y
    config.camera.z
    config.doran.hFov

  if renderResult != 0 then
    return .renderFailed renderResult

  let expectedSize := 4 * config.dims.pixels
  if h : data.size = expectedSize then
    return .ok ⟨config.dims, data, h⟩
  else
    return .renderFailed (-100)

-- ============================================================================
-- High-Level API
-- ============================================================================

/-- Check if Metal GPU is available. -/
def isAvailable : Bool := metalAvailable ()

/-- Render using Metal GPU with full configuration.

This is the main entry point for Metal rendering.
Handles initialization, rendering, and cleanup automatically.
-/
def render (config : RenderConfig) : IO (Result MetalImage) := do
  -- Check availability
  if !isAvailable then
    return .unavailable

  -- Initialize
  let initResult ← metalInit ()
  if initResult != 0 then
    return .initFailed initResult

  -- Render (use accessor methods for constrained types)
  let (renderResult, data) ← metalRenderRaw
    config.dims.width.toUInt32
    config.dims.height.toUInt32
    config.doran.spinParam  -- SpinParam → Float via accessor
    config.doran.extractRadius
    config.doran.maxParam
    config.doran.maxStepRatio
    config.doran.maxSteps.toUInt32
    config.doran.dparam0
    config.camera.t
    config.camera.x
    config.camera.y
    config.camera.z
    config.doran.hFov  -- FOV → Float via accessor

  -- Cleanup
  metalCleanup ()

  -- Check result
  if renderResult != 0 then
    return .renderFailed renderResult

  -- Verify size and construct image
  let expectedSize := 4 * config.dims.pixels
  if h : data.size = expectedSize then
    return .ok ⟨config.dims, data, h⟩
  else
    -- This shouldn't happen if Metal implementation is correct
    return .renderFailed (-100)  -- Size mismatch error

/-- Convenience function: render and save to PPM. -/
def renderToPPM (path : System.FilePath) (config : RenderConfig) : IO (Result Unit) := do
  match ← render config with
  | .ok img =>
    img.writePPM path
    return .ok ()
  | .unavailable => return .unavailable
  | .initFailed c => return .initFailed c
  | .renderFailed c => return .renderFailed c

/-- Quick render with HD resolution and moderate spin. -/
def quickRender (path : System.FilePath) : IO (Result Unit) :=
  renderToPPM path RenderConfig.standard

/-- Render using resource-safe bracket pattern.
    Guarantees cleanup even if rendering throws.
    Returns nested Result: outer for init, inner for render.
-/
def renderSafe (config : RenderConfig) : IO (Result (Result MetalImage)) :=
  withMetal fun token => renderWithToken token config

/-- Flatten nested Result from renderSafe. -/
def renderSafe' (config : RenderConfig) : IO (Result MetalImage) := do
  match ← renderSafe config with
  | .ok inner => return inner
  | .unavailable => return .unavailable
  | .initFailed c => return .initFailed c
  | .renderFailed c => return .renderFailed c

end Metal
end Render
end TetraGrayer
