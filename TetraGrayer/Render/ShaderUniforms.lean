-- Type-Safe Shader Uniforms
-- Ensures Lean types match GPU uniform buffer layout at the type level.

import TetraGrayer.Core.Scalar

namespace TetraGrayer
namespace Render
namespace Shader

-- ============================================================================
-- Uniform Types (GPU-compatible primitives)
-- ============================================================================

/-- GPU-compatible uniform types with known sizes and alignments. -/
inductive UniformType where
  /-- 32-bit float (4 bytes, align 4). -/
  | float32
  /-- 32-bit unsigned int (4 bytes, align 4). -/
  | uint32
  /-- 32-bit signed int (4 bytes, align 4). -/
  | int32
  /-- 4D float vector (16 bytes, align 16). -/
  | vec4
deriving Repr, DecidableEq, Inhabited

namespace UniformType

/-- Size in bytes for each uniform type. -/
def size : UniformType → Nat
  | float32 => 4
  | uint32 => 4
  | int32 => 4
  | vec4 => 16

/-- Alignment requirement in bytes. -/
def alignment : UniformType → Nat
  | float32 => 4
  | uint32 => 4
  | int32 => 4
  | vec4 => 16

/-- Human-readable name matching Metal/GLSL. -/
def name : UniformType → String
  | float32 => "float"
  | uint32 => "uint"
  | int32 => "int"
  | vec4 => "float4"

end UniformType

-- ============================================================================
-- Uniform Field (named typed field)
-- ============================================================================

/-- A named field in a uniform buffer. -/
structure UniformField where
  /-- Field name (for documentation). -/
  name : String
  /-- Field type. -/
  type : UniformType
deriving Repr, DecidableEq

-- ============================================================================
-- Uniform Layout (list of fields with computed size)
-- ============================================================================

/-- Layout of a uniform buffer. -/
def UniformLayout := List UniformField

namespace UniformLayout

/-- Empty layout. -/
def empty : UniformLayout := []

/-- Number of fields. -/
def numFields (l : UniformLayout) : Nat := l.length

/-- Total size in bytes (simple sum, no padding for now). -/
def totalBytes (l : UniformLayout) : Nat :=
  l.foldl (fun acc f => acc + f.type.size) 0

/-- DSL for building layouts. -/
def field (name : String) (type : UniformType) : UniformField := ⟨name, type⟩

end UniformLayout

-- ============================================================================
-- Serialization Helpers
-- ============================================================================

/-- Write a Float as 4 bytes (IEEE 754 binary32). -/
def writeFloat (buf : ByteArray) (v : Float) : ByteArray :=
  let bits := v.toUInt32
  buf.push (bits &&& 0xFF).toUInt8
  |>.push ((bits >>> 8) &&& 0xFF).toUInt8
  |>.push ((bits >>> 16) &&& 0xFF).toUInt8
  |>.push ((bits >>> 24) &&& 0xFF).toUInt8

/-- Write a UInt32 as 4 bytes (little-endian). -/
def writeUInt32 (buf : ByteArray) (v : UInt32) : ByteArray :=
  buf.push (v &&& 0xFF).toUInt8
  |>.push ((v >>> 8) &&& 0xFF).toUInt8
  |>.push ((v >>> 16) &&& 0xFF).toUInt8
  |>.push ((v >>> 24) &&& 0xFF).toUInt8

/-- Write a vec4 as 16 bytes. -/
def writeVec4 (buf : ByteArray) (x y z w : Float) : ByteArray :=
  writeFloat (writeFloat (writeFloat (writeFloat buf x) y) z) w

-- ============================================================================
-- Type-Level Layout Definition
-- ============================================================================

/-- Compute total size from type-level layout. -/
def layoutSize : List UniformType → Nat
  | [] => 0
  | t :: ts => t.size + layoutSize ts

-- ============================================================================
-- Uniform Buffer with Type-Safe Fields
-- ============================================================================

/-- A buffer with statically-known field types. -/
structure TypedBuffer (types : List UniformType) where
  /-- Raw byte data. -/
  data : ByteArray
  /-- Size matches layout. -/
  sizeEq : data.size = layoutSize types

/-- Create a zero-filled TypedBuffer of the correct size. -/
def TypedBuffer.zeros (types : List UniformType) : TypedBuffer types :=
  ⟨ByteArray.mk (Array.replicate (layoutSize types) 0),
   by simp [ByteArray.size]⟩

instance (types : List UniformType) : Inhabited (TypedBuffer types) where
  default := TypedBuffer.zeros types

-- ============================================================================
-- Metal Render Parameters (Concrete Layout)
-- ============================================================================

/-- Metal render params as a list of types. -/
def metalParamsTypes : List UniformType :=
  [.uint32, .uint32,    -- width, height
   .float32, .float32,  -- spinParam, extractRadius
   .float32, .float32,  -- maxParam, maxStepRatio
   .uint32, .float32,   -- maxSteps, dparam0
   .vec4,               -- camPos (t, x, y, z)
   .float32]            -- hFov

/-- Expected size of Metal params buffer. -/
def metalParamsSize : Nat := layoutSize metalParamsTypes

/-- Metal params buffer type. -/
abbrev MetalParams := TypedBuffer metalParamsTypes

namespace MetalParams

/-- Create Metal params buffer with all fields (unsafe version). -/
def createUnsafe (width height : UInt32)
    (spinParam extractRadius maxParam maxStepRatio : Float)
    (maxSteps : UInt32) (dparam0 : Float)
    (camT camX camY camZ hFov : Float) : ByteArray :=
  let buf := ByteArray.emptyWithCapacity metalParamsSize
  let buf := writeUInt32 buf width
  let buf := writeUInt32 buf height
  let buf := writeFloat buf spinParam
  let buf := writeFloat buf extractRadius
  let buf := writeFloat buf maxParam
  let buf := writeFloat buf maxStepRatio
  let buf := writeUInt32 buf maxSteps
  let buf := writeFloat buf dparam0
  let buf := writeVec4 buf camT camX camY camZ
  writeFloat buf hFov

/-- Create Metal params buffer with runtime size verification. -/
def create? (width height : UInt32)
    (spinParam extractRadius maxParam maxStepRatio : Float)
    (maxSteps : UInt32) (dparam0 : Float)
    (camT camX camY camZ hFov : Float) : Option MetalParams :=
  let buf := createUnsafe width height spinParam extractRadius maxParam maxStepRatio
    maxSteps dparam0 camT camX camY camZ hFov
  if h : buf.size = layoutSize metalParamsTypes then
    some ⟨buf, h⟩
  else
    none

/-- Create Metal params buffer (panics if size mismatch). -/
def create! (width height : UInt32)
    (spinParam extractRadius maxParam maxStepRatio : Float)
    (maxSteps : UInt32) (dparam0 : Float)
    (camT camX camY camZ hFov : Float) : MetalParams :=
  match create? width height spinParam extractRadius maxParam maxStepRatio
    maxSteps dparam0 camT camX camY camZ hFov with
  | some p => p
  | none => panic! "MetalParams size mismatch"

/-- Expected buffer size. -/
def expectedSize : Nat := metalParamsSize

/-- Verify buffer size at runtime. -/
def isValid (p : MetalParams) : Bool := p.data.size == metalParamsSize

end MetalParams

-- ============================================================================
-- Layout Descriptor (for debugging/validation)
-- ============================================================================

/-- Human-readable layout description. -/
structure LayoutDescriptor where
  /-- Field descriptions as (offset, name, type, size). -/
  fields : List (Nat × String × String × Nat)
  /-- Total size. -/
  totalSize : Nat

/-- Build descriptor for Metal params. -/
def metalParamsDescriptor : LayoutDescriptor :=
  { fields := [
      (0, "width", "uint32", 4),
      (4, "height", "uint32", 4),
      (8, "spinParam", "float", 4),
      (12, "extractRadius", "float", 4),
      (16, "maxParam", "float", 4),
      (20, "maxStepRatio", "float", 4),
      (24, "maxSteps", "uint32", 4),
      (28, "dparam0", "float", 4),
      (32, "camPos", "float4", 16),
      (48, "hFov", "float", 4)
    ]
    totalSize := 52
  }

/-- Print layout for debugging. -/
def printMetalParamsLayout : IO Unit := do
  IO.println "MetalParams Layout:"
  IO.println "==================="
  for (offset, name, ty, size) in metalParamsDescriptor.fields do
    IO.println s!"  offset {offset}: {name} : {ty} ({size} bytes)"
  IO.println s!"  Total: {metalParamsDescriptor.totalSize} bytes"
  IO.println s!"  Computed: {metalParamsSize} bytes"

end Shader
end Render
end TetraGrayer
