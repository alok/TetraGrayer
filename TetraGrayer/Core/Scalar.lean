/-!
# Core scalar utilities

Scalar alias and numeric helpers for raytracing computations.
-/

namespace TetraGrayer
namespace Core

/-- Project scalar type alias for 64-bit floating-point. -/
abbrev ℝ := Float

-- ============================================================================
-- Numeric coercions for cleaner code
-- ============================================================================

/-- Coerce Nat to Float implicitly. -/
instance : Coe Nat ℝ where
  coe n := Float.ofNat n

/-- Coerce Int to Float implicitly. -/
instance : Coe Int ℝ where
  coe n := Float.ofInt n

-- ============================================================================
-- Basic numeric utilities
-- ============================================================================

/-- Clamp a float to the unit interval (0 to 1). -/
def clamp01 (x : ℝ) : ℝ :=
  if x < 0.0 then 0.0 else if x > 1.0 then 1.0 else x

/-- Clamp a float to arbitrary bounds. -/
def clamp (lo hi x : ℝ) : ℝ :=
  if x < lo then lo else if x > hi then hi else x

/-- Linear interpolation between two values. -/
def lerp (a b t : ℝ) : ℝ := a + t * (b - a)

/-- Smooth Hermite interpolation (smoothstep). -/
def smoothstep (edge0 edge1 x : ℝ) : ℝ :=
  let t := clamp01 ((x - edge0) / (edge1 - edge0))
  t * t * (3.0 - 2.0 * t)

-- ============================================================================
-- Angle utilities
-- ============================================================================

/-- Local π constant as Float. -/
def π : ℝ := 3.141592653589793

/-- Two times π. -/
def τ : ℝ := 2.0 * π

/-- Degrees to radians. -/
def deg2rad (d : ℝ) : ℝ := d * (π / 180.0)

/-- Radians to degrees. -/
def rad2deg (r : ℝ) : ℝ := r * (180.0 / π)

/-- Normalize angle to range 0 to 2π (exclusive). -/
def normalizeAngle (θ : ℝ) : ℝ :=
  let n := θ / τ
  (n - Float.floor n) * τ

-- ============================================================================
-- Combined trig functions (pure Lean - FFI was slower due to tuple boxing)
-- ============================================================================

/-- Combined sin and cos. -/
@[inline] def sincos (θ : ℝ) : ℝ × ℝ := (Float.sin θ, Float.cos θ)

/-- Combined sinh and cosh. -/
@[inline] def sinhcosh (x : ℝ) : ℝ × ℝ := (Float.sinh x, Float.cosh x)

-- ============================================================================
-- Numeric comparison with tolerance
-- ============================================================================

/-- Check if two floats are approximately equal. -/
def approxEq (a b : ℝ) (tol : ℝ := 1e-9) : Bool :=
  Float.abs (a - b) ≤ tol

/-- Check if float is approximately zero. -/
def approxZero (x : ℝ) (tol : ℝ := 1e-9) : Bool :=
  Float.abs x ≤ tol

end Core
end TetraGrayer
