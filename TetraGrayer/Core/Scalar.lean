-- Core scalar utilities: scalar alias and small numeric helpers.

namespace TetraGrayer
namespace Core

-- Project scalar type alias for 64-bit floating-point.
abbrev ℝ := Float

-- Clamp a float to the unit interval [0, 1].
def clamp01 (x : ℝ) : ℝ :=
  if x < 0.0 then 0.0 else if x > 1.0 then 1.0 else x

-- Local π constant as Float.
def π : ℝ := 3.141592653589793

-- Degrees to radians.
def deg2rad (d : ℝ) : ℝ := d * (π / 180.0)

-- Radians to degrees.
def rad2deg (r : ℝ) : ℝ := r * (180.0 / π)

end Core
end TetraGrayer
