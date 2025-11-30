/-
Literate Lean documentation for TetraGrayer code examples.

This file uses Verso's literate programming features for real syntax highlighting.
-/

set_option doc.verso true

/-!
# Doran Spacetime

The Kerr metric describes rotating black holes. TetraGrayer uses Doran coordinates
which avoid coordinate singularities at the event horizon, enabling ray tracing
through the horizon.
-/

/-- Spheroidal radius from Cartesian coordinates. -/
def spheroidalR (a x y z : Float) : Float :=
  let r2 := (x*x + y*y + z*z - a*a) / 2
  Float.sqrt (r2 + Float.sqrt (r2*r2 + a*a*z*z))

/-- Spheroidal angle theta. -/
def spheroidalTheta (a r z : Float) : Float :=
  Float.acos (z / r)

/-!
# Metric Functions

The Kerr metric in Boyer-Lindquist coordinates has two key functions:
- Sigma: related to the radial distance in spheroidal coordinates
- Delta: related to horizons (zeros at r+ and r-)
-/

/-- Sigma function for Kerr metric. -/
def sigma (r a theta : Float) : Float :=
  r * r + a * a * Float.cos theta * Float.cos theta

/-- Delta function for Kerr metric. -/
def delta (r a : Float) : Float :=
  r * r - 2 * r + a * a

/-!
# RK4 Integration

Fourth-order Runge-Kutta integration for geodesic equation.
We define a minimal vector space structure for the integration.
-/

/-- A 4-vector for spacetime calculations. -/
structure Vec4 where
  t : Float
  x : Float
  y : Float
  z : Float
deriving Repr

namespace Vec4

/-- Add two vectors. -/
def add (u v : Vec4) : Vec4 :=
  ⟨u.t + v.t, u.x + v.x, u.y + v.y, u.z + v.z⟩

/-- Scale a vector. -/
def scale (s : Float) (v : Vec4) : Vec4 :=
  ⟨s * v.t, s * v.x, s * v.y, s * v.z⟩

instance : Add Vec4 := ⟨add⟩
instance : HMul Float Vec4 Vec4 := ⟨scale⟩

end Vec4

/-!
# State Vector

The integration state combines position and momentum into a single structure.
-/

/-- Combined state for geodesic integration. -/
structure GeodesicState where
  /-- 4-position in spacetime -/
  pos : Vec4
  /-- 4-momentum (null for photons) -/
  mom : Vec4
deriving Repr

namespace GeodesicState

/-- Add two states (for RK4). -/
def add (s1 s2 : GeodesicState) : GeodesicState :=
  ⟨s1.pos + s2.pos, s1.mom + s2.mom⟩

/-- Scale a state (for RK4). -/
def scale (c : Float) (s : GeodesicState) : GeodesicState :=
  ⟨c * s.pos, c * s.mom⟩

instance : Add GeodesicState := ⟨add⟩
instance : HMul Float GeodesicState GeodesicState := ⟨scale⟩

end GeodesicState

/-!
# RK4 Step

The core RK4 algorithm computes four slopes and combines them with specific weights.
-/

/-- Single RK4 integration step. -/
def rk4Step (rhs : GeodesicState → GeodesicState) (h : Float) (y : GeodesicState) : GeodesicState :=
  let k1 := rhs y
  let k2 := rhs (y + (h/2) * k1)
  let k3 := rhs (y + (h/2) * k2)
  let k4 := rhs (y + h * k3)
  y + (h/6) * (k1 + (2:Float) * k2 + (2:Float) * k3 + k4)

/-!
# Termination Conditions

Rays terminate when they escape to infinity, fall into the horizon, or reach max steps.
-/

/-- Check if ray has escaped to infinity. -/
def hasEscaped (escapeRadius : Float) (pos : Vec4) : Bool :=
  let r2 := pos.x * pos.x + pos.y * pos.y + pos.z * pos.z
  r2 > escapeRadius * escapeRadius

/-- Check if ray has crossed the event horizon. -/
def hasCrossedHorizon (horizonRadius : Float) (pos : Vec4) : Bool :=
  let r2 := pos.x * pos.x + pos.y * pos.y + pos.z * pos.z
  r2 < horizonRadius * horizonRadius

/-!
# Color Mapping

Escaped rays are colored based on their direction on the celestial sphere.
-/

/-- RGB color triple. -/
structure RGB where
  r : UInt8
  g : UInt8
  b : UInt8
deriving Repr

/-- Pi constant for color calculations. -/
def pi : Float := 3.14159265358979

/-- Map direction to color based on celestial quadrant. -/
def celestialColor (dir : Vec4) : RGB :=
  let theta := Float.acos (dir.z / Float.sqrt (dir.x*dir.x + dir.y*dir.y + dir.z*dir.z))
  let phi := Float.atan2 dir.y dir.x
  -- Quadrant-based coloring
  if phi < 0 then
    if theta < pi / 2 then ⟨255, 0, 0⟩   -- Red (top-left)
    else ⟨0, 255, 0⟩                      -- Green (bottom-left)
  else
    if theta < pi / 2 then ⟨0, 0, 255⟩   -- Blue (top-right)
    else ⟨255, 255, 0⟩                    -- Yellow (bottom-right)

/-!
# GPU Configuration

Type-safe configuration for Metal shader generation.
-/

/-- Validated spin parameter between 0 and 1. -/
structure SpinParam where
  val : Float
  valid : Bool

/-- Create a spin parameter with runtime validation. -/
def mkSpin (v : Float) : Option SpinParam :=
  if 0 ≤ v && v ≤ 1 then some ⟨v, true⟩
  else none

/-!
# Example: Non-rotating Black Hole

For a Schwarzschild (non-rotating) black hole, a = 0 simplifies the metric.
-/

/-- Schwarzschild spin parameter. -/
def schwarzschildSpin : SpinParam := ⟨0, true⟩

/-- Example initial position far from black hole. -/
def initialPos : Vec4 := ⟨0, 50, 0, 0⟩

/-- Example initial momentum toward black hole. -/
def initialMom : Vec4 := ⟨1, -1, 0, 0.1⟩

/-!
# Integration Loop

The main ray tracing loop integrates until termination.
-/

/-- Integrate a geodesic until termination. -/
partial def traceRay (rhs : GeodesicState → GeodesicState) (h : Float)
    (escapeR horizonR : Float) (maxSteps : Nat)
    (state : GeodesicState) : GeodesicState := Id.run do
  let mut s := state
  for _ in [0:maxSteps] do
    if hasEscaped escapeR s.pos then return s
    if hasCrossedHorizon horizonR s.pos then return s
    s := rk4Step rhs h s
  return s
