/-
TetraGrayer Documentation - Code Examples
-/
import VersoBlog

open Verso Genre Blog

#doc (Page) "Code Examples" =>

# Lean Code Examples

TetraGrayer is written entirely in Lean 4, leveraging its
strong type system for physical correctness.

## Particle State

A photon is represented by its 4-position and 4-momentum in spacetime:

```
/-- A particle (photon) with 4-position and 4-momentum. -/
@[unbox]
structure Particle where
  /-- Spacetime position (t, x, y, z) -/
  position : CliffordVector
  /-- 4-momentum (E, px, py, pz) -/
  momentum : CliffordVector
deriving Repr, Inhabited
```

The `@[unbox]` attribute tells the compiler to unbox the struct for performance.

## Clifford Vectors

4-vectors use Clifford algebra representation:

```
/-- A 4-component Clifford vector (basis: e₀, e₁, e₂, e₃). -/
@[unbox]
structure CliffordVector where
  v0 : ℝ  -- t component (or e₀ coefficient)
  v1 : ℝ  -- x component (or e₁ coefficient)
  v2 : ℝ  -- y component (or e₂ coefficient)
  v3 : ℝ  -- z component (or e₃ coefficient)
```

## Doran Spacetime

The Kerr metric in Doran coordinates avoids coordinate singularities
at the event horizon:

```
/-- Compute geodesic RHS in Doran coordinates (Kerr black hole). -/
def doranRHS (a : ℝ) (data : ODEData) : Particle :=
  let pos := data.particle.position
  let mom := data.particle.momentum

  -- Spheroidal coordinates
  let r := spheroidalR a pos
  let θ := spheroidalθ a pos

  -- Metric functions
  let Σr := r^2 + a^2 * (cos θ)^2
  let Δr := r^2 - 2*r + a^2

  -- ... geodesic equation terms
```

## Type-Safe GPU Configuration

The Metal FFI uses dependent types to ensure validity:

```
/-- Spin parameter constrained to [0, 1]. -/
structure Spin where
  val : Float
  valid : 0 ≤ val ∧ val ≤ 1

/-- Field of view constrained to (0, π). -/
structure FOV where
  val : Float
  valid : 0 < val ∧ val < Float.pi
```

## RK4 Integration

Fourth-order Runge-Kutta for geodesic integration:

```
/-- Single RK4 step for geodesic integration. -/
def rk4Step (rhs : ODEData → Particle) (h : ℝ) (data : ODEData) : ODEData :=
  let k₁ := rhs data
  let k₂ := rhs (data.advance (h/2) (h/2 * k₁))
  let k₃ := rhs (data.advance (h/2) (h/2 * k₂))
  let k₄ := rhs (data.advance h (h * k₃))

  let δparticle := (h/6) * (k₁ + 2*k₂ + 2*k₃ + k₄)
  data.advance h δparticle
```

## Metal Shader Generation

MSL code is generated from Lean expressions using metaprogramming:

```
/-- Generate MSL for an expression. -/
partial def exprToMSLReduced (e : Expr) : MSLM String := do
  let e ← whnf e

  match e with
  | .app (.app (.const ``HAdd.hAdd _) _) args =>
    let a ← exprToMSLReduced args[0]!
    let b ← exprToMSLReduced args[1]!
    withCSEAuto e do pure s!"({a} + {b})"

  -- ... more patterns
```

## Common Subexpression Elimination

Generated shaders use CSE to avoid recomputing expressions:

```
// Before CSE: many redundant sqrt calls
float r = sqrt(x*x + y*y + z*z);
float theta = atan2(sqrt(x*x + y*y), z);

// After CSE: computed once, reused
float cse1 = (x * x);
float cse2 = (y * y);
float cse3 = (cse1 + cse2);
float cse4 = sqrt(cse3);
float r = sqrt(cse3 + z*z);
float theta = atan2(cse4, z);
```
