/-
Clifford algebra types and operations for 4D spacetime with signature (-,+,+,+).

Ported from tetra-gray's clifford-static.cuh.

Components use bit-ordering convention:
- Vector: indices 0,1,2,3 correspond to e₀,e₁,e₂,e₃
- Bivector: indices 0..5 correspond to e₀₁,e₀₂,e₁₂,e₀₃,e₁₃,e₂₃ (ascending bit order)
- Trivector: indices 0..3 correspond to e₀₁₂,e₀₁₃,e₀₂₃,e₁₂₃
- Versor (even multivector): scalar + 6 bivector + pseudoscalar
-/

import TetraGrayer.Core.Scalar

namespace TetraGrayer
namespace Core

/-- 4-vector in spacetime. -/
structure CliffordVector where
  v0 : ℝ  -- time component
  v1 : ℝ  -- x
  v2 : ℝ  -- y
  v3 : ℝ  -- z
deriving Repr, Inhabited

/-- 6 independent bivector components. -/
structure Bivector where
  b01 : ℝ  -- e₀∧e₁
  b02 : ℝ  -- e₀∧e₂
  b12 : ℝ  -- e₁∧e₂
  b03 : ℝ  -- e₀∧e₃
  b13 : ℝ  -- e₁∧e₃
  b23 : ℝ  -- e₂∧e₃
deriving Repr, Inhabited

/-- 4 trivector components. -/
structure Trivector where
  t012 : ℝ  -- e₀∧e₁∧e₂
  t013 : ℝ  -- e₀∧e₁∧e₃
  t023 : ℝ  -- e₀∧e₂∧e₃
  t123 : ℝ  -- e₁∧e₂∧e₃
deriving Repr, Inhabited

/-- Even-graded multivector (scalar + bivector + pseudoscalar). -/
structure Versor where
  scalar : ℝ           -- grade 0
  bivec  : Bivector    -- grade 2
  pseudo : ℝ           -- grade 4 (pseudoscalar)
deriving Repr, Inhabited

namespace CliffordVector

def zero : CliffordVector := ⟨0, 0, 0, 0⟩

def mk4 (a b c d : ℝ) : CliffordVector := ⟨a, b, c, d⟩

def get (v : CliffordVector) (i : Nat) : ℝ :=
  match i with
  | 0 => v.v0
  | 1 => v.v1
  | 2 => v.v2
  | 3 => v.v3
  | _ => 0.0

def add (a b : CliffordVector) : CliffordVector :=
  { v0 := a.v0 + b.v0, v1 := a.v1 + b.v1, v2 := a.v2 + b.v2, v3 := a.v3 + b.v3 }

def sub (a b : CliffordVector) : CliffordVector :=
  { v0 := a.v0 - b.v0, v1 := a.v1 - b.v1, v2 := a.v2 - b.v2, v3 := a.v3 - b.v3 }

def smul (s : ℝ) (v : CliffordVector) : CliffordVector :=
  { v0 := s * v.v0, v1 := s * v.v1, v2 := s * v.v2, v3 := s * v.v3 }

def neg (v : CliffordVector) : CliffordVector :=
  { v0 := -v.v0, v1 := -v.v1, v2 := -v.v2, v3 := -v.v3 }

def sdiv (v : CliffordVector) (s : ℝ) : CliffordVector :=
  smul (1.0 / s) v

instance : Zero CliffordVector := ⟨zero⟩
instance : Add CliffordVector := ⟨add⟩
instance : Sub CliffordVector := ⟨sub⟩
instance : Neg CliffordVector := ⟨neg⟩
instance : SMul ℝ CliffordVector := ⟨smul⟩
instance : HMul ℝ CliffordVector CliffordVector := ⟨smul⟩
instance : HMul CliffordVector ℝ CliffordVector := ⟨fun v s => smul s v⟩
instance : HDiv CliffordVector ℝ CliffordVector := ⟨sdiv⟩

end CliffordVector

namespace Bivector

def zero : Bivector := ⟨0, 0, 0, 0, 0, 0⟩

def mk6 (a b c d e f : ℝ) : Bivector := ⟨a, b, c, d, e, f⟩

def get (b : Bivector) (i : Nat) : ℝ :=
  match i with
  | 0 => b.b01
  | 1 => b.b02
  | 2 => b.b12
  | 3 => b.b03
  | 4 => b.b13
  | 5 => b.b23
  | _ => 0.0

def add (a b : Bivector) : Bivector :=
  { b01 := a.b01 + b.b01, b02 := a.b02 + b.b02, b12 := a.b12 + b.b12
  , b03 := a.b03 + b.b03, b13 := a.b13 + b.b13, b23 := a.b23 + b.b23 }

def sub (a b : Bivector) : Bivector :=
  { b01 := a.b01 - b.b01, b02 := a.b02 - b.b02, b12 := a.b12 - b.b12
  , b03 := a.b03 - b.b03, b13 := a.b13 - b.b13, b23 := a.b23 - b.b23 }

def smul (s : ℝ) (b : Bivector) : Bivector :=
  { b01 := s * b.b01, b02 := s * b.b02, b12 := s * b.b12
  , b03 := s * b.b03, b13 := s * b.b13, b23 := s * b.b23 }

def neg (b : Bivector) : Bivector :=
  { b01 := -b.b01, b02 := -b.b02, b12 := -b.b12
  , b03 := -b.b03, b13 := -b.b13, b23 := -b.b23 }

instance : Zero Bivector := ⟨zero⟩
instance : Add Bivector := ⟨add⟩
instance : Sub Bivector := ⟨sub⟩
instance : Neg Bivector := ⟨neg⟩
instance : SMul ℝ Bivector := ⟨smul⟩
instance : HMul ℝ Bivector Bivector := ⟨smul⟩
instance : HMul Bivector ℝ Bivector := ⟨fun b s => smul s b⟩

end Bivector

namespace Trivector

def zero : Trivector := ⟨0, 0, 0, 0⟩

def get (t : Trivector) (i : Nat) : ℝ :=
  match i with
  | 0 => t.t012
  | 1 => t.t013
  | 2 => t.t023
  | 3 => t.t123
  | _ => 0.0

def add (a b : Trivector) : Trivector :=
  { t012 := a.t012 + b.t012, t013 := a.t013 + b.t013
  , t023 := a.t023 + b.t023, t123 := a.t123 + b.t123 }

def neg (t : Trivector) : Trivector :=
  { t012 := -t.t012, t013 := -t.t013, t023 := -t.t023, t123 := -t.t123 }

def smul (s : ℝ) (t : Trivector) : Trivector :=
  { t012 := s * t.t012, t013 := s * t.t013, t023 := s * t.t023, t123 := s * t.t123 }

instance : Zero Trivector := ⟨zero⟩
instance : Add Trivector := ⟨add⟩
instance : Neg Trivector := ⟨neg⟩
instance : SMul ℝ Trivector := ⟨smul⟩
instance : HMul ℝ Trivector Trivector := ⟨smul⟩

end Trivector

namespace Versor

def one : Versor := ⟨1, Bivector.zero, 0⟩

def zero : Versor := ⟨0, Bivector.zero, 0⟩

def ofScalar (s : ℝ) : Versor := ⟨s, Bivector.zero, 0⟩

def ofBivector (b : Bivector) : Versor := ⟨0, b, 0⟩

def mkPseudo (p : ℝ) : Versor := ⟨0, Bivector.zero, p⟩

def get (e : Versor) (i : Nat) : ℝ :=
  match i with
  | 0 => e.scalar
  | 1 => e.bivec.b01
  | 2 => e.bivec.b02
  | 3 => e.bivec.b12
  | 4 => e.bivec.b03
  | 5 => e.bivec.b13
  | 6 => e.bivec.b23
  | 7 => e.pseudo
  | _ => 0.0

def add (a b : Versor) : Versor :=
  { scalar := a.scalar + b.scalar
  , bivec := a.bivec + b.bivec
  , pseudo := a.pseudo + b.pseudo }

def smul (s : ℝ) (e : Versor) : Versor :=
  { scalar := s * e.scalar
  , bivec := s • e.bivec
  , pseudo := s * e.pseudo }

def neg (e : Versor) : Versor :=
  { scalar := -e.scalar, bivec := -e.bivec, pseudo := -e.pseudo }

instance : Zero Versor := ⟨zero⟩
instance : One Versor := ⟨one⟩
instance : Add Versor := ⟨add⟩
instance : Neg Versor := ⟨neg⟩
instance : SMul ℝ Versor := ⟨smul⟩
instance : HMul ℝ Versor Versor := ⟨smul⟩
instance : HMul Versor ℝ Versor := ⟨fun e s => smul s e⟩

def sub (a b : Versor) : Versor := a + (-b)

def sdiv (e : Versor) (s : ℝ) : Versor := smul (1.0 / s) e

instance : Sub Versor := ⟨sub⟩
instance : HDiv Versor ℝ Versor := ⟨sdiv⟩

end Versor

-- ============================================================================
-- Metric contractions (inner products) with signature (-,+,+,+)
-- ============================================================================

/-- Vector inner product: v|w = -v₀w₀ + v₁w₁ + v₂w₂ + v₃w₃ -/
def vectorDot (v w : CliffordVector) : ℝ :=
  -v.v0 * w.v0 + v.v1 * w.v1 + v.v2 * w.v2 + v.v3 * w.v3

/-- Bivector inner product with signature. -/
def bivectorDot (b1 b2 : Bivector) : ℝ :=
  b1.b01 * b2.b01 + b1.b02 * b2.b02 - b1.b12 * b2.b12 +
  b1.b03 * b2.b03 - b1.b13 * b2.b13 - b1.b23 * b2.b23

/-- Versor inner product. -/
def versorDot (e1 e2 : Versor) : ℝ :=
  e1.scalar * e2.scalar - e1.pseudo * e2.pseudo + bivectorDot e1.bivec e2.bivec

-- ============================================================================
-- Contractions between different grades
-- ============================================================================

/-- Bivector contracting a vector from the right: B|v -/
def bivectorDotVector (b : Bivector) (v : CliffordVector) : CliffordVector :=
  { v0 := b.b01 * v.v1 + b.b02 * v.v2 + b.b03 * v.v3
  , v1 := b.b01 * v.v0 + b.b12 * v.v2 + b.b13 * v.v3
  , v2 := b.b02 * v.v0 - b.b12 * v.v1 + b.b23 * v.v3
  , v3 := b.b03 * v.v0 - b.b13 * v.v1 - b.b23 * v.v2 }

/-- Vector contracting a bivector from the left: v|B = -(B|v) -/
def vectorDotBivector (v : CliffordVector) (b : Bivector) : CliffordVector :=
  -(bivectorDotVector b v)

-- ============================================================================
-- Wedge products (exterior products)
-- ============================================================================

/-- Wedge product of two vectors: v∧w -/
def vectorWedge (v w : CliffordVector) : Bivector :=
  { b01 := v.v0 * w.v1 - v.v1 * w.v0
  , b02 := v.v0 * w.v2 - v.v2 * w.v0
  , b12 := v.v1 * w.v2 - v.v2 * w.v1
  , b03 := v.v0 * w.v3 - v.v3 * w.v0
  , b13 := v.v1 * w.v3 - v.v3 * w.v1
  , b23 := v.v2 * w.v3 - v.v3 * w.v2 }

/-- Wedge product of bivector and vector: B∧v -/
def bivectorWedgeVector (b : Bivector) (v : CliffordVector) : Trivector :=
  { t012 := b.b01 * v.v2 - b.b02 * v.v1 + b.b12 * v.v0
  , t013 := b.b01 * v.v3 - b.b03 * v.v1 + b.b13 * v.v0
  , t023 := b.b02 * v.v3 - b.b03 * v.v2 + b.b23 * v.v0
  , t123 := b.b12 * v.v3 - b.b13 * v.v2 + b.b23 * v.v1 }

/-- Wedge product of vector and bivector: v∧B = B∧v -/
def vectorWedgeBivector (v : CliffordVector) (b : Bivector) : Trivector :=
  bivectorWedgeVector b v

-- ============================================================================
-- Hodge duals (contraction with pseudoscalar from left)
-- ============================================================================

/-- Hodge dual of bivector: ~B -/
def bivectorDual (b : Bivector) : Bivector :=
  { b01 := -b.b23
  , b02 := b.b13
  , b12 := b.b03
  , b03 := -b.b12
  , b13 := -b.b02
  , b23 := b.b01 }

/-- Hodge dual of vector: ~v (produces trivector) -/
def vectorDual (v : CliffordVector) : Trivector :=
  { t012 := v.v3, t013 := -v.v2, t023 := v.v1, t123 := v.v0 }

/-- Hodge dual of trivector: ~T (produces vector) -/
def trivectorDual (t : Trivector) : CliffordVector :=
  { v0 := -t.t123, v1 := -t.t023, v2 := t.t013, v3 := -t.t012 }

-- ============================================================================
-- Contractions involving trivectors
-- ============================================================================

/-- Trivector contracting bivector: T|B = -~((~T)∧B) -/
def trivectorDotBivector (t : Trivector) (b : Bivector) : CliffordVector :=
  let tDual := trivectorDual t
  let wedged := vectorWedgeBivector tDual b
  CliffordVector.neg (trivectorDual wedged)

/-- Bivector contracting trivector: B|T = T|B -/
def bivectorDotTrivector (b : Bivector) (t : Trivector) : CliffordVector :=
  trivectorDotBivector t b

-- ============================================================================
-- Geometric products
-- ============================================================================

/-- Geometric product of two bivectors: B₁B₂ -/
def bivectorMul (b1 b2 : Bivector) : Versor :=
  let sc := bivectorDot b1 b2
  let ps := b1.b01 * b2.b23 - b1.b02 * b2.b13 + b1.b12 * b2.b03 +
            b1.b03 * b2.b12 - b1.b13 * b2.b02 + b1.b23 * b2.b01
  let bv : Bivector :=
    { b01 := -b1.b02 * b2.b12 - b1.b03 * b2.b13 + b1.b12 * b2.b02 + b1.b13 * b2.b03
    , b02 := b1.b01 * b2.b12 - b1.b03 * b2.b23 - b1.b12 * b2.b01 + b1.b23 * b2.b03
    , b12 := b1.b01 * b2.b02 - b1.b13 * b2.b23 - b1.b02 * b2.b01 + b1.b23 * b2.b13
    , b03 := b1.b01 * b2.b13 + b1.b02 * b2.b23 - b1.b13 * b2.b01 - b1.b23 * b2.b02
    , b13 := b1.b01 * b2.b03 + b1.b12 * b2.b23 - b1.b03 * b2.b01 - b1.b23 * b2.b12
    , b23 := b1.b02 * b2.b03 - b1.b12 * b2.b13 - b1.b03 * b2.b02 + b1.b13 * b2.b12 }
  ⟨sc, bv, ps⟩

/-- Geometric product of two versors: E₁E₂ -/
def versorMul (e1 e2 : Versor) : Versor :=
  let sc := e1.scalar * e2.scalar - e1.pseudo * e2.pseudo
  let ps := e1.scalar * e2.pseudo + e1.pseudo * e2.scalar
  let b1 := e1.bivec
  let b2 := e2.bivec
  let bb := bivectorMul b1 b2
  let scaledB := e2.scalar • b1 + e1.scalar • b2
  let dualB := e2.pseudo • bivectorDual b1 + e1.pseudo • bivectorDual b2
  let finalBivec := bb.bivec + scaledB + dualB
  ⟨sc + bb.scalar, finalBivec, ps + bb.pseudo⟩

instance : Mul Versor := ⟨versorMul⟩

-- ============================================================================
-- Bilinear multiply: E v E⁻¹ (sandwich product for rotations)
-- ============================================================================

/-- Efficient bilinear multiply avoiding explicit inverse.
    Computes E v E⁻¹ for rotations/boosts. -/
def bilinearMultiply (e : Versor) (v : CliffordVector) : CliffordVector :=
  let bv := e.bivec
  let sc := e.scalar
  let ps := e.pseudo
  let bdotv := bivectorDotVector bv v
  let bwedgev := bivectorWedgeVector bv v
  let parta := (sc * sc) • v
  let partb := (ps * ps) • v
  let partc := (2.0 * sc) • bdotv
  let partd := vectorDotBivector bdotv bv
  let parte := trivectorDotBivector bwedgev bv
  let partf := (2.0 * ps) • trivectorDual bwedgev
  let norm := sc * sc - ps * ps - bivectorDot bv bv
  (parta + partb + partc - partd - parte + partf) / norm

-- ============================================================================
-- Rotor creation from angle and plane
-- ============================================================================

/-- Create a rotor from two vectors defining a plane and an angle.

For Euclidean planes (B² < 0): R = cos(θ/2) - B̂ sin(θ/2)
For hyperbolic planes (B² > 0): R = cosh(θ/2) - B̂ sinh(θ/2)
For null planes (B² = 0): R = 1 - B θ/2
-/
def simpleRotorFromAngle (v1 v2 : CliffordVector) (angle : ℝ) : Versor :=
  let bivec := vectorWedge v1 v2
  let bivecSquared := bivectorDot bivec bivec
  let absBivecNorm := Float.sqrt (Float.abs bivecSquared)
  let halfAngle := angle / 2.0
  if bivecSquared < 0.0 then
    -- Euclidean case (spacelike plane)
    let scalarPart := Float.cos halfAngle
    let bivectorPart := (Float.sin halfAngle / absBivecNorm) • bivec
    ⟨scalarPart, -bivectorPart, 0⟩
  else if bivecSquared > 0.0 then
    -- Hyperbolic case (timelike plane)
    let scalarPart := Float.cosh halfAngle
    let bivectorPart := (Float.sinh halfAngle / absBivecNorm) • bivec
    ⟨scalarPart, -bivectorPart, 0⟩
  else
    -- Galilean case (null/lightlike plane)
    ⟨1.0, -(halfAngle • bivec), 0⟩

end Core
end TetraGrayer
