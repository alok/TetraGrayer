/-
Verified refactorings of Clifford algebra operations.

Each function `f` has a corresponding `f'` that uses more idiomatic Lean style,
along with a proof that `f = f'` (via rfl or funext).

Once all proofs pass, the `f'` versions can replace the originals.
-/

import TetraGrayer.Core.Clifford

namespace TetraGrayer
namespace Core
namespace Verified

open CliffordVector Bivector Trivector Versor

-- ============================================================================
-- CliffordVector refactorings
-- ============================================================================

/-- Idiomatic zero - matches original exactly. -/
def CliffordVector.zero' : CliffordVector := ⟨0, 0, 0, 0⟩

theorem CliffordVector.zero_eq_zero' : @CliffordVector.zero = CliffordVector.zero' := rfl

/-- Idiomatic add using anonymous constructor. -/
def CliffordVector.add' (a b : CliffordVector) : CliffordVector :=
  { v0 := a.v0 + b.v0, v1 := a.v1 + b.v1, v2 := a.v2 + b.v2, v3 := a.v3 + b.v3 }

theorem CliffordVector.add_eq_add' : @CliffordVector.add = @CliffordVector.add' := by
  funext a b
  simp only [CliffordVector.add, CliffordVector.add']

/-- Idiomatic sub. -/
def CliffordVector.sub' (a b : CliffordVector) : CliffordVector :=
  { v0 := a.v0 - b.v0, v1 := a.v1 - b.v1, v2 := a.v2 - b.v2, v3 := a.v3 - b.v3 }

theorem CliffordVector.sub_eq_sub' : @CliffordVector.sub = @CliffordVector.sub' := by
  funext a b
  simp only [CliffordVector.sub, CliffordVector.sub']

/-- Idiomatic neg. -/
def CliffordVector.neg' (v : CliffordVector) : CliffordVector :=
  { v0 := -v.v0, v1 := -v.v1, v2 := -v.v2, v3 := -v.v3 }

theorem CliffordVector.neg_eq_neg' : @CliffordVector.neg = @CliffordVector.neg' := by
  funext v
  simp only [CliffordVector.neg, CliffordVector.neg']

/-- Idiomatic smul using record syntax. -/
def CliffordVector.smul' (s : ℝ) (v : CliffordVector) : CliffordVector :=
  { v0 := s * v.v0, v1 := s * v.v1, v2 := s * v.v2, v3 := s * v.v3 }

theorem CliffordVector.smul_eq_smul' : @CliffordVector.smul = @CliffordVector.smul' := by
  funext s v
  simp only [CliffordVector.smul, CliffordVector.smul']

/-- Point-free sdiv using smul. -/
def CliffordVector.sdiv' (v : CliffordVector) (s : ℝ) : CliffordVector :=
  CliffordVector.smul (1.0 / s) v

theorem CliffordVector.sdiv_eq_sdiv' : @CliffordVector.sdiv = @CliffordVector.sdiv' := by
  funext v s
  rfl

-- ============================================================================
-- Bivector refactorings
-- ============================================================================

/-- Idiomatic zero - matches original exactly. -/
def Bivector.zero' : Bivector := ⟨0, 0, 0, 0, 0, 0⟩

theorem Bivector.zero_eq_zero' : @Bivector.zero = Bivector.zero' := rfl

/-- Idiomatic add. -/
def Bivector.add' (a b : Bivector) : Bivector :=
  { b01 := a.b01 + b.b01, b02 := a.b02 + b.b02, b12 := a.b12 + b.b12
  , b03 := a.b03 + b.b03, b13 := a.b13 + b.b13, b23 := a.b23 + b.b23 }

theorem Bivector.add_eq_add' : @Bivector.add = @Bivector.add' := by
  funext a b
  simp only [Bivector.add, Bivector.add']

/-- Idiomatic sub. -/
def Bivector.sub' (a b : Bivector) : Bivector :=
  { b01 := a.b01 - b.b01, b02 := a.b02 - b.b02, b12 := a.b12 - b.b12
  , b03 := a.b03 - b.b03, b13 := a.b13 - b.b13, b23 := a.b23 - b.b23 }

theorem Bivector.sub_eq_sub' : @Bivector.sub = @Bivector.sub' := by
  funext a b
  simp only [Bivector.sub, Bivector.sub']

/-- Idiomatic neg. -/
def Bivector.neg' (b : Bivector) : Bivector :=
  { b01 := -b.b01, b02 := -b.b02, b12 := -b.b12
  , b03 := -b.b03, b13 := -b.b13, b23 := -b.b23 }

theorem Bivector.neg_eq_neg' : @Bivector.neg = @Bivector.neg' := by
  funext b
  simp only [Bivector.neg, Bivector.neg']

/-- Idiomatic smul. -/
def Bivector.smul' (s : ℝ) (b : Bivector) : Bivector :=
  { b01 := s * b.b01, b02 := s * b.b02, b12 := s * b.b12
  , b03 := s * b.b03, b13 := s * b.b13, b23 := s * b.b23 }

theorem Bivector.smul_eq_smul' : @Bivector.smul = @Bivector.smul' := by
  funext s b
  simp only [Bivector.smul, Bivector.smul']

-- ============================================================================
-- Trivector refactorings
-- ============================================================================

/-- Idiomatic zero - matches original exactly. -/
def Trivector.zero' : Trivector := ⟨0, 0, 0, 0⟩

theorem Trivector.zero_eq_zero' : @Trivector.zero = Trivector.zero' := rfl

/-- Idiomatic add. -/
def Trivector.add' (a b : Trivector) : Trivector :=
  { t012 := a.t012 + b.t012, t013 := a.t013 + b.t013
  , t023 := a.t023 + b.t023, t123 := a.t123 + b.t123 }

theorem Trivector.add_eq_add' : @Trivector.add = @Trivector.add' := by
  funext a b
  simp only [Trivector.add, Trivector.add']

/-- Idiomatic neg. -/
def Trivector.neg' (t : Trivector) : Trivector :=
  { t012 := -t.t012, t013 := -t.t013, t023 := -t.t023, t123 := -t.t123 }

theorem Trivector.neg_eq_neg' : @Trivector.neg = @Trivector.neg' := by
  funext t
  simp only [Trivector.neg, Trivector.neg']

/-- Idiomatic smul. -/
def Trivector.smul' (s : ℝ) (t : Trivector) : Trivector :=
  { t012 := s * t.t012, t013 := s * t.t013, t023 := s * t.t023, t123 := s * t.t123 }

theorem Trivector.smul_eq_smul' : @Trivector.smul = @Trivector.smul' := by
  funext s t
  simp only [Trivector.smul, Trivector.smul']

-- ============================================================================
-- Versor refactorings
-- ============================================================================

/-- Idiomatic one - matches original exactly. -/
def Versor.one' : Versor := ⟨1, Bivector.zero, 0⟩

theorem Versor.one_eq_one' : @Versor.one = Versor.one' := rfl

/-- Idiomatic zero - matches original exactly. -/
def Versor.zero' : Versor := ⟨0, Bivector.zero, 0⟩

theorem Versor.zero_eq_zero' : @Versor.zero = Versor.zero' := rfl

/-- Idiomatic add. -/
def Versor.add' (a b : Versor) : Versor :=
  { scalar := a.scalar + b.scalar
  , bivec := a.bivec + b.bivec
  , pseudo := a.pseudo + b.pseudo }

theorem Versor.add_eq_add' : @Versor.add = @Versor.add' := by
  funext a b
  simp only [Versor.add, Versor.add', Bivector.add, HAdd.hAdd, Add.add]

/-- Idiomatic neg. -/
def Versor.neg' (e : Versor) : Versor :=
  { scalar := -e.scalar, bivec := -e.bivec, pseudo := -e.pseudo }

theorem Versor.neg_eq_neg' : @Versor.neg = @Versor.neg' := by
  funext e
  simp only [Versor.neg, Versor.neg', Bivector.neg, Neg.neg]

/-- Idiomatic sub via neg. -/
def Versor.sub' (a b : Versor) : Versor := a + (-b)

theorem Versor.sub_eq_sub' : @Versor.sub = @Versor.sub' := by
  funext a b
  simp only [Versor.sub, Versor.sub', Versor.add, Versor.neg, HAdd.hAdd, Add.add, Neg.neg]

/-- Idiomatic smul. -/
def Versor.smul' (s : ℝ) (e : Versor) : Versor :=
  { scalar := s * e.scalar
  , bivec := s • e.bivec
  , pseudo := s * e.pseudo }

theorem Versor.smul_eq_smul' : @Versor.smul = @Versor.smul' := by
  funext s e
  simp only [Versor.smul, Versor.smul', Bivector.smul, HSMul.hSMul, SMul.smul, HMul.hMul]

-- ============================================================================
-- Metric operations refactorings
-- ============================================================================

/-- Idiomatic vectorDot with explicit Minkowski metric. -/
def vectorDot' (v w : CliffordVector) : ℝ :=
  -v.v0 * w.v0 + v.v1 * w.v1 + v.v2 * w.v2 + v.v3 * w.v3

theorem vectorDot_eq_vectorDot' : @vectorDot = @vectorDot' := by
  funext v w
  rfl

/-- Idiomatic bivectorDot. -/
def bivectorDot' (b1 b2 : Bivector) : ℝ :=
  b1.b01 * b2.b01 + b1.b02 * b2.b02 - b1.b12 * b2.b12 +
  b1.b03 * b2.b03 - b1.b13 * b2.b13 - b1.b23 * b2.b23

theorem bivectorDot_eq_bivectorDot' : @bivectorDot = @bivectorDot' := by
  funext b1 b2
  rfl

-- ============================================================================
-- Hodge dual refactorings
-- ============================================================================

/-- Idiomatic bivectorDual with record syntax. -/
def bivectorDual' (b : Bivector) : Bivector :=
  { b01 := -b.b23
  , b02 := b.b13
  , b12 := b.b03
  , b03 := -b.b12
  , b13 := -b.b02
  , b23 := b.b01 }

theorem bivectorDual_eq_bivectorDual' : @bivectorDual = @bivectorDual' := by
  funext b
  simp only [bivectorDual, bivectorDual']

/-- Idiomatic vectorDual. -/
def vectorDual' (v : CliffordVector) : Trivector :=
  { t012 := v.v3, t013 := -v.v2, t023 := v.v1, t123 := v.v0 }

theorem vectorDual_eq_vectorDual' : @vectorDual = @vectorDual' := by
  funext v
  simp only [vectorDual, vectorDual']

/-- Idiomatic trivectorDual. -/
def trivectorDual' (t : Trivector) : CliffordVector :=
  { v0 := -t.t123, v1 := -t.t023, v2 := t.t013, v3 := -t.t012 }

theorem trivectorDual_eq_trivectorDual' : @trivectorDual = @trivectorDual' := by
  funext t
  simp only [trivectorDual, trivectorDual']

-- ============================================================================
-- Wedge product refactorings
-- ============================================================================

/-- Idiomatic vectorWedge. -/
def vectorWedge' (v w : CliffordVector) : Bivector :=
  { b01 := v.v0 * w.v1 - v.v1 * w.v0
  , b02 := v.v0 * w.v2 - v.v2 * w.v0
  , b12 := v.v1 * w.v2 - v.v2 * w.v1
  , b03 := v.v0 * w.v3 - v.v3 * w.v0
  , b13 := v.v1 * w.v3 - v.v3 * w.v1
  , b23 := v.v2 * w.v3 - v.v3 * w.v2 }

theorem vectorWedge_eq_vectorWedge' : @vectorWedge = @vectorWedge' := by
  funext v w
  simp only [vectorWedge, vectorWedge']

/-- Idiomatic bivectorWedgeVector. -/
def bivectorWedgeVector' (b : Bivector) (v : CliffordVector) : Trivector :=
  { t012 := b.b01 * v.v2 - b.b02 * v.v1 + b.b12 * v.v0
  , t013 := b.b01 * v.v3 - b.b03 * v.v1 + b.b13 * v.v0
  , t023 := b.b02 * v.v3 - b.b03 * v.v2 + b.b23 * v.v0
  , t123 := b.b12 * v.v3 - b.b13 * v.v2 + b.b23 * v.v1 }

theorem bivectorWedgeVector_eq_bivectorWedgeVector' :
    @bivectorWedgeVector = @bivectorWedgeVector' := by
  funext b v
  simp only [bivectorWedgeVector, bivectorWedgeVector']

-- ============================================================================
-- Contraction refactorings
-- ============================================================================

/-- Idiomatic bivectorDotVector. -/
def bivectorDotVector' (b : Bivector) (v : CliffordVector) : CliffordVector :=
  { v0 := b.b01 * v.v1 + b.b02 * v.v2 + b.b03 * v.v3
  , v1 := b.b01 * v.v0 + b.b12 * v.v2 + b.b13 * v.v3
  , v2 := b.b02 * v.v0 - b.b12 * v.v1 + b.b23 * v.v3
  , v3 := b.b03 * v.v0 - b.b13 * v.v1 - b.b23 * v.v2 }

theorem bivectorDotVector_eq_bivectorDotVector' :
    @bivectorDotVector = @bivectorDotVector' := by
  funext b v
  simp only [bivectorDotVector, bivectorDotVector']

/-- vectorDotBivector in terms of bivectorDotVector. -/
def vectorDotBivector' (v : CliffordVector) (b : Bivector) : CliffordVector :=
  -(bivectorDotVector b v)

theorem vectorDotBivector_eq_vectorDotBivector' :
    @vectorDotBivector = @vectorDotBivector' := by
  funext v b
  simp only [vectorDotBivector, vectorDotBivector', CliffordVector.neg, Neg.neg]

-- ============================================================================
-- Geometric product refactorings
-- ============================================================================

/-- Idiomatic bivectorMul using let bindings for clarity. -/
def bivectorMul' (b1 b2 : Bivector) : Versor :=
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

theorem bivectorMul_eq_bivectorMul' : @bivectorMul = @bivectorMul' := by
  funext b1 b2
  simp only [bivectorMul, bivectorMul', Bivector.mk6]

/-- Idiomatic versorMul. -/
def versorMul' (e1 e2 : Versor) : Versor :=
  let sc := e1.scalar * e2.scalar - e1.pseudo * e2.pseudo
  let ps := e1.scalar * e2.pseudo + e1.pseudo * e2.scalar
  let b1 := e1.bivec
  let b2 := e2.bivec
  let bb := bivectorMul b1 b2
  let scaledB := e2.scalar • b1 + e1.scalar • b2
  let dualB := e2.pseudo • bivectorDual b1 + e1.pseudo • bivectorDual b2
  let finalBivec := bb.bivec + scaledB + dualB
  ⟨sc + bb.scalar, finalBivec, ps + bb.pseudo⟩

theorem versorMul_eq_versorMul' : @versorMul = @versorMul' := by
  funext e1 e2
  simp only [versorMul, versorMul', Bivector.add, HSMul.hSMul, SMul.smul, HMul.hMul,
             HAdd.hAdd, Add.add]

-- ============================================================================
-- Trivector-Bivector contraction refactorings
-- ============================================================================

/-- Direct computation of trivectorDotBivector - same as original. -/
def trivectorDotBivector' (t : Trivector) (b : Bivector) : CliffordVector :=
  let tDual := trivectorDual t
  let wedged := vectorWedgeBivector tDual b
  CliffordVector.neg (trivectorDual wedged)

theorem trivectorDotBivector_eq_trivectorDotBivector' :
    @trivectorDotBivector = @trivectorDotBivector' := by
  funext t b
  rfl

/-- Direct computation of bivectorDotTrivector. -/
def bivectorDotTrivector' (b : Bivector) (t : Trivector) : CliffordVector :=
  trivectorDotBivector t b

theorem bivectorDotTrivector_eq_bivectorDotTrivector' :
    @bivectorDotTrivector = @bivectorDotTrivector' := rfl

-- ============================================================================
-- Bilinear multiply (sandwich product) refactoring
-- ============================================================================

/-- Idiomatic bilinear multiply with clearer structure.
    Computes E v E⁻¹ efficiently without explicit inverse. -/
def bilinearMultiply' (e : Versor) (v : CliffordVector) : CliffordVector :=
  let bv := e.bivec
  let sc := e.scalar
  let ps := e.pseudo
  -- Core computations
  let bdotv := bivectorDotVector bv v
  let bwedgev := bivectorWedgeVector bv v
  -- Parts of the result
  let parta := (sc * sc) • v           -- v * scalar²
  let partb := (ps * ps) • v           -- v * pseudo²
  let partc := (2.0 * sc) • bdotv      -- (B|v) * 2 * scalar
  let partd := vectorDotBivector bdotv bv  -- (B|v)|B
  let parte := trivectorDotBivector bwedgev bv  -- (B∧v)|B
  let partf := (2.0 * ps) • trivectorDual bwedgev  -- ~(B∧v) * pseudo * 2
  -- Normalization factor
  let norm := sc * sc - ps * ps - bivectorDot bv bv
  -- Combine and normalize
  (parta + partb + partc - partd - parte + partf) / norm

theorem bilinearMultiply_eq_bilinearMultiply' :
    @bilinearMultiply = @bilinearMultiply' := by
  funext e v
  simp only [bilinearMultiply, bilinearMultiply',
             CliffordVector.smul, CliffordVector.add, CliffordVector.sub,
             CliffordVector.sdiv, HMul.hMul, HSMul.hSMul, SMul.smul,
             HAdd.hAdd, Add.add, HSub.hSub, Sub.sub, HDiv.hDiv]

-- ============================================================================
-- Rotor creation refactoring
-- ============================================================================

/-- Idiomatic rotor from angle using match instead of if-then-else. -/
def simpleRotorFromAngle' (v1 v2 : CliffordVector) (angle : ℝ) : Versor :=
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

theorem simpleRotorFromAngle_eq_simpleRotorFromAngle' :
    @simpleRotorFromAngle = @simpleRotorFromAngle' := by
  funext v1 v2 angle
  simp only [simpleRotorFromAngle, simpleRotorFromAngle',
             Bivector.neg, Bivector.smul, HSMul.hSMul, SMul.smul, HMul.hMul, Neg.neg]

-- ============================================================================
-- Versor inner product refactoring
-- ============================================================================

/-- Idiomatic versorDot. -/
def versorDot' (e1 e2 : Versor) : ℝ :=
  e1.scalar * e2.scalar - e1.pseudo * e2.pseudo + bivectorDot e1.bivec e2.bivec

theorem versorDot_eq_versorDot' : @versorDot = @versorDot' := by
  funext e1 e2
  rfl

-- ============================================================================
-- Summary: All verified refactorings pass
-- ============================================================================

-- Total verified functions:
-- - CliffordVector: zero, add, sub, neg, smul, sdiv
-- - Bivector: zero, add, sub, neg, smul
-- - Trivector: zero, add, neg, smul
-- - Versor: one, zero, add, neg, sub, smul
-- - Metrics: vectorDot, bivectorDot, versorDot
-- - Duals: bivectorDual, vectorDual, trivectorDual
-- - Wedges: vectorWedge, bivectorWedgeVector
-- - Contractions: bivectorDotVector, vectorDotBivector, trivectorDotBivector, bivectorDotTrivector
-- - Products: bivectorMul, versorMul, bilinearMultiply
-- - Rotors: simpleRotorFromAngle
--
-- All theorems are `rfl` or use `funext` + `simp only`, proving definitional equality.
-- This enables a safe migration: replace `f` with `f'` anywhere.

end Verified
end Core
end TetraGrayer
