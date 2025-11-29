/-
Test file for MSL codegen.

This file tests the #msl_struct and #check_msl commands.
-/

import TetraGrayer.Codegen.MSL
import TetraGrayer.Core.Clifford
import TetraGrayer.Core.Particle

set_option linter.missingDocs false

namespace TetraGrayer.Codegen.Test

open TetraGrayer.Core
open TetraGrayer.Codegen

-- ============================================================================
-- Struct Tests
-- ============================================================================

-- Test struct extraction on CliffordVector
#msl_struct TetraGrayer.Core.CliffordVector

-- Test struct extraction on Bivector
#msl_struct TetraGrayer.Core.Bivector

-- Test struct extraction on Particle (has nested CliffordVector fields)
#msl_struct TetraGrayer.Core.Particle

-- ============================================================================
-- Simple Function Tests
-- ============================================================================

-- Basic arithmetic
def testAdd (x y : Float) : Float := x + y
#check_msl TetraGrayer.Codegen.Test.testAdd

def testSub (x y : Float) : Float := x - y
#check_msl TetraGrayer.Codegen.Test.testSub

def testMul (x y : Float) : Float := x * y
#check_msl TetraGrayer.Codegen.Test.testMul

def testDiv (x y : Float) : Float := x / y
#check_msl TetraGrayer.Codegen.Test.testDiv

-- Math functions
def testSqrt (x : Float) : Float := Float.sqrt x
#check_msl TetraGrayer.Codegen.Test.testSqrt

def testTrig (x : Float) : Float := Float.sin x + Float.cos x
#check_msl TetraGrayer.Codegen.Test.testTrig

-- ============================================================================
-- Let Binding Tests
-- ============================================================================

def testLet (a b c : Float) : Float :=
  let sum := a + b
  let product := sum * c
  product
#check_msl TetraGrayer.Codegen.Test.testLet

def testNestedLet (x y : Float) : Float :=
  let a := x + y
  let b := a * a
  let c := Float.sqrt b
  c
#check_msl TetraGrayer.Codegen.Test.testNestedLet

-- ============================================================================
-- Vector Operation Tests (using CliffordVector)
-- ============================================================================

-- Vector addition using struct fields
def vecAddComponents (v w : CliffordVector) : CliffordVector :=
  { v0 := v.v0 + w.v0
  , v1 := v.v1 + w.v1
  , v2 := v.v2 + w.v2
  , v3 := v.v3 + w.v3 }
#check_msl TetraGrayer.Codegen.Test.vecAddComponents

-- Minkowski dot product
def minkowskiDot (v w : CliffordVector) : Float :=
  -v.v0 * w.v0 + v.v1 * w.v1 + v.v2 * w.v2 + v.v3 * w.v3
#check_msl TetraGrayer.Codegen.Test.minkowskiDot

-- Scalar multiplication
def vecScale (s : Float) (v : CliffordVector) : CliffordVector :=
  { v0 := s * v.v0
  , v1 := s * v.v1
  , v2 := s * v.v2
  , v3 := s * v.v3 }
#check_msl TetraGrayer.Codegen.Test.vecScale

-- ============================================================================
-- Particle Operations
-- ============================================================================

-- Extract position from particle
def getPositionX (p : Particle) : Float := p.position.v1
#check_msl TetraGrayer.Codegen.Test.getPositionX

-- Particle update
def updatePosition (p : Particle) (dt : Float) : Particle :=
  { position := { v0 := p.position.v0 + dt * p.momentum.v0
                , v1 := p.position.v1 + dt * p.momentum.v1
                , v2 := p.position.v2 + dt * p.momentum.v2
                , v3 := p.position.v3 + dt * p.momentum.v3 }
  , momentum := p.momentum }
#check_msl TetraGrayer.Codegen.Test.updatePosition

-- ============================================================================
-- Complex Expression Test
-- ============================================================================

def complexExpr (x y z : Float) : Float :=
  let r2 := x * x + y * y + z * z
  let r := Float.sqrt r2
  let theta := Float.acos (z / r)
  let phi := Float.atan2 y x
  r * Float.sin theta * Float.cos phi
#check_msl TetraGrayer.Codegen.Test.complexExpr

-- ============================================================================
-- Actual Clifford Operations from Core
-- ============================================================================

-- Test the actual CliffordVector.add function
#check_msl TetraGrayer.Core.CliffordVector.add

-- Test the actual CliffordVector.smul function
#check_msl TetraGrayer.Core.CliffordVector.smul

-- Test Bivector.add
#check_msl TetraGrayer.Core.Bivector.add

-- Test Bivector.smul
#check_msl TetraGrayer.Core.Bivector.smul

-- Test wedge product (produces a Bivector from two vectors)
#check_msl TetraGrayer.Core.vectorWedge

-- Test bivector dot vector contraction
#check_msl TetraGrayer.Core.bivectorDotVector

-- ============================================================================
-- File Emission Test
-- ============================================================================

-- Generate test output file with Clifford algebra operations
#emit_msl "artifacts/generated_test.metal"
  [TetraGrayer.Core.CliffordVector, TetraGrayer.Core.Bivector, TetraGrayer.Core.Particle]
  [TetraGrayer.Core.CliffordVector.add,
   TetraGrayer.Core.CliffordVector.smul,
   TetraGrayer.Core.Bivector.add,
   TetraGrayer.Core.Bivector.smul,
   TetraGrayer.Core.vectorWedge,
   TetraGrayer.Core.bivectorDotVector,
   TetraGrayer.Codegen.Test.minkowskiDot,
   TetraGrayer.Codegen.Test.updatePosition]

end TetraGrayer.Codegen.Test
