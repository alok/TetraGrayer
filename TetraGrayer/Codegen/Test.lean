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

-- Test struct extraction on CliffordVector
#msl_struct TetraGrayer.Core.CliffordVector

-- Test struct extraction on Bivector
#msl_struct TetraGrayer.Core.Bivector

-- Test struct extraction on Particle
#msl_struct TetraGrayer.Core.Particle

-- Define a simple test function
def testAdd (x y : Float) : Float := x + y

-- Try to emit MSL for it
#check_msl TetraGrayer.Codegen.Test.testAdd

-- A more complex example with let bindings
def testComplex (a b c : Float) : Float :=
  let sum := a + b
  let product := sum * c
  product

#check_msl TetraGrayer.Codegen.Test.testComplex

end TetraGrayer.Codegen.Test
