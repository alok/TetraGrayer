/-
Metal Shading Language (MSL) code generation from Lean definitions.

This module provides elaborator-time translation of Lean functions to MSL,
enabling single-source definitions that compile to both CPU (native Lean)
and GPU (Metal shader) backends.

Architecture:
1. `@[msl]` attribute marks functions as MSL-translatable
2. `MSLType` typeclass maps Lean types to MSL type strings
3. `exprToMSL` translates Lean expressions to MSL source
4. `#emit_msl` command generates .metal files from tagged definitions
-/

import Lean
import TetraGrayer.Core.Scalar

namespace TetraGrayer.Codegen

open Lean Meta Elab Term Command

-- ============================================================================
-- MSL Type Mapping
-- ============================================================================

/-- Typeclass for types that have MSL equivalents. -/
class MSLType (α : Type _) where
  /-- The MSL type name (e.g., "float", "float4"). -/
  mslTypeName : String
  /-- The MSL struct definition if this is a compound type. -/
  mslStructDef : Option String := none

instance : MSLType Float where
  mslTypeName := "float"

instance : MSLType UInt32 where
  mslTypeName := "uint"

instance : MSLType UInt8 where
  mslTypeName := "uchar"

instance : MSLType Bool where
  mslTypeName := "bool"

/-- MSL primitive operations mapping. -/
def mslPrimitiveOp : Name → Option String
  | `Float.add => some "+"
  | `Float.sub => some "-"
  | `Float.mul => some "*"
  | `Float.div => some "/"
  | `Float.sqrt => some "sqrt"
  | `Float.sin => some "sin"
  | `Float.cos => some "cos"
  | `Float.tan => some "tan"
  | `Float.asin => some "asin"
  | `Float.acos => some "acos"
  | `Float.atan => some "atan"
  | `Float.atan2 => some "atan2"
  | `Float.exp => some "exp"
  | `Float.log => some "log"
  | `Float.pow => some "pow"
  | `Float.abs => some "fabs"
  | `max => some "max"
  | `min => some "min"
  | _ => none

-- ============================================================================
-- Struct Layout Verification
-- ============================================================================

/-- Information about a struct field for MSL emission. -/
structure MSLField where
  /-- Field name. -/
  name : String
  /-- MSL type of the field. -/
  typeName : String
  /-- Byte offset in the struct (for verification). -/
  offset : Nat := 0
deriving Repr, Inhabited

/-- Information about a struct for MSL emission. -/
structure MSLStruct where
  /-- Struct name in MSL. -/
  name : String
  /-- Fields in declaration order. -/
  fields : Array MSLField
  /-- Total size in bytes. -/
  size : Nat := 0
deriving Repr, Inhabited

/-- Generate MSL struct definition from MSLStruct. -/
def MSLStruct.toMSL (s : MSLStruct) : String :=
  let fieldLines := s.fields.map fun f => s!"    {f.typeName} {f.name};"
  let body := String.intercalate "\n" fieldLines.toList
  s!"struct {s.name} \{\n{body}\n};"

-- ============================================================================
-- Expression Translation
-- ============================================================================

/-- Context for MSL translation. -/
structure MSLContext where
  /-- Mapping from Lean names to MSL variable names. -/
  varNames : Std.HashMap Name String := {}
  /-- Counter for generating fresh variable names. -/
  freshCounter : Nat := 0
  /-- Indentation level. -/
  indent : Nat := 0
deriving Inhabited

/-- MSL translation monad. -/
abbrev MSLM := StateT MSLContext MetaM

/-- Get fresh variable name. -/
def freshVarName (pre : String := "v") : MSLM String := do
  let ctx ← get
  let name := s!"{pre}{ctx.freshCounter}"
  set { ctx with freshCounter := ctx.freshCounter + 1 }
  return name

/-- Register a Lean name with an MSL variable name. -/
def registerVar (leanName : Name) (mslName : String) : MSLM Unit := do
  let ctx ← get
  set { ctx with varNames := ctx.varNames.insert leanName mslName }

/-- Look up MSL name for a Lean name. -/
def lookupVar (leanName : Name) : MSLM (Option String) := do
  let ctx ← get
  return ctx.varNames[leanName]?

/-- Get current indentation string. -/
def getIndent : MSLM String := do
  let ctx ← get
  return String.mk (List.replicate (ctx.indent * 4) ' ')

/-- Increase indentation. -/
def withIndent (m : MSLM α) : MSLM α := do
  let ctx ← get
  set { ctx with indent := ctx.indent + 1 }
  let result ← m
  set { ctx with indent := ctx.indent }
  return result

/-- Translate a Lean expression to MSL. Returns the MSL expression string. -/
partial def exprToMSL (e : Expr) : MSLM String := do
  match e with
  -- Literals
  | .lit (.natVal n) => return toString n
  | .lit (.strVal s) => return s!"\"{s}\""

  -- Float literals (appear as apps of Float.ofScientific)
  | .app (.app (.app (.const `Float.ofScientific _) mantissa) _sign) _exp =>
    -- Reconstruct float value
    let m ← exprToMSL mantissa
    -- Simplified: just emit as float literal if we can extract
    return s!"{m}.0f" -- TODO: proper scientific notation handling

  -- Local variables (bound by let/lambda)
  | .bvar idx => return s!"v{idx}" -- Should be resolved by context

  -- Free variables
  | .fvar fvarId =>
    let decl ← fvarId.getDecl
    match ← lookupVar decl.userName with
    | some mslName => return mslName
    | none => return decl.userName.toString

  -- Constants (function names, constructors)
  | .const name _ =>
    match mslPrimitiveOp name with
    | some op => return op
    | none =>
      -- Check if it's a known function we've translated
      return name.toString.replace "." "_"

  -- Function application
  | .app f arg =>
    let fExpr ← exprToMSL f
    let argExpr ← exprToMSL arg
    -- Check if f is a binary operator
    if fExpr.startsWith "+" || fExpr.startsWith "-" ||
       fExpr.startsWith "*" || fExpr.startsWith "/" then
      return s!"({argExpr} {fExpr})" -- Will be fixed up by parent
    else
      return s!"{fExpr}({argExpr})"

  -- Lambda (becomes inline or separate function)
  | .lam name _ty body _ =>
    let varName ← freshVarName name.toString
    registerVar name varName
    let bodyStr ← exprToMSL body
    return s!"/* lambda {varName} */ {bodyStr}"

  -- Let binding
  | .letE name _ty val body _ =>
    let varName ← freshVarName name.toString
    registerVar name varName
    let tyStr := "float" -- TODO: proper type translation
    let valStr ← exprToMSL val
    let bodyStr ← exprToMSL body
    let ind ← getIndent
    return s!"{tyStr} {varName} = {valStr};\n{ind}{bodyStr}"

  -- Projections (struct field access)
  | .proj _typeName idx struct =>
    let structStr ← exprToMSL struct
    -- Map projection index to field name
    -- TODO: look up actual field names from structure info
    let fieldName := s!"field{idx}"
    return s!"{structStr}.{fieldName}"

  -- Metavariables (shouldn't appear in final code)
  | .mvar _ => return "/* mvar */"

  -- Sort (type of types, shouldn't appear in value code)
  | .sort _ => return "/* sort */"

  -- ForallE (pi types, shouldn't appear in value code)
  | .forallE _ _ _ _ => return "/* forall */"

  -- MData (metadata wrapper)
  | .mdata _ inner => exprToMSL inner

-- ============================================================================
-- Struct Definition Extraction
-- ============================================================================

/-- Extract MSL struct definition from a Lean structure. -/
def extractStructDef (structName : Name) : MetaM MSLStruct := do
  let env ← getEnv
  let some _info := env.find? structName
    | throwError s!"Structure {structName} not found"

  let some structInfo := getStructureInfo? env structName
    | throwError s!"{structName} is not a structure"

  let fields ← structInfo.fieldNames.mapM fun fieldName => do
    let fullFieldName := structName ++ fieldName
    let some _fieldInfo := env.find? fullFieldName
      | throwError s!"Field {fullFieldName} not found"
    -- TODO: extract actual type and map to MSL
    pure (MSLField.mk fieldName.toString "float" 0)

  return {
    name := structName.toString.replace "." "_"
    fields := fields
    size := fields.size * 4 -- Assume float = 4 bytes
  }

-- ============================================================================
-- Command: #check_msl
-- ============================================================================

/-- Emit MSL for a Lean definition. -/
elab "#check_msl " fn:ident : command => do
  let name := fn.getId
  let info ← getConstInfoDefn name

  let msl ← liftTermElabM do
    let (result, _) ← (exprToMSL info.value).run {}
    return result

  logInfo m!"MSL for {name}:\n{msl}"

/-- Generate MSL struct definition for a Lean structure. -/
elab "#msl_struct " structName:ident : command => do
  let name := structName.getId

  let structDef ← liftTermElabM <| extractStructDef name
  let msl := structDef.toMSL

  logInfo m!"MSL struct for {name}:\n{msl}"

-- ============================================================================
-- File Emission
-- ============================================================================

/-- Write MSL source to a file. -/
def writeMSLFile (path : System.FilePath) (content : String) : IO Unit := do
  IO.FS.writeFile path content

/-- Generate a complete Metal shader file with header. -/
def generateMetalFile (structs : Array MSLStruct) (functions : Array String) : String :=
  let header := "// Auto-generated from Lean definitions\n#include <metal_stdlib>\nusing namespace metal;\n\n"
  let structDefs := structs.map MSLStruct.toMSL |> Array.toList |> String.intercalate "\n\n"
  let funcDefs := functions.toList |> String.intercalate "\n\n"
  header ++ structDefs ++ "\n\n" ++ funcDefs

end TetraGrayer.Codegen
