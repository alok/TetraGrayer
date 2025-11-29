/-
Metal Shading Language (MSL) code generation from Lean definitions.

This module provides elaborator-time translation of Lean functions to MSL,
enabling single-source definitions that compile to both CPU (native Lean)
and GPU (Metal shader) backends.

Architecture:
1. `#msl_struct` extracts Lean structures to MSL struct definitions
2. `#check_msl` translates expressions to MSL source
3. `#emit_msl` writes complete shader files
-/

import Lean
import TetraGrayer.Core.Scalar

namespace TetraGrayer.Codegen

open Lean Meta Elab Term Command

-- ============================================================================
-- MSL Type Mapping
-- ============================================================================

/-- Map Lean type names to MSL type names. -/
def leanTypeToMSL : Name → String
  | `Float => "float"
  | `UInt32 => "uint"
  | `UInt8 => "uchar"
  | `Bool => "bool"
  | `Int => "int"
  | `Nat => "uint"
  | `TetraGrayer.Core.CliffordVector => "Vector4"
  | `TetraGrayer.Core.Bivector => "Bivector"
  | `TetraGrayer.Core.Trivector => "Trivector"
  | `TetraGrayer.Core.Versor => "Versor"
  | `TetraGrayer.Core.Particle => "Particle"
  | n => n.toString.replace "." "_"

/-- Check if a type is a primitive MSL type. -/
def isPrimitiveMSLType : Name → Bool
  | `Float | `UInt32 | `UInt8 | `Bool | `Int | `Nat => true
  | _ => false

-- ============================================================================
-- Binary Operator Detection
-- ============================================================================

/-- Recognized binary operators and their MSL equivalents. -/
def binaryOpMap : List (Name × String) := [
  -- Typeclass-based (before whnf)
  (`HAdd.hAdd, "+"),
  (`HSub.hSub, "-"),
  (`HMul.hMul, "*"),
  (`HDiv.hDiv, "/"),
  -- Primitive Float operations (after whnf)
  (`Float.add, "+"),
  (`Float.sub, "-"),
  (`Float.mul, "*"),
  (`Float.div, "/"),
  -- Boolean ops
  (`And, "&&"),
  (`Or, "||"),
  (`BEq.beq, "=="),
  (`bne, "!="),
  (`LT.lt, "<"),
  (`LE.le, "<="),
  (`GT.gt, ">"),
  (`GE.ge, ">="),
  (`Float.blt, "<"),
  (`Float.ble, "<="),
  (`Float.beq, "=="),
  (`Float.bne, "!=")
]

/-- Recognized unary operators. -/
def unaryOpMap : List (Name × String) := [
  (`Neg.neg, "-"),
  (`Float.neg, "-"),
  (`Not, "!"),
  (`Bool.not, "!")
]

/-- Recognized function calls. -/
def funcCallMap : List (Name × String) := [
  (`Float.sqrt, "sqrt"),
  (`Float.sin, "sin"),
  (`Float.cos, "cos"),
  (`Float.tan, "tan"),
  (`Float.asin, "asin"),
  (`Float.acos, "acos"),
  (`Float.atan, "atan"),
  (`Float.atan2, "atan2"),
  (`Float.exp, "exp"),
  (`Float.log, "log"),
  (`Float.pow, "pow"),
  (`Float.abs, "fabs"),
  (`max, "max"),
  (`min, "min"),
  (`Float.ofNat, "(float)"),
  (`Float.ofInt, "(float)")
]

-- ============================================================================
-- Struct Layout
-- ============================================================================

/-- Information about a struct field for MSL emission. -/
structure MSLField where
  /-- Field name. -/
  name : String
  /-- MSL type of the field. -/
  typeName : String
deriving Repr, Inhabited

/-- Information about a struct for MSL emission. -/
structure MSLStruct where
  /-- Struct name in MSL. -/
  name : String
  /-- Fields in declaration order. -/
  fields : Array MSLField
deriving Repr, Inhabited

/-- Generate MSL struct definition. -/
def MSLStruct.toMSL (s : MSLStruct) : String :=
  let fieldLines := s.fields.map fun f => s!"    {f.typeName} {f.name};"
  let body := String.intercalate "\n" fieldLines.toList
  s!"struct {s.name} \{\n{body}\n};"

-- ============================================================================
-- Expression Translation Context
-- ============================================================================

/-- Context for MSL translation. -/
structure MSLContext where
  /-- Mapping from FVarId to MSL variable names. -/
  varNames : Std.HashMap FVarId String := {}
  /-- Counter for generating fresh variable names. -/
  freshCounter : Nat := 0
  /-- Statements to emit before the final expression. -/
  statements : Array String := #[]
  /-- Indentation level. -/
  indent : Nat := 1
deriving Inhabited

/-- MSL translation monad. -/
abbrev MSLM := StateT MSLContext MetaM

/-- Get fresh variable name. -/
def freshVar (hint : String := "v") : MSLM String := do
  let ctx ← get
  let name := s!"{hint}{ctx.freshCounter}"
  set { ctx with freshCounter := ctx.freshCounter + 1 }
  return name

/-- Register an FVar with an MSL variable name. -/
def registerFVar (fvarId : FVarId) (mslName : String) : MSLM Unit := do
  let ctx ← get
  set { ctx with varNames := ctx.varNames.insert fvarId mslName }

/-- Look up MSL name for an FVar. -/
def lookupFVar (fvarId : FVarId) : MSLM (Option String) := do
  let ctx ← get
  return ctx.varNames[fvarId]?

/-- Add a statement to emit. -/
def addStatement (stmt : String) : MSLM Unit := do
  let ctx ← get
  set { ctx with statements := ctx.statements.push stmt }

/-- Get indentation string. -/
def getIndent : MSLM String := do
  let ctx ← get
  return String.mk (List.replicate (ctx.indent * 4) ' ')

-- ============================================================================
-- Application Pattern Matching
-- ============================================================================

/-- Collect all arguments from nested applications. -/
partial def collectAppArgs (e : Expr) : Expr × Array Expr :=
  match e with
  | .app f arg =>
    let (fn, args) := collectAppArgs f
    (fn, args.push arg)
  | _ => (e, #[])

/-- Get the head constant name from an expression. -/
def getHeadConst (e : Expr) : Option Name :=
  match e with
  | .const n _ => some n
  | .app f _ => getHeadConst f
  | _ => none

/-- Check if expression is a binary operator application. -/
def asBinaryOp (e : Expr) : Option (String × Expr × Expr) := do
  let (fn, args) := collectAppArgs e
  let .const name _ := fn | none
  let op ← binaryOpMap.lookup name
  -- Binary ops in Lean have form: HAdd.hAdd α β γ inst a b
  -- We need at least 6 args for the full application
  guard (args.size ≥ 2)
  -- Last two args are the operands
  let b := args[args.size - 1]!
  let a := args[args.size - 2]!
  return (op, a, b)

/-- Check if expression is a unary operator application. -/
def asUnaryOp (e : Expr) : Option (String × Expr) := do
  let (fn, args) := collectAppArgs e
  let .const name _ := fn | none
  let op ← unaryOpMap.lookup name
  guard (args.size ≥ 1)
  let a := args[args.size - 1]!
  return (op, a)

/-- Check if expression is a known function call. -/
def asFuncCall (e : Expr) : Option (String × Array Expr) := do
  let (fn, args) := collectAppArgs e
  let .const name _ := fn | none
  let mslFunc ← funcCallMap.lookup name
  -- For most math functions, the last arg is the actual argument
  guard (args.size ≥ 1)
  -- Special case for atan2 which has 2 args
  if mslFunc == "atan2" && args.size ≥ 2 then
    return (mslFunc, #[args[args.size - 2]!, args[args.size - 1]!])
  else if mslFunc == "max" || mslFunc == "min" then
    guard (args.size ≥ 2)
    return (mslFunc, #[args[args.size - 2]!, args[args.size - 1]!])
  else
    return (mslFunc, #[args[args.size - 1]!])

/-- Check if expression is a struct constructor. -/
def asStructCtor (e : Expr) : MetaM (Option (Name × Array Expr)) := do
  let (fn, args) := collectAppArgs e
  let .const name _ := fn | return none
  let env ← getEnv
  -- Check if this is a structure constructor
  if isStructure env name then
    return none  -- This is the struct type itself, not constructor
  -- Check if it's a .mk constructor
  let parentName := name.getPrefix
  if isStructure env parentName then
    return some (parentName, args)
  return none

-- ============================================================================
-- Main Expression Translator
-- ============================================================================

/-- Infer MSL type from Lean type expression. -/
def inferMSLType (ty : Expr) : MetaM String := do
  let ty ← whnf ty
  match ty with
  | .const name _ => return leanTypeToMSL name
  | .app (.const name _) _ => return leanTypeToMSL name
  | _ => return "float"  -- Default fallback

mutual
/-- Translate a Lean expression to MSL. Preserves let bindings. -/
partial def exprToMSL (e : Expr) : MSLM String := do
  -- Handle let bindings BEFORE whnf (whnf would reduce them away)
  match e with
  | .letE name ty val body _ =>
    let varName ← freshVar name.toString
    let tyStr ← inferMSLType ty
    let valStr ← exprToMSL val
    let indent ← getIndent
    addStatement s!"{indent}{tyStr} {varName} = {valStr};"
    withLetDecl name ty val fun fvar => do
      registerFVar fvar.fvarId! varName
      let bodyExpr := body.instantiate1 fvar
      exprToMSL bodyExpr
  | .mdata _ inner => exprToMSL inner
  | .lam _ _ _ _ =>
    -- Lambda might contain let bindings, so don't reduce with whnf
    exprToMSLReduced e
  | _ =>
    -- For other expressions, use whnf to reduce typeclass applications
    let e ← whnf e
    exprToMSLReduced e

/-- Translate a whnf-reduced expression to MSL. -/
partial def exprToMSLReduced (e : Expr) : MSLM String := do
  -- Try binary operator
  if let some (op, a, b) := asBinaryOp e then
    let aStr ← exprToMSL a
    let bStr ← exprToMSL b
    return s!"({aStr} {op} {bStr})"

  -- Try unary operator
  if let some (op, a) := asUnaryOp e then
    let aStr ← exprToMSL a
    return s!"({op}{aStr})"

  -- Try function call
  if let some (func, args) := asFuncCall e then
    let argStrs ← args.mapM exprToMSL
    let argsStr := String.intercalate ", " argStrs.toList
    if func.startsWith "(" then
      -- Cast operation
      return s!"{func}({argsStr})"
    else
      return s!"{func}({argsStr})"

  -- Try struct constructor
  if let some (structName, args) := ← asStructCtor e then
    let argStrs ← args.mapM exprToMSL
    let argsStr := String.intercalate ", " argStrs.toList
    let mslName := leanTypeToMSL structName
    return s!"({mslName})\{{argsStr}}"

  match e with
  -- Literals
  | .lit (.natVal n) => return s!"{n}"
  | .lit (.strVal s) => return s!"\"{s}\""

  -- Free variables (bound by lambdas/lets in outer scope)
  | .fvar fvarId =>
    match ← lookupFVar fvarId with
    | some name => return name
    | none =>
      let decl ← fvarId.getDecl
      return decl.userName.toString

  -- Constants
  | .const name _ =>
    -- Check if it's a known constant
    if name == `Float.pi then return "M_PI_F"
    else if name == `true then return "true"
    else if name == `false then return "false"
    else return leanTypeToMSL name

  -- Lambda - enter the binder
  | .lam name ty body bi =>
    let varName ← freshVar name.toString
    withLocalDecl name bi ty fun fvar => do
      registerFVar fvar.fvarId! varName
      let bodyExpr := body.instantiate1 fvar
      exprToMSL bodyExpr

  -- Let binding (shouldn't normally reach here after exprToMSL, but handle for safety)
  | .letE name ty val body _ =>
    let varName ← freshVar name.toString
    let tyStr ← inferMSLType ty
    let valStr ← exprToMSL val
    let indent ← getIndent
    addStatement s!"{indent}{tyStr} {varName} = {valStr};"
    withLetDecl name ty val fun fvar => do
      registerFVar fvar.fvarId! varName
      let bodyExpr := body.instantiate1 fvar
      exprToMSL bodyExpr

  -- Projection (struct field access)
  | .proj structName idx struct =>
    let structStr ← exprToMSL struct
    let env ← getEnv
    -- Get field name from structure info
    let fieldName := match getStructureInfo? env structName with
      | some info =>
        if h : idx < info.fieldNames.size then
          info.fieldNames[idx].toString
        else s!"field{idx}"
      | none => s!"field{idx}"
    return s!"{structStr}.{fieldName}"

  -- Application (catch-all for unhandled cases)
  | .app f arg =>
    let fStr ← exprToMSL f
    let argStr ← exprToMSL arg
    -- If f looks like a function name, call it
    if fStr.all (fun c => c.isAlpha || c.isDigit || c == '_') then
      return s!"{fStr}({argStr})"
    else
      return s!"{fStr}"  -- Likely a partially applied function, just return

  -- Metadata wrapper
  | .mdata _ inner => exprToMSL inner

  -- MVar (shouldn't appear in elaborated terms)
  | .mvar _ => return "/* mvar */"

  -- BVar (shouldn't appear after instantiation)
  | .bvar idx => return s!"v{idx}"

  -- Sort/ForallE (type-level, shouldn't appear in values)
  | .sort _ => return "/* type */"
  | .forallE _ _ _ _ => return "/* forall */"
end

-- ============================================================================
-- Struct Extraction
-- ============================================================================

/-- Extract MSL struct definition from a Lean structure. -/
def extractStructDef (structName : Name) : MetaM MSLStruct := do
  let env ← getEnv
  let some info := getStructureInfo? env structName
    | throwError s!"{structName} is not a structure"

  -- Get the constructor directly using getStructureCtor
  let ctorVal := getStructureCtor env structName

  -- Get field types from constructor type
  let fields ← forallTelescopeReducing ctorVal.type fun args _ => do
    let mut fields : Array MSLField := #[]
    for i in [:info.fieldNames.size] do
      let fieldName := info.fieldNames[i]!
      -- Find the corresponding argument (skip parameters)
      let numParams := ctorVal.numParams
      if h : numParams + i < args.size then
        let arg := args[numParams + i]
        let ty ← inferType arg
        let tyStr ← inferMSLType ty
        fields := fields.push { name := fieldName.toString, typeName := tyStr }
      else
        fields := fields.push { name := fieldName.toString, typeName := "float" }
    return fields

  return {
    name := leanTypeToMSL structName
    fields := fields
  }

-- ============================================================================
-- Function Emission
-- ============================================================================

/-- MSL function definition. -/
structure MSLFunction where
  /-- Function name. -/
  name : String
  /-- Return type. -/
  returnType : String
  /-- Parameter list as "(type name, ...)" -/
  params : String
  /-- Function body. -/
  body : String
deriving Repr

/-- Generate MSL function from definition. -/
def MSLFunction.toMSL (f : MSLFunction) (inline : Bool := true) : String :=
  let inlineKw := if inline then "inline " else ""
  s!"{inlineKw}{f.returnType} {f.name}{f.params} \{\n{f.body}\n}"

/-- Extract function definition from a Lean constant. -/
def extractFunction (name : Name) : MetaM MSLFunction := do
  let info ← getConstInfoDefn name
  let mslName := name.toString.replace "." "_"

  -- Analyze the type to get parameters and return type
  forallTelescopeReducing info.type fun args retTy => do
    let retTyStr ← inferMSLType retTy

    -- Build parameter list
    let mut paramList : Array String := #[]
    let mut ctx : MSLContext := {}

    for arg in args do
      let ty ← inferType arg
      let tyStr ← inferMSLType ty
      let decl ← arg.fvarId!.getDecl
      let paramName := decl.userName.toString
      paramList := paramList.push s!"{tyStr} {paramName}"
      ctx := { ctx with varNames := ctx.varNames.insert arg.fvarId! paramName }

    let paramsStr := s!"({String.intercalate ", " paramList.toList})"

    -- Translate the body
    let body := info.value
    let bodyInstantiated := body.instantiateLevelParams info.levelParams (info.levelParams.map fun _ => .zero)

    -- Apply all the parameter fvars to get the actual body
    let appliedBody := mkAppN bodyInstantiated args

    let (resultExpr, finalCtx) ← (exprToMSL appliedBody).run ctx

    -- Build body with statements
    let indent := "    "
    let stmts := finalCtx.statements.toList
    let stmtsStr := String.intercalate "\n" stmts
    let bodyStr := if stmts.isEmpty then
      s!"{indent}return {resultExpr};"
    else
      s!"{stmtsStr}\n{indent}return {resultExpr};"

    return {
      name := mslName
      returnType := retTyStr
      params := paramsStr
      body := bodyStr
    }

-- ============================================================================
-- Commands
-- ============================================================================

/-- Emit MSL struct definition for a Lean structure. -/
elab "#msl_struct " structName:ident : command => do
  let name := structName.getId
  let structDef ← liftTermElabM <| extractStructDef name
  let msl := structDef.toMSL
  logInfo m!"MSL struct for {name}:\n{msl}"

/-- Check MSL translation for a definition. -/
elab "#check_msl " fn:ident : command => do
  let name := fn.getId
  let func ← liftTermElabM <| extractFunction name
  let msl := func.toMSL
  logInfo m!"MSL for {name}:\n{msl}"

/-- Helper to extract idents from bracket syntax. -/
def extractIdentsFromBrackets (stx : Syntax) : Array Name :=
  -- The syntax is `[ ident,* ]`, which is 3 children: `[`, SepArray, `]`
  if stx.getNumArgs >= 2 then
    let sepArray := stx[1]
    sepArray.getArgs.filterMap fun arg =>
      if arg.isIdent then some arg.getId else none
  else #[]

/-- Emit MSL to a file. -/
elab "#emit_msl " path:str structs:(("[" ident,* "]")?) funcs:(("[" ident,* "]")?) : command => do
  let pathStr := path.getString

  -- Collect struct and function names from optional syntax
  -- The optional syntax `.raw` is the bracket group directly when present
  let structNames := extractIdentsFromBrackets structs.raw

  let funcNames := extractIdentsFromBrackets funcs.raw

  -- Generate MSL
  let header := "// Auto-generated from Lean definitions\n// Do not edit manually!\n\n#include <metal_stdlib>\nusing namespace metal;\n\n"

  let structDefs ← liftTermElabM do
    let mut defs : Array String := #[]
    for name in structNames do
      let s ← extractStructDef name
      defs := defs.push s.toMSL
    return defs

  let funcDefs ← liftTermElabM do
    let mut defs : Array String := #[]
    for name in funcNames do
      let f ← extractFunction name
      defs := defs.push (f.toMSL)
    return defs

  let content := header ++
    String.intercalate "\n\n" structDefs.toList ++
    (if structDefs.isEmpty then "" else "\n\n") ++
    String.intercalate "\n\n" funcDefs.toList ++
    "\n"

  -- Write to file
  IO.FS.writeFile pathStr content
  logInfo m!"Wrote MSL to {pathStr}"

-- ============================================================================
-- Convenience: Generate Vector Operations
-- ============================================================================

/-- Standard MSL header with Vector4, Bivector types. -/
def mslStandardHeader : String := "
// Standard types for Clifford algebra
struct Vector4 {
    float v0, v1, v2, v3;  // t, x, y, z
};

struct Bivector {
    float b01, b02, b12, b03, b13, b23;
};

struct Particle {
    Vector4 position;
    Vector4 momentum;
};

// Vector operations
inline Vector4 vec_add(Vector4 a, Vector4 b) {
    return {a.v0 + b.v0, a.v1 + b.v1, a.v2 + b.v2, a.v3 + b.v3};
}

inline Vector4 vec_sub(Vector4 a, Vector4 b) {
    return {a.v0 - b.v0, a.v1 - b.v1, a.v2 - b.v2, a.v3 - b.v3};
}

inline Vector4 vec_smul(float s, Vector4 v) {
    return {s * v.v0, s * v.v1, s * v.v2, s * v.v3};
}

inline float vec_dot(Vector4 v, Vector4 w) {
    return -v.v0 * w.v0 + v.v1 * w.v1 + v.v2 * w.v2 + v.v3 * w.v3;
}
"

end TetraGrayer.Codegen
