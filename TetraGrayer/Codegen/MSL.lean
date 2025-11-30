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
  (`Float.sinh, "sinh"),
  (`Float.cosh, "cosh"),
  (`Float.tanh, "tanh"),
  (`Float.asin, "asin"),
  (`Float.acos, "acos"),
  (`Float.atan, "atan"),
  (`Float.asinh, "asinh"),
  (`Float.acosh, "acosh"),
  (`Float.atanh, "atanh"),
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

/-- CSE cache entry: stores original expression and assigned variable name. -/
structure CSEEntry where
  /-- The original expression (for collision checking). -/
  expr : Expr
  /-- The assigned MSL variable name. -/
  varName : String
deriving Inhabited

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
  /-- CSE cache: hash → (expr, varName). Used to avoid duplicate computations. -/
  cseCache : Std.HashMap UInt64 (Array CSEEntry) := {}
  /-- Whether CSE is enabled (default true). -/
  cseEnabled : Bool := true
  /-- Minimum expression size for CSE (avoid caching trivial expressions). -/
  cseMinSize : Nat := 3
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
-- Common Subexpression Elimination (CSE)
-- ============================================================================

/-- Compute approximate size of an expression (for CSE threshold). -/
partial def exprSize : Expr → Nat
  | .app f a => 1 + exprSize f + exprSize a
  | .lam _ ty body _ => 1 + exprSize ty + exprSize body
  | .forallE _ ty body _ => 1 + exprSize ty + exprSize body
  | .letE _ ty val body _ => 1 + exprSize ty + exprSize val + exprSize body
  | .mdata _ e => exprSize e
  | .proj _ _ e => 1 + exprSize e
  | _ => 1

/-- Look up an expression in the CSE cache. Uses hash + structural equality. -/
def lookupCSE (e : Expr) : MSLM (Option String) := do
  let ctx ← get
  if !ctx.cseEnabled then return none
  let hash := e.hash
  match ctx.cseCache[hash]? with
  | none => return none
  | some entries =>
    -- Check for structural match (handle hash collisions)
    for entry in entries do
      if entry.expr == e then
        return some entry.varName
    return none

/-- Insert an expression into the CSE cache. -/
def insertCSE (e : Expr) (varName : String) : MSLM Unit := do
  let ctx ← get
  if !ctx.cseEnabled then return
  let hash := e.hash
  let entry : CSEEntry := { expr := e, varName := varName }
  let newEntries := match ctx.cseCache[hash]? with
    | none => #[entry]
    | some entries => entries.push entry
  set { ctx with cseCache := ctx.cseCache.insert hash newEntries }

/-- Translate with CSE: check cache first, emit let binding if new, cache result.
    Returns the variable name (cached or newly created).
    Only caches expressions above the size threshold. -/
def withCSE (e : Expr) (mslType : String) (translate : MSLM String) : MSLM String := do
  let ctx ← get
  -- Skip CSE for small expressions
  if !ctx.cseEnabled || exprSize e < ctx.cseMinSize then
    translate
  else
    -- Check cache
    match ← lookupCSE e with
    | some varName => return varName
    | none =>
      -- Translate and cache
      let mslExpr ← translate
      -- Create a let binding for the result
      let varName ← freshVar "cse"
      let indent ← getIndent
      addStatement s!"{indent}{mslType} {varName} = {mslExpr};"
      insertCSE e varName
      return varName

/-- Check if needle is a substring of haystack. -/
def stringContains (needle haystack : String) : Bool :=
  (haystack.splitOn needle).length > 1

/-- Sanitize a Lean name for MSL (handle hygiene markers, underscores, etc). -/
def sanitizeMSLName (name : String) (hint : String := "unused") : String :=
  -- If name contains hygiene markers, use the hint instead
  if stringContains "._@." name || stringContains "._hyg." name then hint
  -- If name is just underscore, use hint
  else if name == "_" then hint
  -- Otherwise replace dots with underscores and ensure valid C identifier
  else
    let cleaned := name.map fun c => if c == '.' then '_' else c
    -- Ensure doesn't start with digit
    if cleaned.isEmpty || cleaned.front.isDigit then s!"v_{cleaned}"
    else cleaned

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

/-- Check if expression is a comparison (LT.lt, LE.le, etc.) and extract operands. -/
def asComparison (e : Expr) : Option (String × Expr × Expr) := do
  let (fn, args) := collectAppArgs e
  let .const name _ := fn | none
  -- LT.lt : {α : Type} → [LT α] → α → α → Prop
  -- LE.le : {α : Type} → [LE α] → α → α → Prop
  -- GT.gt and GE.ge are defined in terms of LT.lt and LE.le
  let op ← match name with
    | ``LT.lt => some "<"
    | ``LE.le => some "<="
    | ``GT.gt => some ">"
    | ``GE.ge => some ">="
    | ``Eq => some "=="
    | ``Ne => some "!="
    | ``BEq.beq => some "=="
    | _ => none
  -- Args: [type, inst, lhs, rhs] for LT.lt etc.
  guard (args.size ≥ 4)
  let lhs := args[args.size - 2]!
  let rhs := args[args.size - 1]!
  return (op, lhs, rhs)

/-- Check if expression is an if-then-else (ite). -/
def asIte (e : Expr) : Option (Expr × Expr × Expr) := do
  let (fn, args) := collectAppArgs e
  let .const name _ := fn | none
  -- ite : {α : Sort u} → (c : Prop) → [Decidable c] → α → α → α
  -- Args for ite: [type, prop, decidableInst, thenBranch, elseBranch]
  -- dite: {α : Sort u} → (c : Prop) → [Decidable c] → (c → α) → (¬c → α) → α
  guard (name == ``ite || name == ``dite)
  guard (args.size ≥ 5)
  -- For ite: prop is at index 1, then/else are at indices 3,4
  -- args[0] = type, args[1] = prop, args[2] = decidable inst, args[3] = then, args[4] = else
  let propIdx := 1
  let thenIdx := args.size - 2
  let elseIdx := args.size - 1
  return (args[propIdx]!, args[thenIdx]!, args[elseIdx]!)

/-- Check if expression is Decidable.rec (if-then-else after whnf). Returns (op, lhs, rhs, trueBr, falseBr). -/
def asDecidableRec (e : Expr) : Option (String × Expr × Expr × Expr × Expr) := do
  let (fn, args) := collectAppArgs e
  let .const name _ := fn | none
  guard (name == ``Decidable.rec)
  guard (args.size ≥ 5)
  -- Decidable.rec has signature:
  -- @Decidable.rec {a : Prop} {motive : Decidable a → Sort u}
  --   (isFalse : ¬a → motive ...) (isTrue : a → motive ...) (t : Decidable a) → motive t
  -- args[0] = a (prop), args[1] = motive, args[2] = false branch, args[3] = true branch, args[4] = decidable inst
  let falseBrRaw := args[2]!  -- isFalse branch (called when prop is false)
  let trueBrRaw := args[3]!   -- isTrue branch (called when prop is true)
  let decidableInst := args[args.size - 1]!
  -- The branches are lambdas that take proofs - extract their bodies
  -- falseBr : ¬a → result, trueBr : a → result
  let falseBr := match falseBrRaw with
    | .lam _ _ body _ => body  -- Extract body, ignoring the proof argument
    | _ => falseBrRaw
  let trueBr := match trueBrRaw with
    | .lam _ _ body _ => body  -- Extract body, ignoring the proof argument
    | _ => trueBrRaw
  -- Extract comparison operands from decidable instance
  let (decFn, decArgs) := collectAppArgs decidableInst
  guard (decArgs.size >= 2)
  let lhs := decArgs[decArgs.size - 2]!
  let rhs := decArgs[decArgs.size - 1]!
  if let .const decName _ := decFn then
    let nameStr := decName.toString
    -- Determine comparison operator from instance name
    let op := if stringContains "decLt" nameStr || stringContains "Lt" nameStr then "<"
              else if stringContains "decLe" nameStr || stringContains "Le" nameStr then "<="
              else if stringContains "decGe" nameStr || stringContains "Ge" nameStr then ">="
              else if stringContains "decGt" nameStr || stringContains "Gt" nameStr then ">"
              else "??"  -- Unknown comparison
    return (op, lhs, rhs, trueBr, falseBr)
  none

/-- Check if expression is a numeric literal via OfNat, Float.ofNat, or Float.ofScientific. -/
def asNumericLit (e : Expr) : Option String :=
  let (fn, args) := collectAppArgs e
  match fn with
  | .const name _ =>
    -- OfNat.ofNat : {α : Type} → (n : Nat) → [OfNat α n] → α
    -- The nat value is argument index 1
    if name == ``OfNat.ofNat && args.size >= 2 then
      match args[1]! with
      | .lit (.natVal n) => some s!"{n}.0"
      | _ => none
    -- Float.ofNat : Nat → Float
    else if name == ``Float.ofNat && args.size >= 1 then
      match args[args.size - 1]! with
      | .lit (.natVal n) => some s!"{n}.0"
      | _ => none
    -- Float.ofScientific : Nat → Bool → Nat → Float
    -- Float.ofScientific mantissa sign exponent
    -- e.g., 2.0 = ofScientific 20 true 1 = 20 * 10^(-1) = 2.0
    -- e.g., 1.5 = ofScientific 15 true 1 = 15 * 10^(-1) = 1.5
    else if name == ``Float.ofScientific && args.size >= 3 then
      match args[0]!, args[1]!, args[2]! with
      | .lit (.natVal mantissa), .const negExp _, .lit (.natVal exp) =>
        if negExp == ``Bool.true then
          -- Negative exponent: mantissa / 10^exp
          let divisor := (10 : Float) ^ exp.toFloat
          let value := mantissa.toFloat / divisor
          some s!"{value}"
        else
          -- Positive exponent: mantissa * 10^exp
          let multiplier := (10 : Float) ^ exp.toFloat
          let value := mantissa.toFloat * multiplier
          some s!"{value}"
      | _, _, _ => none
    else none
  | _ => none

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

/-- CSE with automatic type inference from expression. -/
def withCSEAuto (e : Expr) (translate : MSLM String) : MSLM String := do
  let ty ← inferType e
  let mslType ← inferMSLType ty
  withCSE e mslType translate

mutual
/-- Translate a Lean expression to MSL. Preserves let bindings and ite. -/
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
    -- Beta-reduce to expose ite/let bindings that might be hidden behind lambdas
    let e := e.headBeta
    -- Check for numeric literals BEFORE whnf (whnf expands OfNat)
    if let some lit := asNumericLit e then
      return lit
    -- Check for comparisons BEFORE whnf (whnf reduces typeclass-based comparisons)
    if let some (op, a, b) := asComparison e then
      let aStr ← exprToMSL a
      let bStr ← exprToMSL b
      return s!"({aStr} {op} {bStr})"
    -- Check for ite/dite BEFORE whnf (whnf reduces through Decidable instance)
    if let some (cond, thenBr, elseBr) := asIte e then
      let condStr ← exprToMSL cond
      let thenStr ← exprToMSL thenBr
      let elseStr ← exprToMSL elseBr
      return s!"({condStr} ? {thenStr} : {elseStr})"
    -- Check for let binding after beta reduction
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
    | _ =>
      -- For other expressions, use whnf to reduce typeclass applications
      let e ← whnf e
      exprToMSLReduced e

/-- Translate a whnf-reduced expression to MSL. -/
partial def exprToMSLReduced (e : Expr) : MSLM String := do
  -- Try if-then-else (ternary operator)
  if let some (cond, thenBr, elseBr) := asIte e then
    let condStr ← exprToMSL cond
    let thenStr ← exprToMSL thenBr
    let elseStr ← exprToMSL elseBr
    return s!"({condStr} ? {thenStr} : {elseStr})"

  -- Try Decidable.rec (if-then-else after whnf reduction)
  if let some (op, lhs, rhs, trueBr, falseBr) := asDecidableRec e then
    let lhsStr ← exprToMSL lhs
    let rhsStr ← exprToMSL rhs
    let trueStr ← exprToMSL trueBr
    let falseStr ← exprToMSL falseBr
    return s!"(({lhsStr} {op} {rhsStr}) ? {trueStr} : {falseStr})"

  -- Try comparison proposition (for ite conditions)
  if let some (op, a, b) := asComparison e then
    let aStr ← exprToMSL a
    let bStr ← exprToMSL b
    return s!"({aStr} {op} {bStr})"

  -- Try binary operator (with CSE + auto type inference)
  if let some (op, a, b) := asBinaryOp e then
    return ← withCSEAuto e do
      let aStr ← exprToMSL a
      let bStr ← exprToMSL b
      return s!"({aStr} {op} {bStr})"

  -- Try unary operator (with CSE + auto type inference)
  if let some (op, a) := asUnaryOp e then
    return ← withCSEAuto e do
      let aStr ← exprToMSL a
      return s!"({op}{aStr})"

  -- Try function call (with CSE + auto type inference)
  if let some (func, args) := asFuncCall e then
    return ← withCSEAuto e do
      let argStrs ← args.mapM exprToMSL
      let argsStr := String.intercalate ", " argStrs.toList
      if func.startsWith "(" then
        -- Cast operation
        return s!"{func}({argsStr})"
      else
        return s!"{func}({argsStr})"

  -- Try Float.ofScientific (before struct constructor check)
  let (fn, args) := collectAppArgs e
  if let .const name _ := fn then
    if name == ``Float.ofScientific && args.size == 3 then
      -- Float.ofScientific mantissa negExp exp = mantissa * 10^(-exp if negExp else exp)
      if let (.lit (.natVal mantissa), .const negExpName _, .lit (.natVal exp)) := (args[0]!, args[1]!, args[2]!) then
        let value := if negExpName == ``Bool.true then
          mantissa.toFloat / (10 : Float) ^ exp.toFloat
        else
          mantissa.toFloat * (10 : Float) ^ exp.toFloat
        return s!"{value}"

  -- Try struct constructor (with CSE + auto type inference)
  if let some (structName, args) := ← asStructCtor e then
    let mslName := leanTypeToMSL structName
    return ← withCSEAuto e do
      let argStrs ← args.mapM exprToMSL
      let argsStr := String.intercalate ", " argStrs.toList
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
    let mut unusedCounter : Nat := 0

    for arg in args do
      let ty ← inferType arg
      let tyStr ← inferMSLType ty
      let decl ← arg.fvarId!.getDecl
      let rawName := decl.userName.toString
      let paramName := sanitizeMSLName rawName s!"unused{unusedCounter}"
      -- Track if we used an "unused" name to increment counter
      if paramName.startsWith "unused" then
        unusedCounter := unusedCounter + 1
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

-- ============================================================================
-- Complete Raytracer Kernel Generation
-- ============================================================================

/-- Generate a complete raytracer Metal kernel shader.

This emits a self-contained Metal compute shader that includes:
- All necessary struct definitions
- Coordinate transformation functions
- Clifford algebra operations
- Doran spacetime RHS
- RK4 integrator (inlined, not recursive)
- Colormap
- Main kernel entry point
-/
def raytracerKernelMSL : String := "
// ============================================================================
// TetraGrayer Metal Raytracer Kernel
// Auto-generated from Lean definitions
// ============================================================================

#include <metal_stdlib>
using namespace metal;

// ============================================================================
// Type Definitions
// ============================================================================

struct Vector4 {
    float v0;  // t (time)
    float v1;  // x
    float v2;  // y
    float v3;  // z
};

struct Bivector {
    float b01;  // t∧x
    float b02;  // t∧y
    float b12;  // x∧y
    float b03;  // t∧z
    float b13;  // x∧z
    float b23;  // y∧z
};

struct Particle {
    Vector4 position;
    Vector4 momentum;
};

struct ODEData {
    Particle value;
    float param;    // affine parameter λ
    float dparam;   // step size
};

struct CameraParams {
    Vector4 position;
    float hFov;
    uint width;
    uint height;
    float dparam;
};

struct DoranParams {
    float spinParam;      // a
    float extractRadius;
    float maxParam;
    float maxStepRatio;
    uint maxSteps;
};

struct RGB {
    uchar r, g, b;
};

// ============================================================================
// Vector Operations
// ============================================================================

inline Vector4 vec_add(Vector4 a, Vector4 b) {
    return (Vector4){a.v0 + b.v0, a.v1 + b.v1, a.v2 + b.v2, a.v3 + b.v3};
}

inline Vector4 vec_sub(Vector4 a, Vector4 b) {
    return (Vector4){a.v0 - b.v0, a.v1 - b.v1, a.v2 - b.v2, a.v3 - b.v3};
}

inline Vector4 vec_neg(Vector4 v) {
    return (Vector4){-v.v0, -v.v1, -v.v2, -v.v3};
}

inline Vector4 vec_smul(float s, Vector4 v) {
    return (Vector4){s * v.v0, s * v.v1, s * v.v2, s * v.v3};
}

inline float vec_dot(Vector4 v, Vector4 w) {
    // Minkowski metric: -+ + +
    return -v.v0 * w.v0 + v.v1 * w.v1 + v.v2 * w.v2 + v.v3 * w.v3;
}

// ============================================================================
// Bivector Operations
// ============================================================================

inline Bivector biv_add(Bivector a, Bivector b) {
    return (Bivector){
        a.b01 + b.b01, a.b02 + b.b02, a.b12 + b.b12,
        a.b03 + b.b03, a.b13 + b.b13, a.b23 + b.b23
    };
}

inline Bivector biv_smul(float s, Bivector b) {
    return (Bivector){
        s * b.b01, s * b.b02, s * b.b12,
        s * b.b03, s * b.b13, s * b.b23
    };
}

inline Bivector vec_wedge(Vector4 v, Vector4 w) {
    return (Bivector){
        v.v0 * w.v1 - v.v1 * w.v0,  // b01
        v.v0 * w.v2 - v.v2 * w.v0,  // b02
        v.v1 * w.v2 - v.v2 * w.v1,  // b12
        v.v0 * w.v3 - v.v3 * w.v0,  // b03
        v.v1 * w.v3 - v.v3 * w.v1,  // b13
        v.v2 * w.v3 - v.v3 * w.v2   // b23
    };
}

inline Vector4 biv_dot_vec(Bivector b, Vector4 v) {
    return (Vector4){
        b.b01 * v.v1 + b.b02 * v.v2 + b.b03 * v.v3,
        b.b01 * v.v0 + b.b12 * v.v2 + b.b13 * v.v3,
        b.b02 * v.v0 - b.b12 * v.v1 + b.b23 * v.v3,
        b.b03 * v.v0 - b.b13 * v.v1 - b.b23 * v.v2
    };
}

inline Bivector biv_dual(Bivector b) {
    // Hodge dual in 4D Minkowski: *B
    return (Bivector){
        -b.b23, b.b13, b.b03,
        -b.b12, b.b02, -b.b01
    };
}

// ============================================================================
// Particle Operations
// ============================================================================

inline Particle particle_add(Particle a, Particle b) {
    return (Particle){vec_add(a.position, b.position), vec_add(a.momentum, b.momentum)};
}

inline Particle particle_smul(float s, Particle p) {
    return (Particle){vec_smul(s, p.position), vec_smul(s, p.momentum)};
}

// ============================================================================
// Coordinate Systems
// ============================================================================

inline float copysign_f(float x, float y) {
    return y >= 0.0f ? fabs(x) : -fabs(x);
}

inline Vector4 spherical_from_cartesian(Vector4 pos) {
    float t = pos.v0;
    float x = pos.v1;
    float y = pos.v2;
    float z = pos.v3;
    float r = sqrt(x*x + y*y + z*z);
    float theta = r > 0.0f ? acos(z / r) : 0.0f;
    float phi = atan2(y, x);
    return (Vector4){t, r, theta, phi};
}

inline Vector4 spheroidal_from_cartesian(float a, Vector4 pos) {
    float t = pos.v0;
    float x = pos.v1;
    float y = pos.v2;
    float z = pos.v3;
    float phi = atan2(y, x);
    float rho = sqrt(x*x + y*y);
    float d1 = sqrt((rho + a) * (rho + a) + z*z);
    float d2 = sqrt((rho - a) * (rho - a) + z*z);
    float coshMu = (d1 + d2) / (2.0f * a);
    float mu = acosh(coshMu);
    float cos2Nu = 1.0f - (d1 - d2) * (d1 - d2) / (4.0f * a * a);
    float cosNuSign = copysign_f(sqrt(max(cos2Nu, 0.0f)), z);
    float nu = acos(cosNuSign);
    return (Vector4){t, mu, nu, phi};
}

// ============================================================================
// Spheroidal Basis Vectors
// ============================================================================

inline Vector4 spheroidal_basis_emu(float sinhMu, float coshMu, float sinNu, float cosNu, float sinPhi, float cosPhi) {
    float denom = sqrt(sinhMu * sinhMu + cosNu * cosNu);
    float invD = 1.0f / denom;
    return (Vector4){0.0f, sinhMu * sinNu * cosPhi * invD, sinhMu * sinNu * sinPhi * invD, coshMu * cosNu * invD};
}

inline Vector4 spheroidal_basis_enu(float sinhMu, float coshMu, float sinNu, float cosNu, float sinPhi, float cosPhi) {
    float denom = sqrt(sinhMu * sinhMu + cosNu * cosNu);
    float invD = 1.0f / denom;
    return (Vector4){0.0f, coshMu * cosNu * cosPhi * invD, coshMu * cosNu * sinPhi * invD, -sinhMu * sinNu * invD};
}

inline Vector4 spheroidal_basis_phi(float sinPhi, float cosPhi) {
    return (Vector4){0.0f, -sinPhi, cosPhi, 0.0f};
}

inline Vector4 spheroidal_basis_t() {
    return (Vector4){1.0f, 0.0f, 0.0f, 0.0f};
}

// ============================================================================
// Doran Metric Functions
// ============================================================================

inline float doran_beta(float coshMu, float sinNu) {
    return atanh(sinNu / coshMu);
}

inline Vector4 doran_vector_v(float beta, Vector4 that, Vector4 phihat) {
    return vec_add(vec_smul(cosh(beta), that), vec_smul(sinh(beta), phihat));
}

inline Vector4 doran_position_gauge(float sinhMu, Vector4 emuhat, float a, Vector4 doranV, Vector4 vecArg) {
    float rootFactor = sqrt(2.0f * sinhMu / a / (1.0f + sinhMu * sinhMu));
    float dotProduct = vec_dot(vecArg, doranV);
    return vec_add(vecArg, vec_smul(rootFactor * dotProduct, emuhat));
}

inline Bivector doran_rotation_gauge(float sinhMu, float cosNu,
    Vector4 muhat, Vector4 nuhat, Vector4 phihat, Vector4 that,
    float beta, Vector4 doranV, float a, Vector4 vecArg) {

    float alpha = -sqrt(2.0f * sinhMu / (a * (sinhMu * sinhMu + cosNu * cosNu)));

    float argDotMu = vec_dot(vecArg, muhat);
    float argDotNu = vec_dot(vecArg, nuhat);
    float argDotPhi = vec_dot(vecArg, phihat);

    Bivector muterm = vec_wedge(muhat, doranV);
    Bivector nuterm = vec_wedge(nuhat, doranV);
    Bivector phiterm = vec_wedge(phihat, that);

    float commonScalar = a * sinhMu;
    float commonPseudo = a * cosNu;
    float commonDenom = commonScalar * commonScalar + commonPseudo * commonPseudo;

    float muScalar = (commonScalar * commonScalar - commonPseudo * commonPseudo) /
                     (commonDenom * commonDenom);
    float muPseudo = (2.0f * commonScalar * commonPseudo) /
                     (commonDenom * commonDenom);

    // mu contribution
    Bivector muContrib = biv_smul(
        (1.0f / alpha) * argDotMu / (commonDenom * commonDenom),
        biv_add(biv_smul(muScalar, muterm), biv_smul(muPseudo, biv_dual(muterm)))
    );

    // nu contribution
    Bivector nuContrib = biv_smul(
        (-alpha) * argDotNu / commonDenom,
        biv_add(biv_smul(commonScalar, nuterm), biv_smul(commonPseudo, biv_dual(nuterm)))
    );

    // phi contribution
    Bivector phiContrib = biv_smul(
        (-alpha / cosh(beta)) * argDotPhi / commonDenom,
        biv_add(biv_smul(commonScalar, phiterm), biv_smul(commonPseudo, biv_dual(phiterm)))
    );

    return biv_add(biv_add(muContrib, nuContrib), phiContrib);
}

inline Particle doran_rhs(Particle data, float a) {
    Vector4 coords = spheroidal_from_cartesian(a, data.position);
    float mu = coords.v1;
    float nu = coords.v2;
    float phi = coords.v3;

    float sinhMu = sinh(mu);
    float coshMu = cosh(mu);
    float sinNu = sin(nu);
    float cosNu = cos(nu);
    float sinPhi = sin(phi);
    float cosPhi = cos(phi);

    Vector4 emu = spheroidal_basis_emu(sinhMu, coshMu, sinNu, cosNu, sinPhi, cosPhi);
    Vector4 enu = spheroidal_basis_enu(sinhMu, coshMu, sinNu, cosNu, sinPhi, cosPhi);
    Vector4 ephi = spheroidal_basis_phi(sinPhi, cosPhi);
    Vector4 et = spheroidal_basis_t();

    float beta = doran_beta(coshMu, sinNu);
    Vector4 V = doran_vector_v(beta, et, ephi);

    Vector4 xprime = doran_position_gauge(sinhMu, emu, a, V, data.momentum);
    Bivector omega = doran_rotation_gauge(sinhMu, cosNu, emu, enu, ephi, et, beta, V, a, data.momentum);
    Vector4 kprime = vec_neg(biv_dot_vec(omega, data.momentum));

    return (Particle){xprime, kprime};
}

// ============================================================================
// RK4 Integration
// ============================================================================

inline Particle rk4_step(Particle y, float dt, float t, float a) {
    Particle k1 = doran_rhs(y, a);
    Particle k2 = doran_rhs(particle_add(y, particle_smul(dt / 2.0f, k1)), a);
    Particle k3 = doran_rhs(particle_add(y, particle_smul(dt / 2.0f, k2)), a);
    Particle k4 = doran_rhs(particle_add(y, particle_smul(dt, k3)), a);

    // y + (dt/6) * (k1 + 2*k2 + 2*k3 + k4)
    Particle sum = particle_add(k1, particle_add(
        particle_smul(2.0f, k2),
        particle_add(particle_smul(2.0f, k3), k4)
    ));
    return particle_add(y, particle_smul(dt / 6.0f, sum));
}

inline ODEData rk4_step_ode(ODEData ode, float a) {
    Particle newValue = rk4_step(ode.value, ode.dparam, ode.param, a);
    return (ODEData){newValue, ode.param + ode.dparam, ode.dparam};
}

inline ODEData adjust_stepsize(float dparam0, ODEData data) {
    float pt = fabs(data.value.momentum.v0);
    float ratio = pt < 1e-10f ? 1e-10f : pt;
    return (ODEData){data.value, data.param, dparam0 / ratio};
}

inline bool escaped(float radius, ODEData data) {
    Vector4 spherical = spherical_from_cartesian(data.value.position);
    return spherical.v1 >= radius;
}

inline bool param_exceeded(float maxParam, ODEData data) {
    return data.param >= maxParam;
}

inline bool step_ratio_exceeded(float maxRatio, float dparam0, ODEData data) {
    return dparam0 / data.dparam >= maxRatio;
}

// ============================================================================
// Colormap
// ============================================================================

inline bool test_stripe(float angle, float stripeInterval, float stripeHalfWidthRatio) {
    float stripeNumber = angle / stripeInterval;
    float stripeRemainder = stripeNumber - round(stripeNumber);
    return fabs(stripeRemainder) <= stripeHalfWidthRatio;
}

inline RGB spherical_colormap(float escapeRadius, ODEData data) {
    Vector4 pos = data.value.position;
    Vector4 spherical = spherical_from_cartesian(pos);

    if (spherical.v1 >= escapeRadius) {
        float theta = spherical.v2;
        float phi = spherical.v3;

        float stripeHalfWidth = 0.05f;
        float stripeInterval = M_PI_F / 18.0f;  // 10 degrees

        if (test_stripe(theta, stripeInterval, stripeHalfWidth) ||
            test_stripe(phi, stripeInterval, stripeHalfWidth)) {
            return (RGB){0, 0, 0};  // black grid
        } else {
            float y = pos.v2;
            float z = pos.v3;
            if (y > 0.0f && z > 0.0f) return (RGB){200, 100, 100};       // red quadrant
            else if (y < 0.0f && z > 0.0f) return (RGB){100, 200, 100};  // green quadrant
            else if (y > 0.0f && z < 0.0f) return (RGB){100, 100, 200};  // blue quadrant
            else return (RGB){200, 200, 100};                            // yellow quadrant
        }
    } else {
        return (RGB){100, 200, 200};  // cyan = didn't escape
    }
}

// ============================================================================
// Camera / Ray Generation
// ============================================================================

inline ODEData pixel_initial_data(CameraParams cam, uint pixelIdx) {
    uint w = cam.width;
    uint h = cam.height;

    float da = cam.hFov / (float)w;

    uint heightIdx = pixelIdx / w;
    uint widthIdx = pixelIdx - heightIdx * w;

    float widthAngle = ((float)(widthIdx) - (float)(w / 2) + 0.5f) * da;
    float heightAngle = ((float)(heightIdx) - (float)(h / 2) + 0.5f) * da;

    // Simple rotation (approximation without full rotor algebra for brevity)
    float cosW = cos(widthAngle);
    float sinW = sin(widthAngle);
    float cosH = cos(heightAngle);
    float sinH = sin(heightAngle);

    // Central momentum: out - time (null vector pointing inward)
    // out direction is (-1, 0, 0), time direction is (1, 0, 0, 0) in 4-vector form
    // After rotation, the spatial part becomes (-cosW*cosH, sinW*cosH, sinH)
    Vector4 rotatedOut = (Vector4){
        0.0f,
        -cosW * cosH,
        sinW * cosH,
        sinH
    };

    // momentum = rotatedOut - timeDir = (0, spatial) - (1, 0, 0, 0) = (-1, spatial)
    Vector4 momentum = (Vector4){
        -1.0f,  // time component
        rotatedOut.v1,
        rotatedOut.v2,
        rotatedOut.v3
    };

    Particle particle = (Particle){cam.position, momentum};
    return (ODEData){particle, 0.0f, cam.dparam};
}

// ============================================================================
// Main Raytracing Kernel
// ============================================================================

kernel void raytrace_doran(
    device RGB* output [[buffer(0)]],
    constant CameraParams& cam [[buffer(1)]],
    constant DoranParams& params [[buffer(2)]],
    uint2 gid [[thread_position_in_grid]]
) {
    uint x = gid.x;
    uint y = gid.y;
    if (x >= cam.width || y >= cam.height) return;

    uint pixelIdx = y * cam.width + x;
    ODEData ode = pixel_initial_data(cam, pixelIdx);

    float dparam0 = cam.dparam;

    // Integration loop
    for (uint step = 0; step < params.maxSteps; step++) {
        // Check stop conditions
        if (escaped(params.extractRadius, ode) ||
            param_exceeded(params.maxParam, ode) ||
            step_ratio_exceeded(params.maxStepRatio, dparam0, ode)) {
            break;
        }

        // RK4 step
        ode = rk4_step_ode(ode, params.spinParam);

        // Adaptive stepsize
        ode = adjust_stepsize(dparam0, ode);
    }

    // Colormap
    output[pixelIdx] = spherical_colormap(params.extractRadius, ode);
}

// Flat spacetime kernel for comparison
kernel void raytrace_flat(
    device RGB* output [[buffer(0)]],
    constant CameraParams& cam [[buffer(1)]],
    constant float& extractRadius [[buffer(2)]],
    constant float& maxParam [[buffer(3)]],
    constant uint& maxSteps [[buffer(4)]],
    uint2 gid [[thread_position_in_grid]]
) {
    uint x = gid.x;
    uint y = gid.y;
    if (x >= cam.width || y >= cam.height) return;

    uint pixelIdx = y * cam.width + x;
    ODEData ode = pixel_initial_data(cam, pixelIdx);

    // In flat spacetime: dx = p, dp = 0
    for (uint step = 0; step < maxSteps; step++) {
        if (escaped(extractRadius, ode) || param_exceeded(maxParam, ode)) break;

        // Simple Euler step for flat space (momentum constant)
        ode.value.position = vec_add(ode.value.position, vec_smul(ode.dparam, ode.value.momentum));
        ode.param += ode.dparam;
    }

    output[pixelIdx] = spherical_colormap(extractRadius, ode);
}
"

/-- Emit the complete raytracer kernel to a file. -/
elab "#emit_raytracer_msl " path:str : command => do
  let pathStr := path.getString
  IO.FS.writeFile pathStr raytracerKernelMSL
  logInfo m!"Wrote raytracer Metal shader to {pathStr}"

end TetraGrayer.Codegen
