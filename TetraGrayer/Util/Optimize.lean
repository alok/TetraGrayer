/-
Compile-time optimization utilities inspired by SciLean.

Provides `rewrite_by` syntax and `def_optimize` command for creating
optimized versions of definitions that get substituted by the compiler.
-/
import Lean

namespace TetraGrayer.Util

open Lean.Parser.Tactic.Conv
open Lean.Elab.Tactic.Conv
open Lean Elab Term Meta

/-- Elaborate a conv rewrite, returning the rewritten expression and equality proof. -/
def elabConvRewrite (e : Expr) (stx : TSyntax `conv) : TermElabM (Expr × Expr) := do
  let (rhs, eq) ← mkConvGoalFor e

  let goals ← Tactic.run eq.mvarId! do
    let (lhsNew, proof) ← convert e (Tactic.evalTactic stx)
    updateLhs lhsNew proof
    return ()

  if goals.isEmpty then
    throwError "bug in elabConvRewrite: no goals"

  if goals.length > 1 then
    let goalStrs ← goals.mapM fun g => do pure s!"{← ppExpr (← g.getType)}"
    throwError "unsolved goals in rewrite: {goalStrs}"

  goals[0]!.refl

  return (← instantiateMVars rhs, ← instantiateMVars eq)

/-- Rewrite a term using conv tactics: `expr rewrite_by conv_tactic` -/
syntax:1 term "rewrite_by" convSeq : term

elab_rules : term
| `($x:term rewrite_by $rw:convSeq) => do
  let x ← elabTerm x none
  synthesizeSyntheticMVarsNoPostponing
  let (x', _eq) ← elabConvRewrite x (← `(conv| ($rw)))
  return x'

/-- Create an optimized version of a definition and register it with csimp.

Usage:
```
def myFn (x : Nat) := x + 0

def_optimize myFn by simp
-- Creates myFn.optimized with body `x`
-- Registers csimp rule: myFn = myFn.optimized
```
-/
elab "def_optimize" f:ident "by " t:convSeq : command => do
  let info ← Lean.getConstInfoDefn f.getId

  Command.liftTermElabM do
    let (value', eq) ←
      lambdaTelescope info.value fun xs val => do
        let (value', eq) ← elabConvRewrite val (← `(conv| ($t)))
        pure (← mkLambdaFVars xs value', ← mkLambdaFVars xs eq)

    let optName := info.name.append (.mkSimple "optimized")

    let optimizedDef : DefinitionVal := {
      name  := optName
      type  := info.type
      value := value'
      hints := info.hints
      safety := info.safety
      levelParams := info.levelParams
    }

    addAndCompile (.defnDecl optimizedDef)

    -- Create equality theorem
    let eqType ← forallTelescope info.type fun xs _ => do
      let lhs ← mkAppOptM info.name (xs.map .some)
      let rhs ← mkAppOptM optName (xs.map .some)
      mkForallFVars xs (← mkEq lhs rhs)

    let thmName := info.name.append (.mkSimple "optimize_rule")

    let eqTheorem : TheoremVal := {
      name  := thmName
      type  := eqType
      value := (← instantiateMVars eq)
      levelParams := info.levelParams
    }

    -- Register with csimp
    let csimpEntry : Compiler.CSimp.Entry := {
      fromDeclName := info.name
      toDeclName := optName
      thmName := thmName
    }

    addDecl (.thmDecl eqTheorem)
    Compiler.CSimp.ext.add csimpEntry

    logInfo m!"Created {optName} and registered csimp rule"

end TetraGrayer.Util
