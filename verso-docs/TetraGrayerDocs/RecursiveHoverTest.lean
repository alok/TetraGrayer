set_option doc.verso true

/-!
# Recursive Hover Test

This module tests recursive hover functionality.
-/

/-- Inner type -/
structure Inner where
  a : Int

/-- Outer type -/
structure Outer where
  i : Inner

/-- A function using Outer -/
structure D1 where
  x : Nat

structure D2 (d1 : D1) where
  y : Nat

structure D3 (d1 : D1) (d2 : D2 d1) where
  z : Nat

structure D4 (d1 : D1) (d2 : D2 d1) (d3 : D3 d1 d2) where
  w : Nat

def depTest (d1 : D1) (d2 : D2 d1) (d3 : D3 d1 d2) (d4 : D4 d1 d2 d3) : Nat := d4.w

def listTest (l : List Nat) : List Nat := l

def d2Test (d : D2 (D1.mk 0)) : Nat := d.y

def arrowTest : D1 → Type := fun _ => Nat

def projTest (d4 : D4 (D1.mk 0) (D2.mk 0) (D3.mk 0)) : Nat := d4.w
