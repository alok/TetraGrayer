-- Lightweight helpers over Lean's `Vector α n` for small fixed-size geometry.
-- This avoids partial indexing by using `Fin n` and small constructors.

import TetraGrayer.Core.Scalar

namespace TetraGrayer
namespace Core

abbrev Vec4 := Vector ℝ 4
abbrev Vec3 := Vector ℝ 3

@[inline] def tOf (v : Vec4) : ℝ := v.get ⟨0, by decide⟩
@[inline] def xOf4 (v : Vec4) : ℝ := v.get ⟨1, by decide⟩
@[inline] def yOf4 (v : Vec4) : ℝ := v.get ⟨2, by decide⟩
@[inline] def zOf4 (v : Vec4) : ℝ := v.get ⟨3, by decide⟩

@[inline] def xOf3 (v : Vec3) : ℝ := v.get ⟨0, by decide⟩
@[inline] def yOf3 (v : Vec3) : ℝ := v.get ⟨1, by decide⟩
@[inline] def zOf3 (v : Vec3) : ℝ := v.get ⟨2, by decide⟩

/-- Construct a 4-vector. -/
@[inline] def vec4 (a b c d : ℝ) : Vec4 :=
  Vector.ofFn (fun i =>
    match i.val with
    | 0 => a
    | 1 => b
    | 2 => c
    | _ => d)

/-- Construct a 3-vector. -/
@[inline] def vec3 (x y z : ℝ) : Vec3 :=
  Vector.ofFn (fun i =>
    match i.val with
    | 0 => x
    | 1 => y
    | _ => z)

@[inline] def vget (v : Vector α n) (i : Fin n) : α := v.get i

@[inline] def vmap (f : α → β) (v : Vector α n) : Vector β n :=
  Vector.ofFn (fun i => f (v.get i))

@[inline] def vzipWith (f : α → β → γ) (a : Vector α n) (b : Vector β n) : Vector γ n :=
  Vector.ofFn (fun i => f (a.get i) (b.get i))

@[inline] def vadd (a b : Vector ℝ n) : Vector ℝ n := vzipWith (·+·) a b

@[inline] def vsub (a b : Vector ℝ n) : Vector ℝ n := vzipWith (·-·) a b

@[inline] def vsmul (s : ℝ) (v : Vector ℝ n) : Vector ℝ n := vmap (fun x => s * x) v

/-- Euclidean dot product for 3-vectors. -/
@[inline] def dot3 (a b : Vec3) : ℝ :=
  let ax := xOf3 a; let ay := yOf3 a; let az := zOf3 a
  let bx := xOf3 b; let bY := yOf3 b; let bz := zOf3 b
  ax * bx + ay * bY + az * bz

/-- Minkowski dot product with signature (-,+,+,+) on 4-vectors. -/
@[inline] def minkowskiDot (a b : Vec4) : ℝ :=
  let tA := tOf a; let xA := xOf4 a; let yA := yOf4 a; let zA := zOf4 a
  let tB := tOf b; let xB := xOf4 b; let yB := yOf4 b; let zB := zOf4 b
  (-tA * tB) + (xA * xB + yA * yB + zA * zB)

/-- Euclidean norm of spatial part of a 4-vector. -/
@[inline] def spatialNorm (v : Vec4) : ℝ :=
  let x := xOf4 v; let y := yOf4 v; let z := zOf4 v
  Float.sqrt (x*x + y*y + z*z)

/-- Normalize the spatial part of a 4-vector, time component unchanged. -/
def normalizeSpatial (v : Vec4) : Vec4 :=
  let t := tOf v
  let x := xOf4 v; let y := yOf4 v; let z := zOf4 v
  let s := spatialNorm v
  let n := if s < 1e-12 then 1e-12 else s
  vec4 t (x / n) (y / n) (z / n)

end Core
end TetraGrayer
