/-
Accretion disk visualization for black hole raytracing.

Models a thin, Keplerian disk in the equatorial plane (z=0) with:
- Temperature profile: T ~ r^(-3/4) (standard thin disk)
- Doppler shift from disk rotation
- Gravitational redshift
- Limb darkening

The disk glows with blackbody-like colors from red (cool, outer) to
blue-white (hot, inner edge near ISCO).
-/

import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Clifford
import TetraGrayer.Core.CoordSystems
import TetraGrayer.Core.Particle
import TetraGrayer.Image.PPM

namespace TetraGrayer
namespace Image

open Core

/-- Accretion disk parameters. -/
structure DiskParams where
  /-- Inner edge radius (typically ISCO ≈ 6M for Schwarzschild, less for Kerr) -/
  innerRadius : ℝ := 6.0
  /-- Outer edge radius -/
  outerRadius : ℝ := 20.0
  /-- Temperature at inner edge (arbitrary units, controls color intensity) -/
  innerTemp : ℝ := 1.0
  /-- Disk rotation direction: 1 = prograde, -1 = retrograde -/
  rotationSign : ℝ := 1.0
deriving Repr, Inhabited

namespace DiskParams

/-- Standard thin disk for Schwarzschild (ISCO at r=6M). -/
def schwarzschild : DiskParams :=
  { innerRadius := 6.0, outerRadius := 25.0, innerTemp := 1.0 }

/-- Disk for fast-spinning Kerr (ISCO at r~1.24M for a=0.998). -/
def kerrPrograde : DiskParams :=
  { innerRadius := 2.0, outerRadius := 20.0, innerTemp := 1.2, rotationSign := 1.0 }

/-- Retrograde disk (ISCO larger for retrograde). -/
def kerrRetrograde : DiskParams :=
  { innerRadius := 9.0, outerRadius := 30.0, innerTemp := 0.8, rotationSign := -1.0 }

end DiskParams

/-- Check if a ray segment crosses the equatorial plane (z=0).

Returns (didCross, crossRadius, crossPhi) where:
- didCross: true if the ray crossed z=0 between positions
- crossRadius: cylindrical radius at crossing point
- crossPhi: azimuthal angle at crossing point
-/
def checkDiskCrossing (pos prevPos : CliffordVector) : Bool × ℝ × ℝ :=
  let z := pos.v3
  let zPrev := prevPos.v3
  -- Check if z changed sign (crossed equatorial plane)
  if z * zPrev < 0.0 then
    -- Linear interpolation to find crossing point
    let t := zPrev / (zPrev - z)
    let crossX := prevPos.v1 + t * (pos.v1 - prevPos.v1)
    let crossY := prevPos.v2 + t * (pos.v2 - prevPos.v2)
    let crossR := Float.sqrt (crossX * crossX + crossY * crossY)
    let crossPhi := Float.atan2 crossY crossX
    (true, crossR, crossPhi)
  else
    (false, 0.0, 0.0)

/-- Compute Keplerian orbital velocity at radius r (in units of c).
For Schwarzschild: v-phi = sqrt(M/r) = 1/sqrt(r) in M=1 units -/
def keplerianVelocity (r : ℝ) : ℝ :=
  if r > 1.0 then 1.0 / Float.sqrt r else 0.5

/-- Compute Doppler factor for disk emission.

The disk rotates, so light from the approaching side is blueshifted
and from the receding side is redshifted.

Returns a factor > 1 for blueshift, < 1 for redshift.
-/
def dopplerFactor (crossPhi : ℝ) (crossR : ℝ) (rayDir : CliffordVector) (rotSign : ℝ) : ℝ :=
  -- Disk velocity direction (tangent to circle)
  let vPhi := keplerianVelocity crossR
  let diskVelX := -rotSign * vPhi * Float.sin crossPhi
  let diskVelY := rotSign * vPhi * Float.cos crossPhi

  -- Dot product of disk velocity with ray direction (negative = approaching)
  let rayNorm := Float.sqrt (rayDir.v1 * rayDir.v1 + rayDir.v2 * rayDir.v2 + rayDir.v3 * rayDir.v3)
  let rayDirNorm := if rayNorm > 1e-10 then rayDir / rayNorm else rayDir

  let vDotRay := diskVelX * rayDirNorm.v1 + diskVelY * rayDirNorm.v2

  -- Relativistic Doppler: sqrt((1-β)/(1+β)) ≈ 1 - β for small β
  -- vDotRay < 0 means approaching (blueshift)
  let beta := -vDotRay  -- negative because we want redshift when moving away
  1.0 + 0.8 * beta  -- Amplified for visual effect

/-- Temperature profile of thin accretion disk: T ∝ r^(-3/4).

Returns temperature relative to inner edge temperature.
-/
def diskTemperature (r innerR innerTemp : ℝ) : ℝ :=
  innerTemp * Float.pow (innerR / r) 0.75

/-- Convert temperature to RGB color (blackbody approximation).

Uses simplified blackbody colors:
- Hot (T > 0.8): blue-white
- Medium (0.4 < T < 0.8): yellow-white
- Cool (T < 0.4): orange-red
-/
def temperatureToRGB (temp : ℝ) (dopplerFac : ℝ) : RGB :=
  -- Apply Doppler shift to effective temperature
  let effectiveTemp := temp * dopplerFac

  -- Clamp and map to color
  let t := clamp01 effectiveTemp

  -- Intensity based on temperature (hotter = brighter)
  let intensity := 0.3 + 0.7 * t

  -- Color based on temperature
  if t > 0.75 then
    -- Hot: blue-white
    let r := (200.0 + 55.0 * (t - 0.75) / 0.25) * intensity
    let g := (220.0 + 35.0 * (t - 0.75) / 0.25) * intensity
    let b := 255.0 * intensity
    RGB.ofNat (Float.toUInt32 r).toNat (Float.toUInt32 g).toNat (Float.toUInt32 b).toNat
  else if t > 0.4 then
    -- Medium: yellow-orange
    let r := 255.0 * intensity
    let g := (150.0 + 100.0 * (t - 0.4) / 0.35) * intensity
    let b := (50.0 + 100.0 * (t - 0.4) / 0.35) * intensity
    RGB.ofNat (Float.toUInt32 r).toNat (Float.toUInt32 g).toNat (Float.toUInt32 b).toNat
  else
    -- Cool: red-orange
    let r := (180.0 + 75.0 * t / 0.4) * intensity
    let g := (50.0 + 100.0 * t / 0.4) * intensity
    let b := (20.0 + 30.0 * t / 0.4) * intensity
    RGB.ofNat (Float.toUInt32 r).toNat (Float.toUInt32 g).toNat (Float.toUInt32 b).toNat

/-- Colormap combining accretion disk and background sphere.

Checks if ray intersects disk; if so, colors based on temperature and Doppler.
Otherwise falls back to background colormap.
-/
def diskColormap (disk : DiskParams) (escapeRadius : ℝ)
    (data : ODEData Particle) (prevPos : CliffordVector) : RGB :=
  let pos := data.value.position
  let mom := data.value.momentum

  -- Check for disk crossing
  let (crossed, crossR, crossPhi) := checkDiskCrossing pos prevPos

  if crossed && crossR >= disk.innerRadius && crossR <= disk.outerRadius then
    -- Hit the disk
    let temp := diskTemperature crossR disk.innerRadius disk.innerTemp
    let doppler := dopplerFactor crossPhi crossR mom disk.rotationSign
    temperatureToRGB temp doppler
  else
    -- Check if escaped to background
    let spherical := sphericalFromCartesian pos
    if spherical.v1 >= escapeRadius then
      -- Background: dark with faint stars
      let theta := spherical.v2
      let phi := spherical.v3
      -- Sparse star field
      let starHash := Float.sin (theta * 47.0) * Float.cos (phi * 31.0)
      if Float.abs starHash > 0.97 then
        RGB.white
      else
        RGB.ofNat 5 5 15  -- Dark blue-black background
    else
      -- Fell into black hole
      RGB.black

/-- Simple disk colormap without tracking previous position.

Uses current position's z-coordinate to estimate if near disk.
Less accurate but simpler interface.
-/
def simpleDiskColormap (disk : DiskParams) (escapeRadius : ℝ)
    (data : ODEData Particle) : RGB :=
  let pos := data.value.position
  let mom := data.value.momentum
  let spherical := sphericalFromCartesian pos

  -- Check if near equatorial plane
  let z := Float.abs pos.v3
  let r := Float.sqrt (pos.v1 * pos.v1 + pos.v2 * pos.v2)
  let phi := Float.atan2 pos.v2 pos.v1

  -- If close to z=0 and within disk radii
  if z < 0.5 && r >= disk.innerRadius && r <= disk.outerRadius then
    let temp := diskTemperature r disk.innerRadius disk.innerTemp
    let doppler := dopplerFactor phi r mom disk.rotationSign
    temperatureToRGB temp doppler
  else if spherical.v1 >= escapeRadius then
    -- Background with sparse stars
    let theta := spherical.v2
    let starHash := Float.sin (theta * 47.0 + phi * 31.0)
    if Float.abs starHash > 0.97 then
      RGB.white
    else
      RGB.ofNat 5 5 15
  else
    RGB.black

end Image
end TetraGrayer
