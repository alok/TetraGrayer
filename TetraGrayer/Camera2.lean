/-
Camera model using Clifford algebra rotors for proper 4D orientation.

Ported from tetra-gray's image_id.cuh (ImageInitialDataSolver).

The camera points in -x direction by default, with width along +y and height along +z.
This convention is chosen so positive rotation angles correspond to increased pixel indices.
The 0th pixel is in the top-left corner.
-/

import TetraGrayer.Core.Scalar
import TetraGrayer.Core.Clifford
import TetraGrayer.Core.Particle

namespace TetraGrayer

open Core

/-- Camera parameters for ray tracing. -/
structure CameraParams where
  position : CliffordVector      -- camera center position (t,x,y,z)
  orientation : Versor           -- camera orientation rotor
  hFov : ℝ                       -- horizontal field of view in radians
  width : Nat                    -- image width in pixels
  height : Nat                   -- image height in pixels
  dparam : ℝ                     -- initial step size for integration
deriving Repr

namespace CameraParams

/-- Default camera at origin, looking along -x, with 90° FOV. -/
def default (w h : Nat) : CameraParams :=
  { position := CliffordVector.mk4 0.0 20.0 0.0 0.0
  , orientation := Versor.one
  , hFov := π / 2.0
  , width := w
  , height := h
  , dparam := 0.05 }

end CameraParams

/-- Baseline direction vectors for the camera.

Camera canonically points in -x direction:
- out = -x (forward)
- left = +y (image width direction)
- up = +z (image height direction)
- time = +t
-/
def baselineTimeDir : CliffordVector := CliffordVector.mk4 1.0 0.0 0.0 0.0
def baselineOutDir : CliffordVector := CliffordVector.mk4 0.0 (-1.0) 0.0 0.0
def baselineLeftDir : CliffordVector := CliffordVector.mk4 0.0 0.0 1.0 0.0
def baselineUpDir : CliffordVector := CliffordVector.mk4 0.0 0.0 0.0 1.0

/-- Compute initial particle data for a pixel.

Creates a null geodesic (photon) starting at the camera position with
momentum pointing towards the pixel direction on the celestial sphere.

Parameters:
- cam: camera parameters
- pixelIdx: linear pixel index (row-major, 0 at top-left)

Returns: ODEData Particle ready for integration.
-/
def pixelInitialData (cam : CameraParams) (pixelIdx : Nat) : ODEData Particle :=
  let w := cam.width
  let h := cam.height

  -- Angular step per pixel
  let da := cam.hFov / Float.ofNat w

  -- Convert linear index to (width_idx, height_idx)
  let heightIdx := pixelIdx / w
  let widthIdx := pixelIdx - heightIdx * w

  -- Width angle: positive = looking left
  -- Center of pixel, 0.5 offset so centerline is pixel boundary
  let widthAngle := (Float.ofInt (Int.ofNat widthIdx - Int.ofNat (w / 2)) + 0.5) * da

  -- Height angle: positive = looking up
  let heightAngle := (Float.ofInt (Int.ofNat heightIdx - Int.ofNat (h / 2)) + 0.5) * da

  -- Create rotors for pixel direction
  -- Left-right rotation in the out-left plane
  let rotorLR := simpleRotorFromAngle baselineOutDir baselineLeftDir widthAngle
  -- Up-down rotation in the up-out plane
  let rotorUD := simpleRotorFromAngle baselineUpDir baselineOutDir heightAngle

  -- Central four-momentum: null vector pointing "out" minus "time"
  -- For photons: p^μ p_μ = 0, with p^t = -1 (ingoing to future)
  let centralMom := baselineOutDir - baselineTimeDir

  -- Compose rotors: orientation * left-right * up-down
  -- Order matters: left-right first in body coords → leftmost in product
  let totalRotor := cam.orientation * rotorLR * rotorUD

  -- Apply rotor to get final momentum direction
  let momentum := bilinearMultiply totalRotor centralMom

  ODEData.ofData (Particle.ofPosMom cam.position momentum) 0.0 cam.dparam

/-- Compute pixel data from 2D coordinates. -/
def pixelInitialData2D (cam : CameraParams) (x y : Nat) : ODEData Particle :=
  pixelInitialData cam (y * cam.width + x)

end TetraGrayer
