/-
TetraGrayer Documentation - Front Page
-/
import VersoBlog

open Verso Genre Blog

#doc (Page) "TetraGrayer" =>

# A General Relativistic Ray Tracer in Lean 4

TetraGrayer is a Lean 4 port of [tetra-gray](https://github.com/muphrid15/tetra-gray),
a general relativistic ray tracer that uses Clifford algebra and tetrads to trace
light through curved spacetime around black holes.

![Gravitational lensing around a Kerr black hole](static/doran-full.png)

# Gravitational Lensing

The classic view of gravitational lensing around a Kerr (rotating) black hole.
The colored quadrants show light rays from different celestial directions.
The cyan sphere at center is the event horizon. Notice how light from behind
the black hole bends around it, creating the characteristic Einstein ring.

# Accretion Disk

![Accretion disk rendering](static/demo-doran-disk.png)

A thin accretion disk around the black hole with temperature-based coloring
(Doppler shift from orbital motion). The disk appears warped due to
gravitational lensing—you can see both the top and bottom of the disk
simultaneously.

# Checkerboard Distortion

![Checkerboard celestial sphere](static/demo-doran-checker.png)

A checkerboard pattern on the celestial sphere visualizes how the black hole's
gravity distorts the background. The warping is strongest near the photon sphere.

# Starfield

![Starfield with gravitational lensing](static/demo-doran-stars.png)

Random stars across the celestial sphere, showing how gravitational lensing
affects point sources. Stars near the black hole appear duplicated and distorted.

# Traversable Wormhole

![Wormhole spacetime](static/demo-wormhole.png)

A Morris-Thorne traversable wormhole connecting two regions of spacetime.
You can see the "other side" of the universe through the throat.

# Features

* _Physically accurate_: Traces null geodesics through Kerr (rotating) black hole spacetime
* _GPU accelerated_: Metal compute shaders with 241x speedup over CPU
* _Type safe_: Leverages Lean's dependent types for dimensional correctness
* _Multiple spacetimes_: Doran (Kerr), flat, and traversable wormhole

# Performance

| Mode | Resolution | Time |
|------|-----------|------|
| CPU (Lean) | 1280x720 | ~24s |
| GPU (Metal) | 1280x720 | ~0.1s |
| _Speedup_ | | _241x_ |

# Quick Start

```
lake build
./.lake/build/bin/tetragrayer demos
```

# Code

See {page_link codeDocs}[Code Examples] for the Lean implementation.
