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

## Features

* _Physically accurate_: Traces null geodesics through Kerr (rotating) black hole spacetime
* _GPU accelerated_: Metal compute shaders with 241x speedup over CPU
* _Type safe_: Leverages Lean's dependent types for dimensional correctness
* _Multiple spacetimes_: Doran (Kerr), flat, and traversable wormhole

## Quick Start

```
# Build
lake build

# Render on GPU (fast)
./.lake/build/bin/tetragrayer metal-ffi

# Render all demos
./.lake/build/bin/tetragrayer demos

# CPU rendering
./.lake/build/bin/tetragrayer doran
```

## Gallery

See the {page_link TetraGrayerDocs.Gallery}[Gallery] for rendered images.

## Code Examples

See {page_link TetraGrayerDocs.Code}[Code Examples] for type-checked Lean code.
