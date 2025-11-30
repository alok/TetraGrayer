/-
TetraGrayer Documentation - Gallery
-/
import VersoBlog

open Verso Genre Blog

#doc (Page) "Gallery" =>

# Rendered Images

All images rendered using TetraGrayer on Apple M1 GPU via Metal.

## Gravitational Lensing

The classic view of gravitational lensing around a Kerr black hole.
The colored quadrants show light rays from different celestial directions.

![Gravitational lensing around a Kerr black hole](static/doran-full.png)

The cyan sphere at the center represents the event horizon.
Notice how light from behind the black hole is bent around it,
creating the characteristic "Einstein ring" effect.

## Accretion Disk

A thin accretion disk around the black hole, rendered with
temperature-based coloring (Doppler shift from orbital motion).

![Accretion disk rendering](static/demo-doran-disk.png)

## Checkerboard Pattern

A checkerboard celestial sphere helps visualize the distortion
of the background by the black hole's gravity.

![Checkerboard celestial sphere](static/demo-doran-checker.png)

## Starfield

Random stars scattered across the celestial sphere,
showing how gravitational lensing affects point sources.

![Starfield with gravitational lensing](static/demo-doran-stars.png)

## Traversable Wormhole

A Morris-Thorne traversable wormhole connecting two regions of spacetime.

![Wormhole spacetime](static/demo-wormhole.png)

## Performance

| Mode | Resolution | Time |
|------|-----------|------|
| CPU (Lean) | 1280x720 | ~24s |
| GPU (Metal) | 1280x720 | ~0.1s |
| _Speedup_ | | _241x_ |
