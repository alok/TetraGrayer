#!/usr/bin/env python3
"""
Metal GPU rendering helper for TetraGrayer.
Called from Lean via subprocess.

Usage: python3 render_helper.py <width> <height> <spinParam> <extractRadius>
                                 <maxParam> <maxStepRatio> <maxSteps> <dparam0>
                                 <camT> <camX> <camY> <camZ> <hFov> <outputPath>
"""

import sys
import ctypes
from pathlib import Path

def main():
    if len(sys.argv) != 15:
        print(f"Usage: {sys.argv[0]} <14 args>", file=sys.stderr)
        print(f"Got {len(sys.argv) - 1} args", file=sys.stderr)
        sys.exit(1)

    # Parse arguments
    width = int(sys.argv[1])
    height = int(sys.argv[2])
    spinParam = float(sys.argv[3])
    extractRadius = float(sys.argv[4])
    maxParam = float(sys.argv[5])
    maxStepRatio = float(sys.argv[6])
    maxSteps = int(sys.argv[7])
    dparam0 = float(sys.argv[8])
    camT = float(sys.argv[9])
    camX = float(sys.argv[10])
    camY = float(sys.argv[11])
    camZ = float(sys.argv[12])
    hFov = float(sys.argv[13])
    outputPath = sys.argv[14]

    # Find the dylib
    script_dir = Path(__file__).parent
    dylib_path = script_dir / "libmetal_raytracer.dylib"

    if not dylib_path.exists():
        print(f"Metal library not found: {dylib_path}", file=sys.stderr)
        print("Build with: make -C ffi/metal", file=sys.stderr)
        sys.exit(1)

    # Load the Metal library
    lib = ctypes.CDLL(str(dylib_path))

    # Initialize Metal
    lib.tetra_metal_init.restype = ctypes.c_int
    result = lib.tetra_metal_init()
    if result != 0:
        print(f"Metal init failed with code: {result}", file=sys.stderr)
        sys.exit(1)

    print(f"Metal initialized successfully")
    print(f"Rendering {width}x{height}...")

    # Allocate output buffer
    buf_size = width * height * 4
    output = (ctypes.c_uint8 * buf_size)()

    # Setup function signature
    lib.tetra_metal_render.argtypes = [
        ctypes.c_uint32, ctypes.c_uint32,  # width, height
        ctypes.c_float, ctypes.c_float, ctypes.c_float, ctypes.c_float,  # spin, extract, maxP, maxR
        ctypes.c_uint32, ctypes.c_float,  # maxSteps, dparam0
        ctypes.c_float, ctypes.c_float, ctypes.c_float, ctypes.c_float,  # cam pos
        ctypes.c_float,  # hFov
        ctypes.POINTER(ctypes.c_uint8)  # output
    ]
    lib.tetra_metal_render.restype = ctypes.c_int

    # Render
    result = lib.tetra_metal_render(
        width, height,
        spinParam, extractRadius, maxParam, maxStepRatio,
        maxSteps, dparam0,
        camT, camX, camY, camZ,
        hFov,
        output
    )

    if result != 0:
        print(f"Metal render failed with code: {result}", file=sys.stderr)
        sys.exit(1)

    print("GPU render complete, writing PPM...")

    # Write PPM
    with open(outputPath, 'w') as f:
        f.write(f'P3\n{width} {height}\n255\n')
        for y in range(height):
            line = ''
            for x in range(width):
                idx = (y * width + x) * 4
                r, g, b = output[idx], output[idx+1], output[idx+2]
                line += f'{r} {g} {b} '
            f.write(line + '\n')

    # Cleanup
    lib.tetra_metal_cleanup()

    print(f"Done! Output: {outputPath}")

if __name__ == "__main__":
    main()
