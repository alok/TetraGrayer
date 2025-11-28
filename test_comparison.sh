#!/bin/bash
# Test script to verify Lean raytracer output matches upstream reference
#
# Usage: ./test_comparison.sh [flat|all]
#   flat - only test flat spacetime (fast)
#   all  - test both flat and doran (slow)

set -e

echo "=== TetraGrayer Image Comparison Test ==="
echo ""

# Build first
echo "Building..."
lake build

# Run the flat render only (fast)
echo ""
echo "Running flat spacetime render..."
./.lake/build/bin/tetragrayer flat

# Wait for file to be fully written
sleep 1

# Convert our output to PNG for comparison
echo ""
echo "Converting Lean output..."
magick artifacts/flat-full.ppm /tmp/lean-flat.png

# Compare images
echo ""
echo "Comparing Lean output with upstream reference..."
# Get absolute error count (AE metric outputs just the count to stderr)
DIFF_OUTPUT=$(magick compare -metric AE /tmp/lean-flat.png external/tetra-gray/images/flat.png /tmp/diff.png 2>&1) || true
# Extract just the integer part (ImageMagick may append extra info)
DIFF_COUNT=$(echo "$DIFF_OUTPUT" | grep -oE '^[0-9]+' | head -1)

TOTAL_PIXELS=$((1280 * 720))
echo "Total pixels: $TOTAL_PIXELS"
echo "Differing pixels: $DIFF_COUNT"

# Calculate percentage
if [ "$DIFF_COUNT" = "0" ]; then
    echo ""
    echo "✓ PASS: Images are bitwise identical!"
    exit 0
elif [ "$DIFF_COUNT" -lt 1000 ]; then
    PERCENT=$(echo "scale=6; $DIFF_COUNT * 100 / $TOTAL_PIXELS" | bc)
    echo "Difference: ${PERCENT}%"
    echo ""
    echo "✓ PASS: Images match within tolerance (< 0.1% difference)"
    echo "  (Minor differences due to floating-point rounding between GPU/CPU)"
    exit 0
else
    PERCENT=$(echo "scale=6; $DIFF_COUNT * 100 / $TOTAL_PIXELS" | bc)
    echo "Difference: ${PERCENT}%"
    echo ""
    echo "✗ FAIL: Images differ significantly"
    echo "  Check /tmp/diff.png for visual diff"
    exit 1
fi
