// Lean FFI wrapper for Metal GPU rendering
// Pure C implementation - no Lean runtime dependencies

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "metal_wrapper.h"

// ============================================================================
// Simple C API (called via Lean @[extern])
// ============================================================================

// Check if Metal is available (returns 1 if yes, 0 if no)
int tetra_metal_available(void) {
    return metal_available();
}

// Initialize Metal (returns 0 on success)
int tetra_metal_init(void) {
    return metal_init();
}

// Cleanup Metal
void tetra_metal_cleanup(void) {
    metal_cleanup();
}

// Render to pre-allocated buffer
// Returns 0 on success, error code otherwise
int tetra_metal_render(
    uint32_t width, uint32_t height,
    float spinParam, float extractRadius, float maxParam, float maxStepRatio,
    uint32_t maxSteps, float dparam0,
    float camT, float camX, float camY, float camZ,
    float hFov,
    uint8_t* output  // Must be width * height * 4 bytes
) {
    MetalRenderParams params;
    params.width = width;
    params.height = height;
    params.spinParam = spinParam;
    params.extractRadius = extractRadius;
    params.maxParam = maxParam;
    params.maxStepRatio = maxStepRatio;
    params.maxSteps = maxSteps;
    params.dparam0 = dparam0;
    params.camPos[0] = camT;
    params.camPos[1] = camX;
    params.camPos[2] = camY;
    params.camPos[3] = camZ;
    params.hFov = hFov;

    return metal_render(&params, output);
}
