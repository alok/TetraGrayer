// Metal wrapper for TetraGrayer - C API for Lean FFI
#ifndef METAL_WRAPPER_H
#define METAL_WRAPPER_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Render parameters matching Metal shader
typedef struct {
    uint32_t width;
    uint32_t height;
    float spinParam;
    float extractRadius;
    float maxParam;
    float maxStepRatio;
    uint32_t maxSteps;
    float dparam0;
    float camPos[4];    // t, x, y, z
    float hFov;
} MetalRenderParams;

// Initialize Metal device and compile shader
// Returns 0 on success, non-zero on error
int metal_init(void);

// Cleanup Metal resources
void metal_cleanup(void);

// Render image using Metal GPU
// output: pre-allocated buffer of size width * height * 4 bytes (RGBA)
// Returns 0 on success, non-zero on error
int metal_render(const MetalRenderParams* params, uint8_t* output);

// Check if Metal is available on this system
int metal_available(void);

#ifdef __cplusplus
}
#endif

#endif // METAL_WRAPPER_H
