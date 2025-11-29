// Lean FFI wrapper for Metal GPU rendering
// Uses Lean runtime types for proper memory management

#include <lean/lean.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "metal_wrapper.h"

// ============================================================================
// Lean FFI - these functions are called from Lean via @[extern]
// ============================================================================

// Check if Metal is available (pure function)
// @[extern "lean_metal_available"]
// opaque metalAvailable : Unit → Bool
LEAN_EXPORT uint8_t lean_metal_available(lean_obj_arg unit) {
    return metal_available() ? 1 : 0;
}

// Initialize Metal device
// @[extern "lean_metal_init"]
// opaque metalInit : Unit → IO Int
LEAN_EXPORT lean_obj_res lean_metal_init(lean_obj_arg unit, lean_obj_arg world) {
    int result = metal_init();
    return lean_io_result_mk_ok(lean_int_to_int(result));
}

// Cleanup Metal resources
// @[extern "lean_metal_cleanup"]
// opaque metalCleanup : Unit → IO Unit
LEAN_EXPORT lean_obj_res lean_metal_cleanup(lean_obj_arg unit, lean_obj_arg world) {
    metal_cleanup();
    return lean_io_result_mk_ok(lean_box(0));
}

// Render to ByteArray
// @[extern "lean_metal_render"]
// opaque metalRenderRaw :
//   (width : UInt32) → (height : UInt32) →
//   (spinParam : Float) → (extractRadius : Float) →
//   (maxParam : Float) → (maxStepRatio : Float) →
//   (maxSteps : UInt32) → (dparam0 : Float) →
//   (camT : Float) → (camX : Float) → (camY : Float) → (camZ : Float) →
//   (hFov : Float) →
//   IO (Int × ByteArray)
LEAN_EXPORT lean_obj_res lean_metal_render(
    uint32_t width, uint32_t height,
    double spinParam, double extractRadius,
    double maxParam, double maxStepRatio,
    uint32_t maxSteps, double dparam0,
    double camT, double camX, double camY, double camZ,
    double hFov,
    lean_obj_arg world
) {
    // Calculate buffer size
    size_t bufferSize = (size_t)width * (size_t)height * 4;

    // Allocate Lean ByteArray
    lean_object* byteArray = lean_alloc_sarray(1, bufferSize, bufferSize);
    uint8_t* data = lean_sarray_cptr(byteArray);

    // Set up Metal parameters
    MetalRenderParams params;
    params.width = width;
    params.height = height;
    params.spinParam = (float)spinParam;
    params.extractRadius = (float)extractRadius;
    params.maxParam = (float)maxParam;
    params.maxStepRatio = (float)maxStepRatio;
    params.maxSteps = maxSteps;
    params.dparam0 = (float)dparam0;
    params.camPos[0] = (float)camT;
    params.camPos[1] = (float)camX;
    params.camPos[2] = (float)camY;
    params.camPos[3] = (float)camZ;
    params.hFov = (float)hFov;

    // Call Metal render
    int result = metal_render(&params, data);

    // Create result tuple (Int × ByteArray)
    lean_object* resultInt = lean_int_to_int(result);
    lean_object* pair = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(pair, 0, resultInt);
    lean_ctor_set(pair, 1, byteArray);

    return lean_io_result_mk_ok(pair);
}
