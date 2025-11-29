// Metal wrapper implementation for TetraGrayer
// Objective-C code to interface with Metal GPU

#import <Foundation/Foundation.h>
#import <Metal/Metal.h>
#import "metal_wrapper.h"

// Global Metal state
static id<MTLDevice> g_device = nil;
static id<MTLCommandQueue> g_commandQueue = nil;
static id<MTLComputePipelineState> g_pipelineState = nil;
static id<MTLLibrary> g_library = nil;

int metal_available(void) {
    id<MTLDevice> device = MTLCreateSystemDefaultDevice();
    if (device) {
        return 1;
    }
    return 0;
}

int metal_init(void) {
    @autoreleasepool {
        // Get the default Metal device
        g_device = MTLCreateSystemDefaultDevice();
        if (!g_device) {
            NSLog(@"Metal is not supported on this device");
            return -1;
        }

        NSLog(@"Using Metal device: %@", g_device.name);

        // Create command queue
        g_commandQueue = [g_device newCommandQueue];
        if (!g_commandQueue) {
            NSLog(@"Failed to create command queue");
            return -2;
        }

        // Load the shader from the bundle or compile from source
        NSError *error = nil;

        // Try to load pre-compiled metallib first
        NSString *libPath = [[NSBundle mainBundle] pathForResource:@"raytracer" ofType:@"metallib"];
        if (libPath) {
            g_library = [g_device newLibraryWithFile:libPath error:&error];
        }

        // If no metallib, compile from source
        if (!g_library) {
            // Find the shader source file
            NSString *shaderPath = nil;

            // Try relative path from executable
            NSString *execPath = [[NSBundle mainBundle] executablePath];
            NSString *execDir = [execPath stringByDeletingLastPathComponent];

            // Try various paths
            NSArray *searchPaths = @[
                [execDir stringByAppendingPathComponent:@"../../../ffi/metal/raytracer.metal"],
                [execDir stringByAppendingPathComponent:@"../../ffi/metal/raytracer.metal"],
                [execDir stringByAppendingPathComponent:@"ffi/metal/raytracer.metal"],
                @"ffi/metal/raytracer.metal",
                @"./ffi/metal/raytracer.metal"
            ];

            for (NSString *path in searchPaths) {
                if ([[NSFileManager defaultManager] fileExistsAtPath:path]) {
                    shaderPath = path;
                    break;
                }
            }

            if (!shaderPath) {
                NSLog(@"Could not find raytracer.metal shader file");
                return -3;
            }

            NSString *shaderSource = [NSString stringWithContentsOfFile:shaderPath
                                                               encoding:NSUTF8StringEncoding
                                                                  error:&error];
            if (error) {
                NSLog(@"Failed to read shader source: %@", error);
                return -4;
            }

            MTLCompileOptions *options = [[MTLCompileOptions alloc] init];
            options.fastMathEnabled = YES;

            g_library = [g_device newLibraryWithSource:shaderSource options:options error:&error];
            if (error) {
                NSLog(@"Failed to compile shader: %@", error);
                return -5;
            }
        }

        // Get the kernel function
        id<MTLFunction> kernelFunction = [g_library newFunctionWithName:@"raytrace_kernel"];
        if (!kernelFunction) {
            NSLog(@"Failed to find raytrace_kernel function");
            return -6;
        }

        // Create compute pipeline
        g_pipelineState = [g_device newComputePipelineStateWithFunction:kernelFunction error:&error];
        if (error) {
            NSLog(@"Failed to create pipeline state: %@", error);
            return -7;
        }

        NSLog(@"Metal initialized successfully");
        return 0;
    }
}

void metal_cleanup(void) {
    @autoreleasepool {
        g_pipelineState = nil;
        g_library = nil;
        g_commandQueue = nil;
        g_device = nil;
    }
}

// Struct matching Metal shader's RenderParams
typedef struct {
    uint32_t width;
    uint32_t height;
    float spinParam;
    float extractRadius;
    float maxParam;
    float maxStepRatio;
    uint32_t maxSteps;
    float dparam0;
    float camPos[4];
    float hFov;
} GPURenderParams;

int metal_render(const MetalRenderParams* params, uint8_t* output) {
    @autoreleasepool {
        if (!g_device || !g_commandQueue || !g_pipelineState) {
            NSLog(@"Metal not initialized");
            return -1;
        }

        uint32_t width = params->width;
        uint32_t height = params->height;
        size_t outputSize = width * height * 4;

        // Create output buffer
        id<MTLBuffer> outputBuffer = [g_device newBufferWithLength:outputSize
                                                           options:MTLResourceStorageModeShared];
        if (!outputBuffer) {
            NSLog(@"Failed to create output buffer");
            return -2;
        }

        // Create params buffer
        GPURenderParams gpuParams;
        gpuParams.width = width;
        gpuParams.height = height;
        gpuParams.spinParam = params->spinParam;
        gpuParams.extractRadius = params->extractRadius;
        gpuParams.maxParam = params->maxParam;
        gpuParams.maxStepRatio = params->maxStepRatio;
        gpuParams.maxSteps = params->maxSteps;
        gpuParams.dparam0 = params->dparam0;
        gpuParams.camPos[0] = params->camPos[0];
        gpuParams.camPos[1] = params->camPos[1];
        gpuParams.camPos[2] = params->camPos[2];
        gpuParams.camPos[3] = params->camPos[3];
        gpuParams.hFov = params->hFov;

        id<MTLBuffer> paramsBuffer = [g_device newBufferWithBytes:&gpuParams
                                                           length:sizeof(GPURenderParams)
                                                          options:MTLResourceStorageModeShared];

        // Create command buffer
        id<MTLCommandBuffer> commandBuffer = [g_commandQueue commandBuffer];
        id<MTLComputeCommandEncoder> encoder = [commandBuffer computeCommandEncoder];

        [encoder setComputePipelineState:g_pipelineState];
        [encoder setBuffer:outputBuffer offset:0 atIndex:0];
        [encoder setBuffer:paramsBuffer offset:0 atIndex:1];

        // Calculate thread group sizes
        NSUInteger threadGroupSize = g_pipelineState.maxTotalThreadsPerThreadgroup;
        if (threadGroupSize > 256) threadGroupSize = 256;

        // Use 16x16 thread groups for 2D dispatch
        MTLSize threadsPerGroup = MTLSizeMake(16, 16, 1);
        MTLSize numGroups = MTLSizeMake(
            (width + 15) / 16,
            (height + 15) / 16,
            1
        );

        [encoder dispatchThreadgroups:numGroups threadsPerThreadgroup:threadsPerGroup];
        [encoder endEncoding];

        // Execute and wait
        [commandBuffer commit];
        [commandBuffer waitUntilCompleted];

        // Check for errors
        if (commandBuffer.error) {
            NSLog(@"Command buffer error: %@", commandBuffer.error);
            return -3;
        }

        // Copy results to output
        memcpy(output, outputBuffer.contents, outputSize);

        return 0;
    }
}
