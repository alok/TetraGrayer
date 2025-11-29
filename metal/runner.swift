#!/usr/bin/env swift
// Metal Raytracer Runner
// Executes the generated Metal shader and outputs a PPM image

import Metal
import Foundation

// Structs must match the Metal shader layout exactly
struct Vector4 {
    var v0: Float  // t
    var v1: Float  // x
    var v2: Float  // y
    var v3: Float  // z
}

struct CameraParams {
    var position: Vector4
    var hFov: Float
    var width: UInt32
    var height: UInt32
    var dparam: Float
}

struct DoranParams {
    var spinParam: Float
    var extractRadius: Float
    var maxParam: Float
    var maxStepRatio: Float
    var maxSteps: UInt32
}

struct RGB {
    var r: UInt8
    var g: UInt8
    var b: UInt8
}

// Write PPM file
func writePPM(path: String, width: Int, height: Int, pixels: [RGB]) {
    var data = "P6\n\(width) \(height)\n255\n"
    let headerData = data.data(using: .utf8)!

    var pixelData = Data()
    for pixel in pixels {
        pixelData.append(pixel.r)
        pixelData.append(pixel.g)
        pixelData.append(pixel.b)
    }

    let fullData = headerData + pixelData
    try! fullData.write(to: URL(fileURLWithPath: path))
    print("Wrote \(path)")
}

// Main
func main() {
    // Get Metal device
    guard let device = MTLCreateSystemDefaultDevice() else {
        fatalError("Metal not supported on this device")
    }
    print("Using Metal device: \(device.name)")

    // Load shader from file
    let shaderPath = "../artifacts/raytracer.metal"
    let shaderSource: String
    do {
        shaderSource = try String(contentsOfFile: shaderPath, encoding: .utf8)
    } catch {
        fatalError("Failed to load shader: \(error)")
    }

    // Compile shader
    let library: MTLLibrary
    do {
        library = try device.makeLibrary(source: shaderSource, options: nil)
    } catch {
        fatalError("Failed to compile shader: \(error)")
    }

    // Get kernel function
    guard let kernelFunction = library.makeFunction(name: "raytrace_doran") else {
        fatalError("Failed to find raytrace_doran kernel")
    }

    // Create compute pipeline
    let pipeline: MTLComputePipelineState
    do {
        pipeline = try device.makeComputePipelineState(function: kernelFunction)
    } catch {
        fatalError("Failed to create pipeline: \(error)")
    }

    // Setup parameters
    let width: UInt32 = 640
    let height: UInt32 = 360
    let totalPixels = Int(width * height)

    var camera = CameraParams(
        position: Vector4(v0: 0.0, v1: 25.0, v2: 0.0, v3: 5.0),
        hFov: Float.pi / 2.0,
        width: width,
        height: height,
        dparam: 0.05
    )

    var params = DoranParams(
        spinParam: 0.7,
        extractRadius: 50.0,
        maxParam: 500.0,
        maxStepRatio: 40.0,
        maxSteps: 80000
    )

    // Create buffers
    let outputBuffer = device.makeBuffer(length: totalPixels * MemoryLayout<RGB>.size, options: .storageModeShared)!
    let cameraBuffer = device.makeBuffer(bytes: &camera, length: MemoryLayout<CameraParams>.size, options: .storageModeShared)!
    let paramsBuffer = device.makeBuffer(bytes: &params, length: MemoryLayout<DoranParams>.size, options: .storageModeShared)!

    // Create command queue
    guard let commandQueue = device.makeCommandQueue() else {
        fatalError("Failed to create command queue")
    }

    // Create command buffer
    guard let commandBuffer = commandQueue.makeCommandBuffer() else {
        fatalError("Failed to create command buffer")
    }

    // Create compute encoder
    guard let encoder = commandBuffer.makeComputeCommandEncoder() else {
        fatalError("Failed to create compute encoder")
    }

    encoder.setComputePipelineState(pipeline)
    encoder.setBuffer(outputBuffer, offset: 0, index: 0)
    encoder.setBuffer(cameraBuffer, offset: 0, index: 1)
    encoder.setBuffer(paramsBuffer, offset: 0, index: 2)

    // Dispatch threads
    let threadGroupSize = MTLSize(width: 16, height: 16, depth: 1)
    let gridSize = MTLSize(
        width: Int(width),
        height: Int(height),
        depth: 1
    )

    print("Dispatching \(width)x\(height) = \(totalPixels) pixels")
    let startTime = Date()

    encoder.dispatchThreads(gridSize, threadsPerThreadgroup: threadGroupSize)
    encoder.endEncoding()

    // Execute and wait
    commandBuffer.commit()
    commandBuffer.waitUntilCompleted()

    let elapsed = Date().timeIntervalSince(startTime)
    print("GPU execution time: \(String(format: "%.3f", elapsed))s")

    // Read results
    let resultPointer = outputBuffer.contents().bindMemory(to: RGB.self, capacity: totalPixels)
    let pixels = Array(UnsafeBufferPointer(start: resultPointer, count: totalPixels))

    // Write output
    writePPM(path: "../artifacts/gpu-doran.ppm", width: Int(width), height: Int(height), pixels: pixels)

    print("Done!")
}

main()
