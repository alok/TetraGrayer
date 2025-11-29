// Auto-generated from Lean definitions
// Do not edit manually!

#include <metal_stdlib>
using namespace metal;

struct Vector4 {
    float v0;
    float v1;
    float v2;
    float v3;
};

struct Bivector {
    float b01;
    float b02;
    float b12;
    float b03;
    float b13;
    float b23;
};

struct Particle {
    Vector4 position;
    Vector4 momentum;
};

inline Vector4 TetraGrayer_Codegen_Test_vecAddComponents(Vector4 v, Vector4 w) {
    return (Vector4){(v.v0 + w.v0), (v.v1 + w.v1), (v.v2 + w.v2), (v.v3 + w.v3)};
}

inline float TetraGrayer_Codegen_Test_minkowskiDot(Vector4 v, Vector4 w) {
    return (((((-v.v0) * w.v0) + (v.v1 * w.v1)) + (v.v2 * w.v2)) + (v.v3 * w.v3));
}

inline Vector4 TetraGrayer_Codegen_Test_vecScale(float s, Vector4 v) {
    return (Vector4){(s * v.v0), (s * v.v1), (s * v.v2), (s * v.v3)};
}

inline Particle TetraGrayer_Codegen_Test_updatePosition(Particle p, float dt) {
    return (Particle){(Vector4){(p.position.v0 + (dt * p.momentum.v0)), (p.position.v1 + (dt * p.momentum.v1)), (p.position.v2 + (dt * p.momentum.v2)), (p.position.v3 + (dt * p.momentum.v3))}, p.momentum};
}
