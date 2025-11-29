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

inline Vector4 TetraGrayer_Core_CliffordVector_add(Vector4 a, Vector4 b) {
    return (Vector4){(a.v0 + b.v0), (a.v1 + b.v1), (a.v2 + b.v2), (a.v3 + b.v3)};
}

inline Vector4 TetraGrayer_Core_CliffordVector_smul(float s, Vector4 v) {
    return (Vector4){(s * v.v0), (s * v.v1), (s * v.v2), (s * v.v3)};
}

inline Bivector TetraGrayer_Core_Bivector_add(Bivector a, Bivector b) {
    return (Bivector){(a.b01 + b.b01), (a.b02 + b.b02), (a.b12 + b.b12), (a.b03 + b.b03), (a.b13 + b.b13), (a.b23 + b.b23)};
}

inline Bivector TetraGrayer_Core_Bivector_smul(float s, Bivector b) {
    return (Bivector){(s * b.b01), (s * b.b02), (s * b.b12), (s * b.b03), (s * b.b13), (s * b.b23)};
}

inline Bivector TetraGrayer_Core_vectorWedge(Vector4 v, Vector4 w) {
    return (Bivector){((v.v0 * w.v1) - (v.v1 * w.v0)), ((v.v0 * w.v2) - (v.v2 * w.v0)), ((v.v1 * w.v2) - (v.v2 * w.v1)), ((v.v0 * w.v3) - (v.v3 * w.v0)), ((v.v1 * w.v3) - (v.v3 * w.v1)), ((v.v2 * w.v3) - (v.v3 * w.v2))};
}

inline Vector4 TetraGrayer_Core_bivectorDotVector(Bivector b, Vector4 v) {
    return (Vector4){(((b.b01 * v.v1) + (b.b02 * v.v2)) + (b.b03 * v.v3)), (((b.b01 * v.v0) + (b.b12 * v.v2)) + (b.b13 * v.v3)), (((b.b02 * v.v0) - (b.b12 * v.v1)) + (b.b23 * v.v3)), (((b.b03 * v.v0) - (b.b13 * v.v1)) - (b.b23 * v.v2))};
}

inline float TetraGrayer_Codegen_Test_minkowskiDot(Vector4 v, Vector4 w) {
    return (((((-v.v0) * w.v0) + (v.v1 * w.v1)) + (v.v2 * w.v2)) + (v.v3 * w.v3));
}

inline Particle TetraGrayer_Codegen_Test_updatePosition(Particle p, float dt) {
    return (Particle){(Vector4){(p.position.v0 + (dt * p.momentum.v0)), (p.position.v1 + (dt * p.momentum.v1)), (p.position.v2 + (dt * p.momentum.v2)), (p.position.v3 + (dt * p.momentum.v3))}, p.momentum};
}
