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
    float cse0 = (a.v0 + b.v0);
    float cse1 = (a.v1 + b.v1);
    float cse2 = (a.v2 + b.v2);
    float cse3 = (a.v3 + b.v3);
    Vector4 cse4 = (Vector4){cse0, cse1, cse2, cse3};
    return cse4;
}

inline Vector4 TetraGrayer_Core_CliffordVector_smul(float s, Vector4 v) {
    float cse0 = (s * v.v0);
    float cse1 = (s * v.v1);
    float cse2 = (s * v.v2);
    float cse3 = (s * v.v3);
    Vector4 cse4 = (Vector4){cse0, cse1, cse2, cse3};
    return cse4;
}

inline Bivector TetraGrayer_Core_Bivector_add(Bivector a, Bivector b) {
    float cse0 = (a.b01 + b.b01);
    float cse1 = (a.b02 + b.b02);
    float cse2 = (a.b12 + b.b12);
    float cse3 = (a.b03 + b.b03);
    float cse4 = (a.b13 + b.b13);
    float cse5 = (a.b23 + b.b23);
    Bivector cse6 = (Bivector){cse0, cse1, cse2, cse3, cse4, cse5};
    return cse6;
}

inline Bivector TetraGrayer_Core_Bivector_smul(float s, Bivector b) {
    float cse0 = (s * b.b01);
    float cse1 = (s * b.b02);
    float cse2 = (s * b.b12);
    float cse3 = (s * b.b03);
    float cse4 = (s * b.b13);
    float cse5 = (s * b.b23);
    Bivector cse6 = (Bivector){cse0, cse1, cse2, cse3, cse4, cse5};
    return cse6;
}

inline Bivector TetraGrayer_Core_vectorWedge(Vector4 v, Vector4 w) {
    float cse0 = (v.v0 * w.v1);
    float cse1 = (v.v1 * w.v0);
    float cse2 = (cse0 - cse1);
    float cse3 = (v.v0 * w.v2);
    float cse4 = (v.v2 * w.v0);
    float cse5 = (cse3 - cse4);
    float cse6 = (v.v1 * w.v2);
    float cse7 = (v.v2 * w.v1);
    float cse8 = (cse6 - cse7);
    float cse9 = (v.v0 * w.v3);
    float cse10 = (v.v3 * w.v0);
    float cse11 = (cse9 - cse10);
    float cse12 = (v.v1 * w.v3);
    float cse13 = (v.v3 * w.v1);
    float cse14 = (cse12 - cse13);
    float cse15 = (v.v2 * w.v3);
    float cse16 = (v.v3 * w.v2);
    float cse17 = (cse15 - cse16);
    Bivector cse18 = (Bivector){cse2, cse5, cse8, cse11, cse14, cse17};
    return cse18;
}

inline Vector4 TetraGrayer_Core_bivectorDotVector(Bivector b, Vector4 v) {
    float cse0 = (b.b01 * v.v1);
    float cse1 = (b.b02 * v.v2);
    float cse2 = (cse0 + cse1);
    float cse3 = (b.b03 * v.v3);
    float cse4 = (cse2 + cse3);
    float cse5 = (b.b01 * v.v0);
    float cse6 = (b.b12 * v.v2);
    float cse7 = (cse5 + cse6);
    float cse8 = (b.b13 * v.v3);
    float cse9 = (cse7 + cse8);
    float cse10 = (b.b02 * v.v0);
    float cse11 = (b.b12 * v.v1);
    float cse12 = (cse10 - cse11);
    float cse13 = (b.b23 * v.v3);
    float cse14 = (cse12 + cse13);
    float cse15 = (b.b03 * v.v0);
    float cse16 = (b.b13 * v.v1);
    float cse17 = (cse15 - cse16);
    float cse18 = (b.b23 * v.v2);
    float cse19 = (cse17 - cse18);
    Vector4 cse20 = (Vector4){cse4, cse9, cse14, cse19};
    return cse20;
}

inline float TetraGrayer_Codegen_Test_minkowskiDot(Vector4 v, Vector4 w) {
    float cse0 = (-v.v0);
    float cse1 = (cse0 * w.v0);
    float cse2 = (v.v1 * w.v1);
    float cse3 = (cse1 + cse2);
    float cse4 = (v.v2 * w.v2);
    float cse5 = (cse3 + cse4);
    float cse6 = (v.v3 * w.v3);
    float cse7 = (cse5 + cse6);
    return cse7;
}

inline Particle TetraGrayer_Codegen_Test_updatePosition(Particle p, float dt) {
    float cse0 = (dt * p.momentum.v0);
    float cse1 = (p.position.v0 + cse0);
    float cse2 = (dt * p.momentum.v1);
    float cse3 = (p.position.v1 + cse2);
    float cse4 = (dt * p.momentum.v2);
    float cse5 = (p.position.v2 + cse4);
    float cse6 = (dt * p.momentum.v3);
    float cse7 = (p.position.v3 + cse6);
    Vector4 cse8 = (Vector4){cse1, cse3, cse5, cse7};
    Particle cse9 = (Particle){cse8, p.momentum};
    return cse9;
}
