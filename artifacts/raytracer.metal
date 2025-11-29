
// ============================================================================
// TetraGrayer Metal Raytracer Kernel
// Auto-generated from Lean definitions
// ============================================================================

#include <metal_stdlib>
using namespace metal;

// ============================================================================
// Type Definitions
// ============================================================================

struct Vector4 {
    float v0;  // t (time)
    float v1;  // x
    float v2;  // y
    float v3;  // z
};

struct Bivector {
    float b01;  // t∧x
    float b02;  // t∧y
    float b12;  // x∧y
    float b03;  // t∧z
    float b13;  // x∧z
    float b23;  // y∧z
};

struct Particle {
    Vector4 position;
    Vector4 momentum;
};

struct ODEData {
    Particle value;
    float param;    // affine parameter λ
    float dparam;   // step size
};

struct CameraParams {
    Vector4 position;
    float hFov;
    uint width;
    uint height;
    float dparam;
};

struct DoranParams {
    float spinParam;      // a
    float extractRadius;
    float maxParam;
    float maxStepRatio;
    uint maxSteps;
};

struct RGB {
    uchar r, g, b;
};

// ============================================================================
// Vector Operations
// ============================================================================

inline Vector4 vec_add(Vector4 a, Vector4 b) {
    return (Vector4){a.v0 + b.v0, a.v1 + b.v1, a.v2 + b.v2, a.v3 + b.v3};
}

inline Vector4 vec_sub(Vector4 a, Vector4 b) {
    return (Vector4){a.v0 - b.v0, a.v1 - b.v1, a.v2 - b.v2, a.v3 - b.v3};
}

inline Vector4 vec_neg(Vector4 v) {
    return (Vector4){-v.v0, -v.v1, -v.v2, -v.v3};
}

inline Vector4 vec_smul(float s, Vector4 v) {
    return (Vector4){s * v.v0, s * v.v1, s * v.v2, s * v.v3};
}

inline float vec_dot(Vector4 v, Vector4 w) {
    // Minkowski metric: -+ + +
    return -v.v0 * w.v0 + v.v1 * w.v1 + v.v2 * w.v2 + v.v3 * w.v3;
}

// ============================================================================
// Bivector Operations
// ============================================================================

inline Bivector biv_add(Bivector a, Bivector b) {
    return (Bivector){
        a.b01 + b.b01, a.b02 + b.b02, a.b12 + b.b12,
        a.b03 + b.b03, a.b13 + b.b13, a.b23 + b.b23
    };
}

inline Bivector biv_smul(float s, Bivector b) {
    return (Bivector){
        s * b.b01, s * b.b02, s * b.b12,
        s * b.b03, s * b.b13, s * b.b23
    };
}

inline Bivector vec_wedge(Vector4 v, Vector4 w) {
    return (Bivector){
        v.v0 * w.v1 - v.v1 * w.v0,  // b01
        v.v0 * w.v2 - v.v2 * w.v0,  // b02
        v.v1 * w.v2 - v.v2 * w.v1,  // b12
        v.v0 * w.v3 - v.v3 * w.v0,  // b03
        v.v1 * w.v3 - v.v3 * w.v1,  // b13
        v.v2 * w.v3 - v.v3 * w.v2   // b23
    };
}

inline Vector4 biv_dot_vec(Bivector b, Vector4 v) {
    return (Vector4){
        b.b01 * v.v1 + b.b02 * v.v2 + b.b03 * v.v3,
        b.b01 * v.v0 + b.b12 * v.v2 + b.b13 * v.v3,
        b.b02 * v.v0 - b.b12 * v.v1 + b.b23 * v.v3,
        b.b03 * v.v0 - b.b13 * v.v1 - b.b23 * v.v2
    };
}

inline Bivector biv_dual(Bivector b) {
    // Hodge dual in 4D Minkowski: *B
    return (Bivector){
        -b.b23, b.b13, b.b03,
        -b.b12, b.b02, -b.b01
    };
}

// ============================================================================
// Particle Operations
// ============================================================================

inline Particle particle_add(Particle a, Particle b) {
    return (Particle){vec_add(a.position, b.position), vec_add(a.momentum, b.momentum)};
}

inline Particle particle_smul(float s, Particle p) {
    return (Particle){vec_smul(s, p.position), vec_smul(s, p.momentum)};
}

// ============================================================================
// Coordinate Systems
// ============================================================================

inline float copysign_f(float x, float y) {
    return y >= 0.0f ? fabs(x) : -fabs(x);
}

inline Vector4 spherical_from_cartesian(Vector4 pos) {
    float t = pos.v0;
    float x = pos.v1;
    float y = pos.v2;
    float z = pos.v3;
    float r = sqrt(x*x + y*y + z*z);
    float theta = r > 0.0f ? acos(z / r) : 0.0f;
    float phi = atan2(y, x);
    return (Vector4){t, r, theta, phi};
}

inline Vector4 spheroidal_from_cartesian(float a, Vector4 pos) {
    float t = pos.v0;
    float x = pos.v1;
    float y = pos.v2;
    float z = pos.v3;
    float phi = atan2(y, x);
    float rho = sqrt(x*x + y*y);
    float d1 = sqrt((rho + a) * (rho + a) + z*z);
    float d2 = sqrt((rho - a) * (rho - a) + z*z);
    float coshMu = (d1 + d2) / (2.0f * a);
    float mu = acosh(coshMu);
    float cos2Nu = 1.0f - (d1 - d2) * (d1 - d2) / (4.0f * a * a);
    float cosNuSign = copysign_f(sqrt(max(cos2Nu, 0.0f)), z);
    float nu = acos(cosNuSign);
    return (Vector4){t, mu, nu, phi};
}

// ============================================================================
// Spheroidal Basis Vectors
// ============================================================================

inline Vector4 spheroidal_basis_emu(float sinhMu, float coshMu, float sinNu, float cosNu, float sinPhi, float cosPhi) {
    float denom = sqrt(sinhMu * sinhMu + cosNu * cosNu);
    float invD = 1.0f / denom;
    return (Vector4){0.0f, sinhMu * sinNu * cosPhi * invD, sinhMu * sinNu * sinPhi * invD, coshMu * cosNu * invD};
}

inline Vector4 spheroidal_basis_enu(float sinhMu, float coshMu, float sinNu, float cosNu, float sinPhi, float cosPhi) {
    float denom = sqrt(sinhMu * sinhMu + cosNu * cosNu);
    float invD = 1.0f / denom;
    return (Vector4){0.0f, coshMu * cosNu * cosPhi * invD, coshMu * cosNu * sinPhi * invD, -sinhMu * sinNu * invD};
}

inline Vector4 spheroidal_basis_phi(float sinPhi, float cosPhi) {
    return (Vector4){0.0f, -sinPhi, cosPhi, 0.0f};
}

inline Vector4 spheroidal_basis_t() {
    return (Vector4){1.0f, 0.0f, 0.0f, 0.0f};
}

// ============================================================================
// Doran Metric Functions
// ============================================================================

inline float doran_beta(float coshMu, float sinNu) {
    return atanh(sinNu / coshMu);
}

inline Vector4 doran_vector_v(float beta, Vector4 that, Vector4 phihat) {
    return vec_add(vec_smul(cosh(beta), that), vec_smul(sinh(beta), phihat));
}

inline Vector4 doran_position_gauge(float sinhMu, Vector4 emuhat, float a, Vector4 doranV, Vector4 vecArg) {
    float rootFactor = sqrt(2.0f * sinhMu / a / (1.0f + sinhMu * sinhMu));
    float dotProduct = vec_dot(vecArg, doranV);
    return vec_add(vecArg, vec_smul(rootFactor * dotProduct, emuhat));
}

inline Bivector doran_rotation_gauge(float sinhMu, float cosNu,
    Vector4 muhat, Vector4 nuhat, Vector4 phihat, Vector4 that,
    float beta, Vector4 doranV, float a, Vector4 vecArg) {

    float alpha = -sqrt(2.0f * sinhMu / (a * (sinhMu * sinhMu + cosNu * cosNu)));

    float argDotMu = vec_dot(vecArg, muhat);
    float argDotNu = vec_dot(vecArg, nuhat);
    float argDotPhi = vec_dot(vecArg, phihat);

    Bivector muterm = vec_wedge(muhat, doranV);
    Bivector nuterm = vec_wedge(nuhat, doranV);
    Bivector phiterm = vec_wedge(phihat, that);

    float commonScalar = a * sinhMu;
    float commonPseudo = a * cosNu;
    float commonDenom = commonScalar * commonScalar + commonPseudo * commonPseudo;

    float muScalar = (commonScalar * commonScalar - commonPseudo * commonPseudo) /
                     (commonDenom * commonDenom);
    float muPseudo = (2.0f * commonScalar * commonPseudo) /
                     (commonDenom * commonDenom);

    // mu contribution
    Bivector muContrib = biv_smul(
        (1.0f / alpha) * argDotMu / (commonDenom * commonDenom),
        biv_add(biv_smul(muScalar, muterm), biv_smul(muPseudo, biv_dual(muterm)))
    );

    // nu contribution
    Bivector nuContrib = biv_smul(
        (-alpha) * argDotNu / commonDenom,
        biv_add(biv_smul(commonScalar, nuterm), biv_smul(commonPseudo, biv_dual(nuterm)))
    );

    // phi contribution
    Bivector phiContrib = biv_smul(
        (-alpha / cosh(beta)) * argDotPhi / commonDenom,
        biv_add(biv_smul(commonScalar, phiterm), biv_smul(commonPseudo, biv_dual(phiterm)))
    );

    return biv_add(biv_add(muContrib, nuContrib), phiContrib);
}

inline Particle doran_rhs(Particle data, float a) {
    Vector4 coords = spheroidal_from_cartesian(a, data.position);
    float mu = coords.v1;
    float nu = coords.v2;
    float phi = coords.v3;

    float sinhMu = sinh(mu);
    float coshMu = cosh(mu);
    float sinNu = sin(nu);
    float cosNu = cos(nu);
    float sinPhi = sin(phi);
    float cosPhi = cos(phi);

    Vector4 emu = spheroidal_basis_emu(sinhMu, coshMu, sinNu, cosNu, sinPhi, cosPhi);
    Vector4 enu = spheroidal_basis_enu(sinhMu, coshMu, sinNu, cosNu, sinPhi, cosPhi);
    Vector4 ephi = spheroidal_basis_phi(sinPhi, cosPhi);
    Vector4 et = spheroidal_basis_t();

    float beta = doran_beta(coshMu, sinNu);
    Vector4 V = doran_vector_v(beta, et, ephi);

    Vector4 xprime = doran_position_gauge(sinhMu, emu, a, V, data.momentum);
    Bivector omega = doran_rotation_gauge(sinhMu, cosNu, emu, enu, ephi, et, beta, V, a, data.momentum);
    Vector4 kprime = vec_neg(biv_dot_vec(omega, data.momentum));

    return (Particle){xprime, kprime};
}

// ============================================================================
// RK4 Integration
// ============================================================================

inline Particle rk4_step(Particle y, float dt, float t, float a) {
    Particle k1 = doran_rhs(y, a);
    Particle k2 = doran_rhs(particle_add(y, particle_smul(dt / 2.0f, k1)), a);
    Particle k3 = doran_rhs(particle_add(y, particle_smul(dt / 2.0f, k2)), a);
    Particle k4 = doran_rhs(particle_add(y, particle_smul(dt, k3)), a);

    // y + (dt/6) * (k1 + 2*k2 + 2*k3 + k4)
    Particle sum = particle_add(k1, particle_add(
        particle_smul(2.0f, k2),
        particle_add(particle_smul(2.0f, k3), k4)
    ));
    return particle_add(y, particle_smul(dt / 6.0f, sum));
}

inline ODEData rk4_step_ode(ODEData ode, float a) {
    Particle newValue = rk4_step(ode.value, ode.dparam, ode.param, a);
    return (ODEData){newValue, ode.param + ode.dparam, ode.dparam};
}

inline ODEData adjust_stepsize(float dparam0, ODEData data) {
    float pt = fabs(data.value.momentum.v0);
    float ratio = pt < 1e-10f ? 1e-10f : pt;
    return (ODEData){data.value, data.param, dparam0 / ratio};
}

inline bool escaped(float radius, ODEData data) {
    Vector4 spherical = spherical_from_cartesian(data.value.position);
    return spherical.v1 >= radius;
}

inline bool param_exceeded(float maxParam, ODEData data) {
    return data.param >= maxParam;
}

inline bool step_ratio_exceeded(float maxRatio, float dparam0, ODEData data) {
    return dparam0 / data.dparam >= maxRatio;
}

// ============================================================================
// Colormap
// ============================================================================

inline bool test_stripe(float angle, float stripeInterval, float stripeHalfWidthRatio) {
    float stripeNumber = angle / stripeInterval;
    float stripeRemainder = stripeNumber - round(stripeNumber);
    return fabs(stripeRemainder) <= stripeHalfWidthRatio;
}

inline RGB spherical_colormap(float escapeRadius, ODEData data) {
    Vector4 pos = data.value.position;
    Vector4 spherical = spherical_from_cartesian(pos);

    if (spherical.v1 >= escapeRadius) {
        float theta = spherical.v2;
        float phi = spherical.v3;

        float stripeHalfWidth = 0.05f;
        float stripeInterval = M_PI_F / 18.0f;  // 10 degrees

        if (test_stripe(theta, stripeInterval, stripeHalfWidth) ||
            test_stripe(phi, stripeInterval, stripeHalfWidth)) {
            return (RGB){0, 0, 0};  // black grid
        } else {
            float y = pos.v2;
            float z = pos.v3;
            if (y > 0.0f && z > 0.0f) return (RGB){200, 100, 100};       // red quadrant
            else if (y < 0.0f && z > 0.0f) return (RGB){100, 200, 100};  // green quadrant
            else if (y > 0.0f && z < 0.0f) return (RGB){100, 100, 200};  // blue quadrant
            else return (RGB){200, 200, 100};                            // yellow quadrant
        }
    } else {
        return (RGB){100, 200, 200};  // cyan = didn't escape
    }
}

// ============================================================================
// Camera / Ray Generation
// ============================================================================

inline ODEData pixel_initial_data(CameraParams cam, uint pixelIdx) {
    uint w = cam.width;
    uint h = cam.height;

    float da = cam.hFov / (float)w;

    uint heightIdx = pixelIdx / w;
    uint widthIdx = pixelIdx - heightIdx * w;

    float widthAngle = ((float)(widthIdx) - (float)(w / 2) + 0.5f) * da;
    float heightAngle = ((float)(heightIdx) - (float)(h / 2) + 0.5f) * da;

    // Simple rotation (approximation without full rotor algebra for brevity)
    float cosW = cos(widthAngle);
    float sinW = sin(widthAngle);
    float cosH = cos(heightAngle);
    float sinH = sin(heightAngle);

    // Central momentum: out - time (null vector pointing inward)
    // out direction is (-1, 0, 0), time direction is (1, 0, 0, 0) in 4-vector form
    // After rotation, the spatial part becomes (-cosW*cosH, sinW*cosH, sinH)
    Vector4 rotatedOut = (Vector4){
        0.0f,
        -cosW * cosH,
        sinW * cosH,
        sinH
    };

    // momentum = rotatedOut - timeDir = (0, spatial) - (1, 0, 0, 0) = (-1, spatial)
    Vector4 momentum = (Vector4){
        -1.0f,  // time component
        rotatedOut.v1,
        rotatedOut.v2,
        rotatedOut.v3
    };

    Particle particle = (Particle){cam.position, momentum};
    return (ODEData){particle, 0.0f, cam.dparam};
}

// ============================================================================
// Main Raytracing Kernel
// ============================================================================

kernel void raytrace_doran(
    device RGB* output [[buffer(0)]],
    constant CameraParams& cam [[buffer(1)]],
    constant DoranParams& params [[buffer(2)]],
    uint2 gid [[thread_position_in_grid]]
) {
    uint x = gid.x;
    uint y = gid.y;
    if (x >= cam.width || y >= cam.height) return;

    uint pixelIdx = y * cam.width + x;
    ODEData ode = pixel_initial_data(cam, pixelIdx);

    float dparam0 = cam.dparam;

    // Integration loop
    for (uint step = 0; step < params.maxSteps; step++) {
        // Check stop conditions
        if (escaped(params.extractRadius, ode) ||
            param_exceeded(params.maxParam, ode) ||
            step_ratio_exceeded(params.maxStepRatio, dparam0, ode)) {
            break;
        }

        // RK4 step
        ode = rk4_step_ode(ode, params.spinParam);

        // Adaptive stepsize
        ode = adjust_stepsize(dparam0, ode);
    }

    // Colormap
    output[pixelIdx] = spherical_colormap(params.extractRadius, ode);
}

// Flat spacetime kernel for comparison
kernel void raytrace_flat(
    device RGB* output [[buffer(0)]],
    constant CameraParams& cam [[buffer(1)]],
    constant float& extractRadius [[buffer(2)]],
    constant float& maxParam [[buffer(3)]],
    constant uint& maxSteps [[buffer(4)]],
    uint2 gid [[thread_position_in_grid]]
) {
    uint x = gid.x;
    uint y = gid.y;
    if (x >= cam.width || y >= cam.height) return;

    uint pixelIdx = y * cam.width + x;
    ODEData ode = pixel_initial_data(cam, pixelIdx);

    // In flat spacetime: dx = p, dp = 0
    for (uint step = 0; step < maxSteps; step++) {
        if (escaped(extractRadius, ode) || param_exceeded(maxParam, ode)) break;

        // Simple Euler step for flat space (momentum constant)
        ode.value.position = vec_add(ode.value.position, vec_smul(ode.dparam, ode.value.momentum));
        ode.param += ode.dparam;
    }

    output[pixelIdx] = spherical_colormap(extractRadius, ode);
}
