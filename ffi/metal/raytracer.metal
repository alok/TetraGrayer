// Metal compute shader for TetraGrayer raytracing
// Implements Doran (Kerr) black hole geodesic integration

#include <metal_stdlib>
using namespace metal;

// ============================================================================
// Clifford algebra types (signature -,+,+,+)
// ============================================================================

struct Vector4 {
    float v0, v1, v2, v3;  // t, x, y, z
};

struct Bivector {
    float b01, b02, b12, b03, b13, b23;
};

struct Particle {
    Vector4 position;
    Vector4 momentum;
};

// ============================================================================
// Vector operations
// ============================================================================

inline Vector4 vec_add(Vector4 a, Vector4 b) {
    return {a.v0 + b.v0, a.v1 + b.v1, a.v2 + b.v2, a.v3 + b.v3};
}

inline Vector4 vec_sub(Vector4 a, Vector4 b) {
    return {a.v0 - b.v0, a.v1 - b.v1, a.v2 - b.v2, a.v3 - b.v3};
}

inline Vector4 vec_smul(float s, Vector4 v) {
    return {s * v.v0, s * v.v1, s * v.v2, s * v.v3};
}

inline Vector4 vec_neg(Vector4 v) {
    return {-v.v0, -v.v1, -v.v2, -v.v3};
}

// Minkowski dot product: -v0*w0 + v1*w1 + v2*w2 + v3*w3
inline float vec_dot(Vector4 v, Vector4 w) {
    return -v.v0 * w.v0 + v.v1 * w.v1 + v.v2 * w.v2 + v.v3 * w.v3;
}

// ============================================================================
// Bivector operations
// ============================================================================

inline Bivector biv_add(Bivector a, Bivector b) {
    return {a.b01 + b.b01, a.b02 + b.b02, a.b12 + b.b12,
            a.b03 + b.b03, a.b13 + b.b13, a.b23 + b.b23};
}

inline Bivector biv_smul(float s, Bivector b) {
    return {s * b.b01, s * b.b02, s * b.b12, s * b.b03, s * b.b13, s * b.b23};
}

// Hodge dual of bivector
inline Bivector biv_dual(Bivector b) {
    return {-b.b23, b.b13, b.b03, -b.b12, -b.b02, b.b01};
}

// Wedge product of two vectors: v ^ w
inline Bivector vec_wedge(Vector4 v, Vector4 w) {
    return {
        v.v0 * w.v1 - v.v1 * w.v0,  // b01
        v.v0 * w.v2 - v.v2 * w.v0,  // b02
        v.v1 * w.v2 - v.v2 * w.v1,  // b12
        v.v0 * w.v3 - v.v3 * w.v0,  // b03
        v.v1 * w.v3 - v.v3 * w.v1,  // b13
        v.v2 * w.v3 - v.v3 * w.v2   // b23
    };
}

// Bivector contracting vector from right: B|v
inline Vector4 biv_dot_vec(Bivector b, Vector4 v) {
    return {
        b.b01 * v.v1 + b.b02 * v.v2 + b.b03 * v.v3,
        b.b01 * v.v0 + b.b12 * v.v2 + b.b13 * v.v3,
        b.b02 * v.v0 - b.b12 * v.v1 + b.b23 * v.v3,
        b.b03 * v.v0 - b.b13 * v.v1 - b.b23 * v.v2
    };
}

// ============================================================================
// Particle operations
// ============================================================================

inline Particle particle_add(Particle a, Particle b) {
    return {vec_add(a.position, b.position), vec_add(a.momentum, b.momentum)};
}

inline Particle particle_smul(float s, Particle p) {
    return {vec_smul(s, p.position), vec_smul(s, p.momentum)};
}

// ============================================================================
// Coordinate systems
// ============================================================================

// Spheroidal coordinates from Cartesian
inline float3 spheroidal_from_cartesian(float a, Vector4 pos) {
    float x = pos.v1, y = pos.v2, z = pos.v3;
    float rho2 = x*x + y*y;
    float rho = sqrt(rho2);
    float r2 = rho2 + z*z;

    // mu = asinh(r/a), nu = asin(z/r), phi = atan2(y, x)
    float r = sqrt(r2);
    float mu = asinh(r / a);
    float nu = (r > 1e-10) ? asin(z / r) : 0.0;
    float phi = atan2(y, x);

    return float3(mu, nu, phi);
}

// Spheroidal basis vectors
inline Vector4 basis_emu(float sinhMu, float coshMu, float sinNu, float cosNu,
                         float sinPhi, float cosPhi) {
    float denom = sqrt(sinhMu * sinhMu + sinNu * sinNu);
    if (denom < 1e-10) denom = 1e-10;
    return {0.0,
            coshMu * cosNu * cosPhi / denom,
            coshMu * cosNu * sinPhi / denom,
            sinhMu * sinNu / denom};
}

inline Vector4 basis_enu(float sinhMu, float coshMu, float sinNu, float cosNu,
                         float sinPhi, float cosPhi) {
    float denom = sqrt(sinhMu * sinhMu + sinNu * sinNu);
    if (denom < 1e-10) denom = 1e-10;
    return {0.0,
            -sinhMu * sinNu * cosPhi / denom,
            -sinhMu * sinNu * sinPhi / denom,
            coshMu * cosNu / denom};
}

inline Vector4 basis_phi(float sinPhi, float cosPhi) {
    return {0.0, -sinPhi, cosPhi, 0.0};
}

inline Vector4 basis_t() {
    return {1.0, 0.0, 0.0, 0.0};
}

// ============================================================================
// Doran metric functions
// ============================================================================

inline float doran_beta(float coshMu, float sinNu) {
    float arg = sinNu / coshMu;
    // atanh(x) = 0.5 * log((1+x)/(1-x))
    return 0.5 * log((1.0 + arg) / (1.0 - arg));
}

inline Vector4 doran_vector_v(float beta, Vector4 that, Vector4 phihat) {
    return vec_add(vec_smul(cosh(beta), that), vec_smul(sinh(beta), phihat));
}

inline Vector4 doran_position_gauge(float sinhMu, Vector4 emuhat, float a,
                                    Vector4 doranV, Vector4 vecArg) {
    float rootFactor = sqrt(2.0 * sinhMu / a / (1.0 + sinhMu * sinhMu));
    float dotProduct = vec_dot(vecArg, doranV);
    return vec_add(vecArg, vec_smul(rootFactor * dotProduct, emuhat));
}

inline Bivector doran_rotation_gauge(float sinhMu, float cosNu,
                                     Vector4 muhat, Vector4 nuhat,
                                     Vector4 phihat, Vector4 that,
                                     float beta, Vector4 doranV, float a,
                                     Vector4 vecArg) {
    float alpha = -sqrt(2.0 * sinhMu / (a * (sinhMu * sinhMu + cosNu * cosNu)));

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
    float muPseudo = (2.0 * commonScalar * commonPseudo) /
                     (commonDenom * commonDenom);

    // mu contribution
    Bivector muContrib = biv_smul(
        (1.0 / alpha) * argDotMu / (commonDenom * commonDenom),
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

// ============================================================================
// Doran RHS function
// ============================================================================

inline Particle doran_rhs(Particle data, float a) {
    float3 coords = spheroidal_from_cartesian(a, data.position);
    float mu = coords.x;
    float nu = coords.y;
    float phi = coords.z;

    float sinhMu = sinh(mu);
    float coshMu = cosh(mu);
    float sinNu = sin(nu);
    float cosNu = cos(nu);
    float sinPhi = sin(phi);
    float cosPhi = cos(phi);

    Vector4 emu = basis_emu(sinhMu, coshMu, sinNu, cosNu, sinPhi, cosPhi);
    Vector4 enu = basis_enu(sinhMu, coshMu, sinNu, cosNu, sinPhi, cosPhi);
    Vector4 ephi = basis_phi(sinPhi, cosPhi);
    Vector4 et = basis_t();

    float beta = doran_beta(coshMu, sinNu);
    Vector4 V = doran_vector_v(beta, et, ephi);

    // x' = h(k)
    Vector4 xprime = doran_position_gauge(sinhMu, emu, a, V, data.momentum);

    // k' = -omega(k)|k
    Bivector omega = doran_rotation_gauge(sinhMu, cosNu, emu, enu, ephi, et,
                                          beta, V, a, data.momentum);
    Vector4 kprime = vec_neg(biv_dot_vec(omega, data.momentum));

    return {xprime, kprime};
}

// ============================================================================
// RK4 integrator
// ============================================================================

inline Particle rk4_step(Particle y, float dt, float a) {
    Particle k1 = doran_rhs(y, a);
    Particle k2 = doran_rhs(particle_add(y, particle_smul(dt * 0.5, k1)), a);
    Particle k3 = doran_rhs(particle_add(y, particle_smul(dt * 0.5, k2)), a);
    Particle k4 = doran_rhs(particle_add(y, particle_smul(dt, k3)), a);

    // y + (dt/6) * (k1 + 2*k2 + 2*k3 + k4)
    Particle sum = particle_add(k1, particle_smul(2.0, k2));
    sum = particle_add(sum, particle_smul(2.0, k3));
    sum = particle_add(sum, k4);

    return particle_add(y, particle_smul(dt / 6.0, sum));
}

// ============================================================================
// Colormap - matches CPU sphericalColormap exactly
// ============================================================================

// Test if angle falls on a grid stripe (10° intervals)
inline bool test_stripe(float angle, float stripeInterval, float stripeHalfWidthRatio) {
    float stripeNumber = angle / stripeInterval;
    float stripeRemainder = stripeNumber - round(stripeNumber);
    return fabs(stripeRemainder) <= stripeHalfWidthRatio;
}

// Spherical coordinates from Cartesian position
inline float3 spherical_from_cartesian_simple(Vector4 pos) {
    float x = pos.v1, y = pos.v2, z = pos.v3;
    float r = sqrt(x*x + y*y + z*z);
    float theta = acos(z / max(r, 1e-10f));  // [0, pi]
    float phi = atan2(y, x);                  // [-pi, pi]
    return float3(r, theta, phi);
}

inline float3 spherical_colormap(Vector4 pos, float extractRadius) {
    float3 spherical = spherical_from_cartesian_simple(pos);
    float r = spherical.x;
    float theta = spherical.y;
    float phi = spherical.z;

    // Check if escaped
    if (r >= extractRadius) {
        // Grid parameters: 10° intervals, 0.05 half-width
        float stripeHalfWidth = 0.05f;
        float stripeInterval = M_PI_F / 18.0f;  // 10 degrees

        // Check if on grid line
        if (test_stripe(theta, stripeInterval, stripeHalfWidth) ||
            test_stripe(phi, stripeInterval, stripeHalfWidth)) {
            return float3(0.0, 0.0, 0.0);  // Black grid line
        }

        // Quadrant colors based on Cartesian y, z
        float y = pos.v2;
        float z = pos.v3;

        if (y > 0.0 && z > 0.0) {
            return float3(1.0, 0.0, 0.0);  // Red
        } else if (y < 0.0 && z > 0.0) {
            return float3(0.0, 1.0, 0.0);  // Green
        } else if (y > 0.0 && z < 0.0) {
            return float3(0.0, 0.0, 1.0);  // Blue
        } else {
            return float3(1.0, 1.0, 0.0);  // Yellow
        }
    }

    // Didn't escape: cyan (fell into black hole or hit max parameter)
    return float3(0.0, 1.0, 1.0);
}

// ============================================================================
// Main kernel
// ============================================================================

struct RenderParams {
    uint width;
    uint height;
    float spinParam;
    float extractRadius;
    float maxParam;
    float maxStepRatio;
    uint maxSteps;
    float dparam0;
    float4 camPos;      // Camera position (t, x, y, z)
    float hFov;         // Horizontal field of view
};

kernel void raytrace_kernel(
    device uchar4* output [[buffer(0)]],
    constant RenderParams& params [[buffer(1)]],
    uint2 gid [[thread_position_in_grid]]
) {
    uint x = gid.x;
    uint y = gid.y;

    if (x >= params.width || y >= params.height) return;

    uint pixelIdx = y * params.width + x;

    // Match CPU camera model (Camera.lean):
    // Angular step per pixel: da = hFov / width
    float da = params.hFov / float(params.width);

    // Width angle: positive = looking left (+y direction)
    // Center of pixel with 0.5 offset
    float widthAngle = (float(x) - float(params.width / 2) + 0.5f) * da;

    // Height angle: match CPU convention
    // CPU: (heightIdx - h/2 + 0.5) * da
    // For y=0 (top), this is negative; for y=h-1 (bottom), positive
    float heightAngle = (float(y) - float(params.height / 2) + 0.5f) * da;

    // Initial position (t, x, y, z)
    Particle state;
    state.position = {params.camPos.x, params.camPos.y, params.camPos.z, params.camPos.w};

    // Baseline momentum: pointing in -x direction, with p^t = -1 for null geodesic
    // CPU uses: centralMom = baselineOutDir - baselineTimeDir = (0,-1,0,0) - (1,0,0,0) = (-1,-1,0,0)
    // Apply rotations for pixel direction (simplified, no full rotor algebra)
    float cosW = cos(widthAngle);
    float sinW = sin(widthAngle);
    float cosH = cos(heightAngle);
    float sinH = sin(heightAngle);

    // Rotate baseline direction (-1, 0, 0) by widthAngle in xy plane, then heightAngle in xz plane
    // After width rotation: (-cosW, sinW, 0)
    // After height rotation: (-cosW*cosH, sinW, -cosW*sinH) - but we need to be careful about order

    // Actually, let's match the CPU more directly:
    // CPU rotates (0, -1, 0, 0) in out-left plane by widthAngle, then in up-out plane by heightAngle
    // out = -x = (0, -1, 0, 0), left = +y = (0, 0, 1, 0), up = +z = (0, 0, 0, 1)

    // Simple approximation: small angle rotations
    // px = -cos(widthAngle)*cos(heightAngle) ≈ -1 for small angles
    // py = sin(widthAngle)
    // pz = cos(widthAngle)*sin(heightAngle)

    float px = -cosW * cosH;
    float py = sinW;
    float pz = -cosW * sinH;  // Note: negative because heightAngle convention

    // For null geodesic: p^t = -|p_spatial| (future-directed ingoing)
    // CPU has p^t = -1, and spatial part is unnormalized
    // Match CPU: p = (-1, px, py, pz) where |spatial| ≈ 1
    float pMag = sqrt(px*px + py*py + pz*pz);

    // Null condition: -pt^2 + px^2 + py^2 + pz^2 = 0 => pt = -|p_spatial|
    state.momentum = {-pMag, px, py, pz};

    // Integration loop with adaptive stepping
    // Match CPU: integrateAdaptive checks stop BEFORE step, adjusts dparam AFTER step
    float param = 0.0;
    float dparam = params.dparam0;

    for (uint step = 0; step < params.maxSteps; step++) {
        // Check escape - use spherical r, not just Cartesian distance
        float x = state.position.v1, y = state.position.v2, z = state.position.v3;
        float r = sqrt(x*x + y*y + z*z);
        if (r >= params.extractRadius) break;

        // Check max param
        if (param >= params.maxParam) break;

        // Check step ratio (blueshift): CPU checks dparam0/dparam >= maxRatio
        // After adjustment, dparam = dparam0/|pt|, so this becomes |pt| >= maxRatio
        float pt = fabs(state.momentum.v0);
        if (params.dparam0 / dparam >= params.maxStepRatio) break;

        // RK4 step
        state = rk4_step(state, dparam, params.spinParam);
        param += dparam;

        // Adjust step size based on blueshift (for NEXT iteration)
        pt = fabs(state.momentum.v0);  // Use updated momentum
        float ratio = (pt > 1e-10) ? pt : 1e-10;
        dparam = params.dparam0 / ratio;
    }

    // Colormap
    float3 color = spherical_colormap(state.position, params.extractRadius);

    // Output as RGBA
    output[pixelIdx] = uchar4(
        uchar(clamp(color.x * 255.0, 0.0, 255.0)),
        uchar(clamp(color.y * 255.0, 0.0, 255.0)),
        uchar(clamp(color.z * 255.0, 0.0, 255.0)),
        255
    );
}
