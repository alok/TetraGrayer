// Fast trig functions for TetraGrayer
// Compile with: clang -O3 -ffast-math -c trig.c -o trig.o

#include <math.h>
#include <lean/lean.h>

// sincos - computes both sin and cos in one call
// Returns a Lean pair (Float × Float)
LEAN_EXPORT lean_obj_res tetra_sincos(double x) {
    double s, c;
    __sincos(x, &s, &c);  // macOS uses __sincos

    // Create Lean pair
    lean_object* pair = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(pair, 0, lean_box_float(s));
    lean_ctor_set(pair, 1, lean_box_float(c));
    return pair;
}

// sinhcosh - for hyperbolic
LEAN_EXPORT lean_obj_res tetra_sinhcosh(double x) {
    double s = sinh(x);
    double c = cosh(x);

    lean_object* pair = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(pair, 0, lean_box_float(s));
    lean_ctor_set(pair, 1, lean_box_float(c));
    return pair;
}
