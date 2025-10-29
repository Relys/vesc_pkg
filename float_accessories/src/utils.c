// Copyright 2024 Lukas Hrazky
//
// This file is part of the Refloat VESC package.
//
// Refloat VESC package is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by the
// Free Software Foundation, either version 3 of the License, or (at your
// option) any later version.
//
// Refloat VESC package is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
// or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
// more details.
//
// You should have received a copy of the GNU General Public License along with
// this program. If not, see <http://www.gnu.org/licenses/>.

#include "utils.h"
#include <limits.h>

uint32_t rnd(uint32_t seed) {
    return seed * 1664525u + 1013904223u;
}

void rate_limitf(float *value, float target, float step) {
    if (fabsf(target - *value) < step) {
        *value = target;
    } else if (target - *value > 0) {
        *value += step;
    } else {
        *value -= step;
    }
}
float clampf(float value, float min, float max) {
    const float m = value < min ? min : value;
    return m > max ? max : m;
}

float map_range(float x, float out_min, float out_max) {
    if (x < 0.0f) x = 0.0f;
    if (x > 1.0f) x = 1.0f;
    return out_min + (out_max - out_min) * x;
}

// 8-bit scale with clamp & round
uint8_t scale8(uint8_t v, float s) {
    int t = (int)(v * s + 0.5f);
    if (t < 0)   t = 0;
    if (t > 255) t = 255;
    return (uint8_t)t;
}


// mini_math.c  — PIC-safe float math for rv32imc blobs (no libm, no F-ops)
// Implements: fabsf, floorf, ceilf, roundf, lroundf, fminf, fmaxf, fmodf, powf
// Notes:
//  - powf: supports only integer exponents; non-integer -> quiet NaN
//  - fmodf: x - trunc(x/y)*y (C semantics; same sign as x)
//  - All functions avoid absolute-addressed tables; safe for runtime-loaded PIC.

/* -------- bit helpers (no libm needed) -------- */

static inline uint32_t uf(float x){ union { float f; uint32_t u; } v = { x }; return v.u; }
static inline float   ff(uint32_t u){ union { float f; uint32_t u; } v = { .u = u }; return v.f; }

static inline int is_nan_f(float x){
    uint32_t u = uf(x);
    return ((u & 0x7f800000u) == 0x7f800000u) && (u & 0x007fffffu);
}
static inline int is_inf_f(float x){
    return (uf(x) & 0x7fffffffU) == 0x7f800000U;
}
static inline int signbit_f(float x){
    return (int)(uf(x) >> 31);
}
static inline float qnanf_(void){
    // quiet NaN payload: 0x7FC00000
    return ff(0x7fc00000u);
}

/* -------- fabsf -------- */

float fabsf(float x){
    return ff(uf(x) & 0x7fffffffU);
}

/* -------- floorf (no libm; PIC-safe) -------- */

float floorf(float x) {
    uint32_t u = uf(x);
    uint32_t s = u >> 31;
    uint32_t e = (u >> 23) & 0xFF;
    uint32_t m = u & 0x7FFFFFu;

    if (e == 0xFF) return x;               // NaN/Inf
    if (e < 127) {                          // |x| < 1
        return s && (u<<1) ? -1.0f : 0.0f;  // -fraction -> -1, +fraction -> 0, +/-0 -> 0
    }
    if (e >= 150) return x;                 // |x| >= 2^24 already integral

    uint32_t shift = 150 - e;               // 1..23
    uint32_t frac_mask = (1u << shift) - 1u;
    if ((m & frac_mask) == 0) return x;     // already integral

    // Clear fractional bits
    m &= ~frac_mask;
    uint32_t u_trunc = (s<<31) | (e<<23) | m;
    float t = ff(u_trunc);
    return s ? (float)((int)t - 1) : t;
}

/* -------- ceilf (mirror of floorf) -------- */

float ceilf(float x) {
    uint32_t u = uf(x);
    uint32_t s = u >> 31;
    uint32_t e = (u >> 23) & 0xFF;
    uint32_t m = u & 0x7FFFFFu;

    if (e == 0xFF) return x;               // NaN/Inf
    if (e < 127) {                          // |x| < 1
        return s ? 0.0f : ((u<<1) ? 1.0f : 0.0f);
    }
    if (e >= 150) return x;                 // integral

    uint32_t shift = 150 - e;               // 1..23
    uint32_t frac_mask = (1u << shift) - 1u;
    if ((m & frac_mask) == 0) return x;     // already integral

    // Clear fractional bits
    m &= ~frac_mask;
    uint32_t u_trunc = (s<<31) | (e<<23) | m;
    float t = ff(u_trunc);
    return s ? t : (float)((int)t + 1);
}

/* -------- roundf (half-away-from-zero, per C) -------- */

float roundf(float x){
    uint32_t u = uf(x);
    uint32_t e = (u >> 23) & 0xFF;

    if (e >= 150 || e == 0xFF) return x;  // already integral or NaN/Inf
    if (e < 127) {
        // |x| < 1
        return signbit_f(x) ? ( (u<<1) ? -1.0f : 0.0f ) : ( (u<<1) ? 1.0f : 0.0f );
    }
    // 1 <= |x| < 2^24
    int i = (int)x;             // trunc toward zero
    float fi = (float)i;
    float frac = x - fi;
    if (frac > 0.5f) return (float)(i + 1);
    if (frac < -0.5f) return (float)(i - 1);
    if (frac == 0.5f) return (float)(i + 1);   // half away from zero
    if (frac == -0.5f) return (float)(i - 1);
    return fi;
}

/* -------- lroundf (RV32: long is 32-bit; half-away-from-zero) -------- */

long lroundf(float x){
#if LONG_MAX == 2147483647L
    // Saturate to 32-bit long range if needed
    if (!is_nan_f(x)) {
        if (x > 2147483647.0f) return 2147483647L;
        if (x < -2147483648.0f) return -2147483648L;
    }
#endif
    float r = roundf(x);
    return (long)r;  // trunc cast is safe after round
}

/* -------- fminf / fmaxf (NaN rules: if one arg is NaN, return the other; if both NaN -> NaN) -------- */

float fminf(float a, float b){
    int na = is_nan_f(a), nb = is_nan_f(b);
    if (na && nb) return qnanf_();
    if (na) return b;
    if (nb) return a;
    // Preserve -0.0 for min(-0.0, +0.0)
    if (a == b) return signbit_f(a) ? a : b;
    return (a < b) ? a : b;
}

float fmaxf(float a, float b){
    int na = is_nan_f(a), nb = is_nan_f(b);
    if (na && nb) return qnanf_();
    if (na) return b;
    if (nb) return a;
    // Preserve +0.0 for max(-0.0, +0.0)
    if (a == b) return signbit_f(a) ? b : a;
    return (a > b) ? a : b;
}

/* -------- fmodf: x - trunc(x/y)*y; if y==0 -> NaN -------- */

float fmodf(float x, float y){
    if (is_nan_f(x) || is_nan_f(y) || y == 0.0f) return qnanf_();
    if (is_inf_f(x) && !is_nan_f(y)) return qnanf_();
    if (is_inf_f(y)) return x;

    float q = x / y;            // soft-float helper, OK
    int qi = (int)q;            // trunc toward zero
    float r = x - (float)qi * y;
    // Ensure sign of result is same as x; if r == 0, sign must be sign of x
    if (r == 0.0f) {
        // Force signed zero via bit trick
        return signbit_f(x) ? ff(0x80000000u) : 0.0f;
    }
    return r;
}

/* -------- powf: integer exponents only (fast, PIC-safe) -------- */

float powf(float x, float y){
    // If y is NaN or not finite → limited handling
    if (is_nan_f(y)) return qnanf_();

    // Detect if y is an integer within safe range
    // For |y| < 2^24, all integers are exactly representable in float.
    // Check: y == (float)(int)y
    int yi = (int)y;
    if ((float)yi == y) {
        // Integer exponent: exponentiation by squaring
        int n = yi;
        if (n == 0) return 1.0f;

        // Handle negative exponent
        int neg = 0;
        if (n < 0) { neg = 1; n = -n; }

        // Special bases
        if (x == 1.0f) return 1.0f;
        if (x == -1.0f) return (n & 1) ? -1.0f : 1.0f;
        if (x == 0.0f)  return neg ? qnanf_() : 0.0f;  // 0^(-n) undefined -> NaN; 0^(+n)=0
        if (is_nan_f(x)) return qnanf_();

        float result = 1.0f;
        float base = x;
        while (n) {
            if (n & 1) result = result * base;   // soft-float mul
            n >>= 1;
            if (n) base = base * base;
        }
        if (neg) {
            // 1/result (handle division by zero -> Inf as per float rules)
            result = 1.0f / result;
        }
        return result;
    }

    // Non-integer exponent not supported in this minimal PIC-safe build
    return qnanf_();
}
