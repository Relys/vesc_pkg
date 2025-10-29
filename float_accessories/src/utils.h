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

#pragma once

#include "vesc_c_if.h"

#include <math.h>
#include <stdint.h>

#define ERPM_MOVING_THRESHOLD 10.0f

#define unused(x) (void) (x)

#define deg2rad(deg) ((deg) * (M_PI / 180.0f))
#define rad2deg(rad) ((rad) * (180.0f / M_PI))

uint32_t rnd(uint32_t seed);

float clampf(float value, float min, float max);

/**
 * Rate-limits @p value towards @p target by an amount of maximum value of @p step.
 *
 * If the difference between @p value and @p target is greater than step, @p
 * value is increased or decreased (if @p target is greater or less than @p
 * value respectively) by @p step. Otherwise, @p value is set to @p target.
 *
 * @param value A pointer to value to rate-limit towards @p target.
 * @param target A target to rate-limit @p value towards.
 * @param step A maximum unit of change of @p value.
 */
void rate_limitf(float *value, float target, float step);

float map_range(float x, float out_min, float out_max);
uint8_t scale8(uint8_t v, float s);

float powf(float x, float y);
float fmodf(float x, float y);
float fmaxf(float a, float b);
float fminf(float a, float b);
long lroundf(float x);
float roundf(float x);
float ceilf(float x);
float floorf(float x);
float fabsf(float x);