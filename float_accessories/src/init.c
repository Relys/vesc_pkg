// Copyright 2025 Lukas Hrazky
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

#include "init.h"

void state_init(State *state) {
    state->state = STATE_STARTUP;
    state->mode = MODE_NORMAL;
    state->sat = SAT_NONE;
    state->stop_condition = STOP_NONE;
    state->charging = false;
    state->wheelslip = false;
    state->darkride = false;
}


void imu_init(IMU *imu) {
    imu->pitch = 0.0f;
    imu->balance_pitch = 0.0f;
    imu->roll = 0.0f;
    imu->yaw = 0.0f;
    imu->gyro_y = 0.0f;

    imu->flywheel_pitch_offset = 0.0f;
    imu->flywheel_roll_offset = 0.0f;
}

void torque_tilt_reset(TorqueTilt *tt) {
    tt->setpoint = 0;
    tt->ramped_step_size = 0;
}

void atr_reset(ATR *atr) {
    atr->accel_diff = 0;
    atr->speed_boost = 0;
    atr->target = 0;
    atr->setpoint = 0;
    atr->ramped_step_size = 0;
}

void brake_tilt_reset(BrakeTilt *bt) {
    bt->target = 0;
    bt->setpoint = 0;
}

void turn_tilt_reset(TurnTilt *tt) {
    tt->last_yaw_angle = 0;
    tt->last_yaw_change = 0;
    tt->yaw_change = 0;
    tt->yaw_aggregate = 0;

    tt->target = 0;
    tt->setpoint = 0;
}

void booster_reset(Booster *b) {
    b->current = 0;
}

void remote_init(Remote *remote) {
    remote->input = 0;
    remote->ramped_step_size = 0;
    remote->setpoint = 0;
}

void charging_init(Charging *charging) {
    charging->timer = 0.0f;
    charging->voltage = 0.0f;
    charging->current = 0.0f;
}

void motor_data_reset(MotorData *m) {
    m->abs_erpm_smooth = 0;
    m->duty_raw = 0;

    m->acceleration = 0;
}