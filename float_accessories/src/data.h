// Copyright 2025 Syler Clayton
//
// This file is part of the Float Accessories VESC package.
//
// Float Accessories VESC package is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by the
// Free Software Foundation, either version 3 of the License, or (at your
// option) any later version.
//
// Float Accessories VESC package is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
// or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
// more details.
//
// You should have received a copy of the GNU General Public License along with
// this program. If not, see <http://www.gnu.org/licenses/>.

#pragma once

#include <vesc_c_if.h>
#include "conf/datatypes.h"
#include "leds.h"

typedef struct {
    lib_thread main_thread;
    lib_thread aux_thread;

    FloatAccessoriesConfig float_accessories_conf;

    int fw_version_major, fw_version_minor, fw_version_beta;
    int can_id, can_id_bms;
    IMU imu;
    Leds leds;
    FootpadSensor footpad;
    State state;
    MotorData motor;

    TorqueTilt torque_tilt;
    ATR atr;
    BrakeTilt brake_tilt;
    TurnTilt turn_tilt;
    Booster booster;
    Remote remote;
    Charging charging;
    int fault_code;
    float amp_hours;
    float amp_hours_charged;
    float watt_hours;
    float watt_hours_charged;
    int setpoint;
    int beep_reason;
    float battery_percent_remaining;
    float distance_abs;
    float balance_current;
    uint32_t odometer;

    bool humidity_esc_supported; // New feature for controller box humidity sensor.

    bool light_control_sync;
    bool last_control_sync_packet_was_on; // Race condition prevention.
    float prev_brightness;
    float prev_idle_brightness;
    float prev_status_brightness;
    bool lcm_enabled;

    int cell_num;

    uint64_t can_last_activity_time;

} Data;