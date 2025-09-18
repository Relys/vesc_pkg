
// Copyright 2022 Benjamin Vedder <benjamin@vedder.se>
// Copyright 2024 Lukas Hrazky
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
#include "conf/datatypes.h"

void state_init(State *state);

void imu_init(IMU *imu);

void torque_tilt_reset(TorqueTilt *tt);

void atr_reset(ATR *atr);

void brake_tilt_reset(BrakeTilt *bt);

void turn_tilt_reset(TurnTilt *tt);

void booster_reset(Booster *b);

void remote_init(Remote *remote);

void charging_init(Charging *charging);

void motor_data_reset(MotorData *m);