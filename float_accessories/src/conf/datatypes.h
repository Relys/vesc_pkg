// Copyright 2022 Benjamin Vedder <benjamin@vedder.se>
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

#ifndef DATATYPES_H_
#define DATATYPES_H_

#include <stdbool.h>
#include <stdint.h>

typedef enum {
    INPUTTILT_NONE = 0,
    INPUTTILT_UART,
    INPUTTILT_PPM
} FLOAT_INPUTTILT_REMOTE_TYPE;

typedef enum {
    PARKING_BRAKE_ALWAYS = 0,
    PARKING_BRAKE_IDLE,
    PARKING_BRAKE_NEVER
} ParkingBrakeMode;

typedef enum {
    LED_MODE_OFF = 0,
    LED_MODE_INTERNAL,
    LED_MODE_EXTERNAL,
} LedMode;

typedef enum {
    LED_PIN_B6 = 0,
    LED_PIN_B7
} LedPin;

typedef enum {
    LED_COLOR_GRB = 0,
    LED_COLOR_GRBW,
    LED_COLOR_RGB,
    LED_COLOR_WRGB,
    LED_COLOR_RGBW
} LedColorOrder;

typedef enum {
    LED_STRIP_ORDER_NONE = 0,
    LED_STRIP_ORDER_1ST,
    LED_STRIP_ORDER_2ND,
    LED_STRIP_ORDER_3RD
} LedStripOrder;

typedef enum {
    COLOR_BLACK = 0,
    COLOR_WHITE_FULL,
    COLOR_WHITE_RGB,
    COLOR_WHITE_SINGLE,
    COLOR_RED,
    COLOR_FERRARI,
    COLOR_FLAME,
    COLOR_CORAL,
    COLOR_SUNSET,
    COLOR_SUNRISE,
    COLOR_GOLD,
    COLOR_ORANGE,
    COLOR_YELLOW,
    COLOR_BANANA,
    COLOR_LIME,
    COLOR_ACID,
    COLOR_SAGE,
    COLOR_GREEN,
    COLOR_MINT,
    COLOR_TIFFANY,
    COLOR_CYAN,
    COLOR_STEEL,
    COLOR_SKY,
    COLOR_AZURE,
    COLOR_SAPPHIRE,
    COLOR_BLUE,
    COLOR_VIOLET,
    COLOR_AMETHYST,
    COLOR_MAGENTA,
    COLOR_PINK,
    COLOR_FUCHSIA,
    COLOR_LAVENDER,
} LedColor;

typedef enum {
    LED_ANIM_SOLID = 0,
    LED_ANIM_FADE,
    LED_ANIM_PULSE,
    LED_ANIM_STROBE,
    LED_ANIM_KNIGHT_RIDER,
    LED_ANIM_FELONY,
    LED_ANIM_RAINBOW_CYCLE,
    LED_ANIM_RAINBOW_FADE,
    LED_ANIM_RAINBOW_ROLL,
} LedAnimMode;

typedef enum {
    LED_TRANS_FADE = 0,
    LED_TRANS_FADE_OUT_IN,
    LED_TRANS_CIPHER,
    LED_TRANS_MONO_CIPHER,
} LedTransition;

typedef struct {
    float brightness;
    LedColor color1;
    LedColor color2;
    LedAnimMode mode;
    float speed;
} LedBar;

typedef struct {
    uint16_t idle_timeout;
    float duty_threshold;
    float red_bar_percentage;
    bool show_sensors_while_running;
    float brightness_headlights_on;
    float brightness_headlights_off;
} StatusBar;

typedef struct {
    bool on;
    bool headlights_on;
    bool highbeams_on;

    LedTransition headlights_transition;
    LedTransition direction_transition;

    bool lights_off_when_lifted;
    bool status_on_front_when_lifted;

    LedBar headlights;
    LedBar taillights;
    LedBar front;
    LedBar rear;
    StatusBar status;
    LedBar status_idle;
} CfgLeds;

typedef enum {
    CUSTOM,
    LASERBEAMS
} LedStripType;

typedef struct {
    uint8_t pin;
    LedStripType strip_type;
    LedStripOrder order;
    uint8_t count;
    LedColorOrder color_order;
    bool reverse;
} CfgLedStrip;

typedef struct {
    bool enabled;
    uint8_t flicker;
    CfgLedStrip status;
    CfgLedStrip front;
    CfgLedStrip rear;
} CfgHwLeds;

typedef struct {
    bool enabled;
    uint32_t mac_addr_hi, mac_addr_lo;
    uint16_t secret_code;
} CfgPubmote;

typedef struct {
    CfgHwLeds leds;
    CfgPubmote pubmote;
} CfgHardware;

typedef struct {
    uint16_t frequency;
    float strength;
} CfgHapticTone;

typedef struct {
    CfgHapticTone duty;
    CfgHapticTone error;
    CfgHapticTone vibrate;
    float min_strength;
    float strength_curvature;
    float max_strength_speed;
    float duty_solid_offset;
    float current_threshold;
} CfgHapticFeedback;

typedef enum {
    SFT_NONE = 0,
    SFT_THREE_STAGE,
    SFT_EMA3,
    SFT_NICO
} TargetFilterType;

typedef struct {
    TargetFilterType type;
    TargetFilterType tt_type;
    TargetFilterType it_type;
    float alpha;
    float in_alpha_away;
    float in_alpha_back;
    float ema_half_time;
    float ema_return_multiplier;
} CfgTargetFilter;

typedef struct {
    bool is_default;
} CfgMeta;

typedef enum {
	FS_NONE = 0,
	FS_LEFT = 1,
	FS_RIGHT = 2,
	FS_BOTH = 3
} FootpadSensorState;

typedef struct {
	float adc1, adc2;
	FootpadSensorState state;
} FootpadSensor;

typedef enum {
    STATE_DISABLED = 0,
    STATE_STARTUP = 1,
    STATE_READY = 2,
    STATE_RUNNING = 3
} RunState;

typedef enum {
    MODE_NORMAL = 0,
    MODE_HANDTEST = 1,
    MODE_FLYWHEEL = 2
} Mode;

typedef enum {
    STOP_NONE = 0,
    STOP_PITCH = 1,
    STOP_ROLL = 2,
    STOP_SWITCH_HALF = 3,
    STOP_SWITCH_FULL = 4,
    STOP_REVERSE_STOP = 5,
    STOP_QUICKSTOP = 6
} StopCondition;

// leaving gaps for more states inbetween the different "classes" of the types
// (normal / warning / error)
typedef enum {
    SAT_NONE = 0,
    SAT_CENTERING = 1,
    SAT_REVERSESTOP = 2,
    SAT_PB_DUTY = 6,
    SAT_PB_HIGH_VOLTAGE = 10,
    SAT_PB_LOW_VOLTAGE = 11,
    SAT_PB_TEMPERATURE = 12
} SetpointAdjustmentType;

typedef struct {
    RunState state;
    Mode mode;
    SetpointAdjustmentType sat;
    StopCondition stop_condition;
    bool charging;
    bool wheelslip;
    bool darkride;
} State;

typedef struct {
    float erpm;
    float abs_erpm;
    float abs_erpm_smooth;
    float last_erpm;
    int8_t erpm_sign;

    float speed;

    float current;  //  "regular" motor current (positive = accelerating, negative = braking)
    float dir_current;  // directional current (sign represents direction of torque generation)
    float filt_current;  // filtered directional current
    bool braking;

    float duty_cycle;
    float duty_raw;

    // an average calculated over last ACCEL_ARRAY_SIZE values
    float acceleration;

    float batt_current;
    float batt_voltage;

    float mosfet_temp;
    float motor_temp;

    float current_min;
    float current_max;
    float battery_current_min;
    float battery_current_max;
} MotorData;

typedef struct {
    float pitch;
    float balance_pitch;
    float roll;
    float yaw;
    float gyro_y;

    float flywheel_pitch_offset;
    float flywheel_roll_offset;
} IMU;

typedef struct {
    float on_step_size;
    float off_step_size;
    float ramped_step_size;

    float target;
    float setpoint;
} TorqueTilt;

typedef struct {
    float on_step_size;
    float off_step_size;
    float ramped_step_size;

    float accel_diff;
    float speed_boost;

    float target;
    float setpoint;

    float speed_boost_mult;
} ATR;

typedef struct {
    float factor;
    float target;
    float setpoint;
} BrakeTilt;

typedef struct {
    float step_size;
    float boost_per_erpm;

    float last_yaw_angle;
    float last_yaw_change;
    float yaw_change;
    float abs_yaw_change;
    float yaw_aggregate;

    float target;
    float setpoint;
} TurnTilt;

typedef struct {
    float current;
} Booster;

typedef struct {
    float step_size;

    float input;
    float ramped_step_size;

    float setpoint;
} Remote;

typedef struct {
    float timer;
    float voltage;
    float current;
} Charging;

typedef struct {

    // Optional future fields
    CfgLeds leds;
    CfgHardware hardware;
    bool disabled;
    int16_t can_id;
    CfgMeta meta;

} FloatAccessoriesConfig;

// DATATYPES_H_
#endif
