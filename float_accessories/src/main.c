// Copyright 2019 - 2022 Mitch Lustig
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

#include <stdint.h>
#include <string.h>

#include "vesc_c_if.h"

#include "conf/buffer.h"
#include "conf/conf_general.h"
#include "conf/confparser.h"
#include "conf/confxml.h"
#include "conf/datatypes.h"
#include "data.h"
#include "init.h"
#include "led_strip.h"
#include "leds.h"
#include "utils.h"

HEADER
enum {
    COMMAND_INFO = 0,  // get version / package info
    COMMAND_GET_RTDATA = 1,  // get rt data
    COMMAND_RT_TUNE = 2,  // runtime tuning (don't write to eeprom)
    COMMAND_TUNE_DEFAULTS = 3,  // set tune to defaults (no eeprom)
    COMMAND_CFG_SAVE = 4,  // save config to eeprom
    COMMAND_CFG_RESTORE = 5,  // restore config from eeprom
    COMMAND_TUNE_OTHER = 6,  // make runtime changes to startup/etc
    COMMAND_RC_MOVE = 7,  // move motor while board is idle
    COMMAND_BOOSTER = 8,  // change booster settings
    COMMAND_PRINT_INFO = 9,  // print verbose info
    COMMAND_GET_ALLDATA = 10,  // send all data, compact
    COMMAND_EXPERIMENT = 11,  // generic cmd for sending data, used for testing/tuning new features
    COMMAND_LOCK = 12,
    COMMAND_HANDTEST = 13,
    COMMAND_TUNE_TILT = 14,
    COMMAND_FLYWHEEL = 22,
    COMMAND_REALTIME_DATA = 31,
    COMMAND_REALTIME_DATA_IDS = 32,
    COMMAND_DATA_RECORD_REQUEST = 41,

    // commands above 200 are unstable and can change protocol at any time
    COMMAND_LIGHTS_CONTROL = 202,
} Commands;

typedef enum {
    COMMAND_LCM_POLL = 24,  // this should only be called by external light modules
    COMMAND_LCM_LIGHT_INFO = 25,  // to be called by apps to get lighting info
    COMMAND_LCM_LIGHT_CTRL = 26,  // to be called by apps to change light settings
    COMMAND_LCM_DEVICE_INFO = 27,  // to be called by apps to check lighting controller firmware
    COMMAND_LCM_GET_BATTERY = 29,

    COMMAND_LCM_DEBUG = 99,  // reserved for external debug purposes
} LcmCommands;

#define COMMAND_HUMIDITY 51

#define FLOAT_MAGIC 101
#define FLOAT_ACCESSORIES_QML_MAGIC 103
#define FLOAT_ACCESSORIES_MAGIC 104
#define COMM_FORWARD_CAN 34
#define COMM_CUSTOM_APP_DATA 36

// TODO: The required buffer size is not provided by the confparser. Until it's
// added, use a number that should always be bigger. On a config write, we
// check if we've written past the buffer end and crash. On a config read,
// there's no way to check and the trailing end of the config will have bogus
// numbers read from the EEPROM.

#ifndef SERIALIZED_CONFIG_LENGTH
#define SERIALIZED_CONFIG_LENGTH 320
#endif

static void write_cfg_to_eeprom(Data *d) {
    const size_t words = (SERIALIZED_CONFIG_LENGTH - 1) / 4 + 1;
    const size_t bufsize = words * 4;
    uint32_t *buffer = VESC_IF->malloc(bufsize);
    if (!buffer) {
        VESC_IF->printf("Failed to write config: Out of memory.");
        return;
    }

    memset(buffer, 0, bufsize);

    uint32_t written_bytes =
        confparser_serialize_floataccessoriesconfig((uint8_t *) buffer, &d->float_accessories_conf);
    if (written_bytes > bufsize) {
        VESC_IF->printf("Config write buffer overflow, terminating.");
        // fatal_error_terminate(); //TODO
    }

    bool write_ok = true;
    for (uint32_t i = 0; i < words; ++i) {
        eeprom_var v;
        v.as_u32 = buffer[i];
        if (!VESC_IF->store_eeprom_var(&v, i)) {
            write_ok = false;
            break;
        }
    }

    VESC_IF->free(buffer);

    if (write_ok) {
        VESC_IF->printf("Config written: %uB", written_bytes);
        // beep_alert(d, 1, 0);
        leds_status_confirm(&d->leds);
    } else {
        VESC_IF->printf("Failed to write config.");
    }
}

static void read_cfg_from_eeprom(Data *d) {
    uint32_t words = (SERIALIZED_CONFIG_LENGTH - 1) / 4 + 1;
    uint32_t *buffer = VESC_IF->malloc(words * sizeof(uint32_t));
    if (!buffer) {
        VESC_IF->printf("Failed to read config: Out of memory.");
        return;
    }

    eeprom_var v;
    bool read_ok = true;
    for (uint32_t i = 0; i < words; ++i) {
        if (!VESC_IF->read_eeprom_var(&v, i)) {
            read_ok = false;
            break;
        }
        buffer[i] = v.as_u32;
    }

    if (read_ok) {
        if (!confparser_deserialize_floataccessoriesconfig(
                (uint8_t *) buffer, &d->float_accessories_conf
            )) {
            VESC_IF->printf("Failed to deserialize config, using defaults.");
            confparser_set_defaults_floataccessoriesconfig(&d->float_accessories_conf);
        }
    } else {
        VESC_IF->printf("Failed to read config, using defaults.");
        confparser_set_defaults_floataccessoriesconfig(&d->float_accessories_conf);
    }
    VESC_IF->free(buffer);
}

// Used to send the current or default configuration to VESC Tool.
static int get_cfg(uint8_t *buffer, bool is_default) {
    Data *d = (Data *) ARG;
    FloatAccessoriesConfig *cfg;
    if (is_default) {
        cfg = VESC_IF->malloc(sizeof(FloatAccessoriesConfig));
        if (!cfg) {
            VESC_IF->printf("Failed to send default config to VESC tool: Out of memory.");
            return 0;
        }
        confparser_set_defaults_floataccessoriesconfig(cfg);
    } else {
        cfg = &d->float_accessories_conf;
    }

    int res = confparser_serialize_floataccessoriesconfig(buffer, cfg);

    if (is_default) {
        VESC_IF->free(cfg);
    }

    return res;
}

void send_message(const char *text, bool popup) {

    size_t text_len = strlen(text);
    int msg_size = 2 + text_len; // magic + cmd + text (no '\0')

    uint8_t *msg = VESC_IF->malloc(msg_size);
    if (!msg) {
        VESC_IF->printf("send_message: malloc failed\n");
        return;
    }

    msg[0] = FLOAT_ACCESSORIES_QML_MAGIC;
    if (popup) {
        msg[1] = 0;
    } else {
        msg[1] = 1;
    }

    memcpy(&msg[2], text, text_len);

    VESC_IF->send_app_data(msg, msg_size, 0, 0);
    VESC_IF->free(msg);
}

// Used to set and write configuration from VESC Tool.
static bool set_cfg(uint8_t *buffer) {
    Data *d = (Data *) ARG;
    // don't let users use the Float Accessories Cfg "write" button in special modes
    // if (d->state.mode != MODE_NORMAL) {
    //    return false;
    //}
    int old_status_pin = d->float_accessories_conf.hardware.leds.status.pin;
    int old_front_pin = d->float_accessories_conf.hardware.leds.front.pin;
    int old_rear_pin = d->float_accessories_conf.hardware.leds.rear.pin;
    bool res = confparser_deserialize_floataccessoriesconfig(buffer, &d->float_accessories_conf);

    // don't allow to disable the package in the RUNNING state
    // if (d->state.state == STATE_RUNNING) {
    //    d->float_conf.disabled = false;
    //}

    if (!(
            (d->float_accessories_conf.hardware.leds.status.pin ==
                 d->float_accessories_conf.hardware.leds.front.pin &&
             d->float_accessories_conf.hardware.leds.front.pin ==
                 d->float_accessories_conf.hardware.leds.rear.pin) ||
            (d->float_accessories_conf.hardware.leds.status.pin !=
                 d->float_accessories_conf.hardware.leds.front.pin &&
             d->float_accessories_conf.hardware.leds.front.pin !=
                 d->float_accessories_conf.hardware.leds.rear.pin &&
             d->float_accessories_conf.hardware.leds.status.pin !=
                 d->float_accessories_conf.hardware.leds.rear.pin)
        ))  // Only support chaining all together or seprate.
    {
        d->float_accessories_conf.hardware.leds.status.pin = old_status_pin;
        d->float_accessories_conf.hardware.leds.front.pin = old_front_pin;
        d->float_accessories_conf.hardware.leds.rear.pin = old_rear_pin;
        send_message("Error: LED Pins must be on a single strip or all on separate strips.", true);
        //return false; Let's actually continue to save, just have the pins reverted.
    }

    // Always reset the is_default flag on writing - whatever we write we
    // consider to not be the default config anymore
    d->float_accessories_conf.meta.is_default = false;

    // Store to EEPROM
    if (res) {
        write_cfg_to_eeprom(d);
        // configure(d);
        leds_configure(&d->leds, &d->float_accessories_conf.leds);
    }

    return res;
}

static int get_cfg_xml(uint8_t **buffer) {
    // Note: As the address of data_float_accessories_config_ is not known
    // at compile time it will be relative to where it is in the
    // linked binary. Therefore we add PROG_ADDR to it so that it
    // points to where it ends up on the ESP32.
    *buffer = data_floataccessoriesconfig_ + PROG_ADDR;
    return DATA_FLOATACCESSORIESCONFIG__SIZE;
}

static void main_thd(void *arg) {
    Data *d = (Data *) arg;

    bool check_humidity = false;
    int command_get_alldata_buf_size = 3;
    uint8_t *command_get_alldata_buf = VESC_IF->malloc(command_get_alldata_buf_size);
    command_get_alldata_buf[0] = FLOAT_MAGIC;
    command_get_alldata_buf[1] = COMMAND_GET_ALLDATA;
    command_get_alldata_buf[2] = 3;

    int command_get_hum_buf_size = 2;
    uint8_t *command_get_hum_buf = VESC_IF->malloc(command_get_hum_buf_size);
    command_get_hum_buf[0] = FLOAT_MAGIC;
    command_get_hum_buf[1] = COMMAND_HUMIDITY;

    int command_lights_size = 2;
    uint8_t *command_lights = VESC_IF->malloc(command_lights_size);
    command_lights[0] = FLOAT_MAGIC;
    command_lights[1] = COMMAND_LCM_LIGHT_INFO;

    int command_light_ctrl_size = 5;
    uint8_t *command_light_ctrl = VESC_IF->malloc(command_light_ctrl_size);
    command_light_ctrl[0] = FLOAT_MAGIC;
    command_light_ctrl[1] = COMMAND_LCM_LIGHT_CTRL;
    
    //VESC_IF->thread_set_priority(-2);
    while (!VESC_IF->should_terminate()) {
        uint32_t time = 500;
        if (d->can_id >= 0) {
            //TODO Let's slow doing how much we're polling can bus if the float package is disabled. Hopefully that will fix motor conf.

            if(d->state.state == STATE_DISABLED)
            {
                time = 1000;
                command_get_alldata_buf[2]=1;
            } else {
                time = 500;
                command_get_alldata_buf[2]=3;

                 if(!d->light_control_sync && d->lcm_enabled)
                {

                    if(d->leds.cfg->headlights_on)
                    {
                        command_light_ctrl[2] = (uint8_t)d->leds.cfg->headlights.brightness*100;
                        d->prev_brightness=d->leds.cfg->headlights.brightness;
                        command_light_ctrl[4] = (uint8_t)d->leds.cfg->status.brightness_headlights_on*100;
                        d->prev_status_brightness=d->leds.cfg->status.brightness_headlights_on;
                        d->last_control_sync_packet_was_on = true;
                    } else {
                        command_light_ctrl[2] = (uint8_t)d->leds.cfg->front.brightness*100;
                        d->prev_brightness=d->leds.cfg->front.brightness;
                        command_light_ctrl[4] = (uint8_t)d->leds.cfg->status.brightness_headlights_off*100;
                        d->prev_status_brightness=d->leds.cfg->status.brightness_headlights_off;
                        d->last_control_sync_packet_was_on = false;
                    }
                    command_light_ctrl[3] = (uint8_t)d->leds.cfg->front.brightness*100;
                    d->prev_idle_brightness=d->leds.cfg->front.brightness;

                    VESC_IF->send_app_data(
                    command_light_ctrl, command_light_ctrl_size, 2, d->can_id);
                }
                VESC_IF->send_app_data(
                command_lights, command_lights_size, 2, d->can_id); 
                
            }
            VESC_IF->send_app_data(
                command_get_alldata_buf, command_get_alldata_buf_size, 2, d->can_id);

            if (!check_humidity) {
                VESC_IF->send_app_data(command_get_hum_buf, command_get_hum_buf_size, 2, d->can_id);
                check_humidity = true;

            }
        }
        VESC_IF->sleep_ms(500);
    }
    VESC_IF->free(command_get_alldata_buf);
    VESC_IF->free(command_get_hum_buf);
    VESC_IF->free(command_lights);
    VESC_IF->free(command_light_ctrl);
}

static void aux_thd(void *arg) {
    Data *d = (Data *) arg;
    VESC_IF->thread_set_priority(+2);

    while (!VESC_IF->should_terminate()) {
        leds_update(
            &d->leds,
            &d->state,
            d->footpad.state,
            d->imu.pitch,
            d->motor.erpm,
            d->motor.duty_cycle,
            d->battery_percent_remaining/100.0,
            d->distance_abs, false
        ); //TODO implement is charging function
        VESC_IF->sleep_us(1e6 / LEDS_REFRESH_RATE);
    }
}

// Called when code is stopped
static void stop(void *arg) {
    Data *d = (Data *) arg;
    VESC_IF->set_app_data_handler(NULL);
    VESC_IF->conf_custom_clear_configs();
    if (d->aux_thread) {
        VESC_IF->request_terminate(d->aux_thread);
    }
    if (d->main_thread) {
        VESC_IF->request_terminate(d->main_thread);
    }
    VESC_IF->printf("Terminating.");
    leds_destroy(&d->leds);
    VESC_IF->free(d);
}

static SetpointAdjustmentType decode_sat(uint8_t sat_code) {
    switch (sat_code & 0x0F) {
    case 0:
        return SAT_CENTERING;
    case 1:
        return SAT_REVERSESTOP;
    case 2:
        return SAT_NONE;
    case 3:
        return SAT_PB_DUTY;
    case 4:
        return SAT_PB_HIGH_VOLTAGE;
    case 5:
        return SAT_PB_LOW_VOLTAGE;
    case 6:
        return SAT_PB_TEMPERATURE;
    // case 7: return SAT_PB_ALERT; //TODO
    default:
        return SAT_CENTERING;
    }
}

static void decode_state_compat(uint8_t code, State *st) {
    st->charging = false;
    st->wheelslip = false;
    st->darkride = false;

    switch (code & 0x0F) {
    case 14:
        st->charging = true;
        st->state = STATE_READY;
        st->stop_condition = STOP_NONE;
        st->mode = MODE_NORMAL;
        return;
    case 15:
        st->state = STATE_DISABLED;
        st->stop_condition = STOP_NONE;
        st->mode = MODE_NORMAL;
        return;
    case 0:
        st->state = STATE_STARTUP;
        st->stop_condition = STOP_NONE;
        st->mode = MODE_NORMAL;
        return;
    case 1:
        st->state = STATE_RUNNING;
        st->stop_condition = STOP_NONE;
        return;
    case 2:
        st->state = STATE_RUNNING;
        st->stop_condition = STOP_NONE;
        return;  // tiltback via sat
    case 3:
        st->state = STATE_RUNNING;
        st->wheelslip = true;
        st->stop_condition = STOP_NONE;
        return;
    case 4:
        st->state = STATE_RUNNING;
        st->darkride = true;
        st->stop_condition = STOP_NONE;
        return;
    case 5:
        st->state = STATE_RUNNING;
        st->stop_condition = STOP_NONE;
        st->mode = MODE_FLYWHEEL;
        return;
    case 6:
        st->state = STATE_READY;
        st->stop_condition = STOP_PITCH;
        return;
    case 7:
        st->state = STATE_READY;
        st->stop_condition = STOP_ROLL;
        return;
    case 8:
        st->state = STATE_READY;
        st->stop_condition = STOP_SWITCH_HALF;
        return;
    case 9:
        st->state = STATE_READY;
        st->stop_condition = STOP_SWITCH_FULL;
        return;
    case 12:
        st->state = STATE_READY;
        st->stop_condition = STOP_REVERSE_STOP;
        return;
    case 13:
        st->state = STATE_READY;
        st->stop_condition = STOP_QUICKSTOP;
        return;
    case 11:
    default:
        st->state = STATE_READY;
        st->stop_condition = STOP_NONE;
        return;
    }
}

static lbm_value ext_update_data(lbm_value *args, lbm_uint argn) {
    Data *d = (Data *) ARG;
    if (argn > 8) {
        // d->can_last_activity_time = VESC_IF->system_time();
        int state_byte = VESC_IF->lbm_dec_as_i32(args[0]);
        int switch_state_byte = VESC_IF->lbm_dec_as_i32(args[1]);
        decode_state_compat(state_byte & 0x0F, &d->state);
        d->state.sat = decode_sat(state_byte >> 4);
        uint8_t fp_low = switch_state_byte & 0x0F;  // includes handtest in bit3
        bool handtest = (fp_low & 0x08) != 0;
        d->footpad.state = (FootpadSensorState) (fp_low & 0x07);
        d->beep_reason = (uint8_t) ((switch_state_byte >> 4) & 0x0F);

        if (handtest) {
            d->state.mode = MODE_HANDTEST;
        } else if (d->state.mode != MODE_FLYWHEEL && d->state.mode != MODE_NORMAL) {
            // only normalize if decode_state_compat didn't already set a concrete mode
            d->state.mode = MODE_NORMAL;
        }
        d->footpad.adc1 = VESC_IF->lbm_dec_as_float(args[2]);
        d->footpad.adc2 = VESC_IF->lbm_dec_as_float(args[3]);
        
        switch(d->footpad.state) {
            case 3:
                d->footpad.state = 3;
                break;
            case 1:
                if(d->footpad.adc2 > d->footpad.adc1) d->footpad.state = 2;
                break;
            default:
                break;
        };

        d->motor.erpm = VESC_IF->lbm_dec_as_float(args[4]);
        d->motor.duty_cycle = VESC_IF->lbm_dec_as_float(args[5]);
        d->distance_abs = VESC_IF->lbm_dec_as_float(args[6]);
        d->battery_percent_remaining = VESC_IF->lbm_dec_as_float(args[7]);
        d->imu.pitch = VESC_IF->lbm_dec_as_float(args[8]);
    }
    return VESC_IF->lbm_enc_sym_true;
}

#define NEED(n)                                                                                    \
    do {                                                                                           \
        if (off + (n) > (int32_t) len)                                                             \
            return;                                                                                \
    } while (0)
// -- receiver -------------------------------------------------------------
static void cmd_recv_all_data(Data *d, const uint8_t *buf, size_t len) {
    if (!buf || len < 3) {
        return;
    }
    if (buf[0] != 101 || buf[1] != COMMAND_GET_ALLDATA) {
        return;
    }

    int32_t off = 2;

    // Fault?
    NEED(1);
    if (buf[off] == 69) {
        NEED(2);
        d->fault_code = buf[off + 1];
        // d->can_last_activity_time = VESC_IF->system_time();
        return;
    }

    NEED(1);
    uint8_t mode = buf[off++];

    // 3× float16/10: balance_current, balance_pitch, roll
    NEED(3 * 2);
    d->balance_current = buffer_get_float16(buf, 10.0f, &off);
    d->imu.balance_pitch = buffer_get_float16(buf, 10.0f, &off);
    d->imu.roll = buffer_get_float16(buf, 10.0f, &off);
    // state/sat nibble, then switch-state+beep
    NEED(2);
    uint8_t state_byte = buf[off++];
    uint8_t switch_state_byte = buf[off++];

    decode_state_compat(state_byte & 0x0F, &d->state);
    d->state.sat = decode_sat(state_byte >> 4);

    uint8_t fp_low = switch_state_byte & 0x0F;  // includes handtest in bit3
    bool handtest = (fp_low & 0x08) != 0;
    d->footpad.state = (FootpadSensorState) (fp_low & 0x07);
    d->beep_reason = (switch_state_byte >> 4);

    if (handtest) {
        d->state.mode = MODE_HANDTEST;
    } else if (d->state.mode != MODE_FLYWHEEL) {
        d->state.mode = MODE_NORMAL;
    }

    // Footpad ADCs: bytes / 50
    NEED(2);
    d->footpad.adc1 = (float) ((int) buf[off++]) / 50.0f;
    d->footpad.adc2 = (float) ((int) buf[off++]) / 50.0f;
    switch(d->footpad.state) {
        case 3:
            d->footpad.state = 3;
            break;
        case 1:
            if(d->footpad.adc2 > d->footpad.adc1) d->footpad.state = 2;
            break;
        default:
            break;
    };
    // Setpoints (6 bytes): (b - 128) / 5
    NEED(6);
    d->setpoint = ((int) buf[off++] - 128) / 5.0f;
    d->atr.setpoint = ((int) buf[off++] - 128) / 5.0f;
    d->brake_tilt.setpoint = ((int) buf[off++] - 128) / 5.0f;
    d->torque_tilt.setpoint = ((int) buf[off++] - 128) / 5.0f;
    d->turn_tilt.setpoint = ((int) buf[off++] - 128) / 5.0f;
    d->remote.setpoint = ((int) buf[off++] - 128) / 5.0f;

    // IMU pitch (f16/10) and booster.current (byte - 128)
    NEED(2 + 1);
    d->imu.pitch = buffer_get_float16(buf, 10.0f, &off);
    d->booster.current = (float) ((int) buf[off++] - 128);

    // Motor block
    NEED(2 + 2 + 2 + 2 + 2 + 1);
    d->motor.batt_voltage = buffer_get_float16(buf, 10.0f, &off);
    d->motor.erpm = buffer_get_int16(buf, &off);

    float speed_mps = buffer_get_float16(buf, 10.0f, &off);
    d->motor.speed = speed_mps * 3.6f;

    d->motor.current = buffer_get_float16(buf, 10.0f, &off);
    d->motor.batt_current = buffer_get_float16(buf, 10.0f, &off);

    d->motor.duty_cycle = ((int) buf[off++] - 128) / 100.0f;

    NEED(1);
    {
        uint8_t idb = buf[off++];
        // TODO
        // d->motor.id_abs = (idb == 222) ? -1.0f : ((float)idb / 3.0f);
    }

    if (mode >= 2) {
        NEED(4 + 3);
        d->distance_abs = buffer_get_float32_auto(buf, &off);
        d->motor.mosfet_temp = (float) buf[off++] / 2.0f;
        d->motor.motor_temp = (float) buf[off++] / 2.0f;

        // TODO
        off++;
        // d->motor.batt_temp   = (float)buf[off++] / 2.0f;
    }

    if (mode >= 3) {
        NEED(4 + 2 + 2 + 2 + 2 + 1);
        d->odometer = buffer_get_uint32(buf, &off);
        d->amp_hours = buffer_get_float16(buf, 10.0f, &off);
        d->amp_hours_charged = buffer_get_float16(buf, 10.0f, &off);
        d->watt_hours = buffer_get_float16(buf, 1.0f, &off);
        d->watt_hours_charged = buffer_get_float16(buf, 1.0f, &off);
        d->battery_percent_remaining = (float) buf[off++] / 2.0f;
    }

    if (mode >= 4) {
        NEED(2 + 2);
        d->charging.current = buffer_get_float16(buf, 10.0f, &off);
        d->charging.voltage = buffer_get_float16(buf, 10.0f, &off);
    }
    d->fault_code = FAULT_CODE_NONE;
}

static void cmd_recv_light_info(Data *d, const uint8_t *buf, size_t len) {
    if (!buf || len < 3) {
        return;
    }
    if (buf[0] != 101 || buf[1] != COMMAND_LCM_LIGHT_INFO) {
        return;
    }

    int32_t off = 2;

    // Fault?
    NEED(1);
    uint8_t enabled = buf[off++];
    d->lcm_enabled = enabled;
    if(!enabled) return;
    NEED(1 + 1 + 1);
    float brightness = buf[off++]/100.0;
    float idle_brightness = buf[off++]/100.0;
    float status_brightness = buf[off++]/100.0;

    if (!d->light_control_sync)
    {
        if(d->leds.headlights_on && d->last_control_sync_packet_was_on && brightness != d->leds.cfg->headlights.brightness && idle_brightness != d->leds.cfg->front.brightness && status_brightness != d->leds.cfg->status.brightness_headlights_on) {
            return;
        }
        if(!d->leds.headlights_on && !d->last_control_sync_packet_was_on && brightness != d->leds.cfg->front.brightness && idle_brightness != d->leds.cfg->front.brightness && status_brightness != d->leds.cfg->status.brightness_headlights_off) {
            return;
        }
        d->light_control_sync = true;
    }

    if(brightness == d->prev_brightness && idle_brightness == d->prev_idle_brightness && status_brightness == d->prev_status_brightness)//hopefully shouldn't clobber
    {
        return;
    }
    d->prev_brightness=brightness;
    d->prev_idle_brightness=idle_brightness;
    d->prev_status_brightness=status_brightness;

    //check headlights on/off here too
    if(d->leds.headlights_on)
    {
        d->leds.cfg->status.brightness_headlights_on = status_brightness;
    } 
    else 
    {
        d->leds.cfg->status.brightness_headlights_off = status_brightness;
    }
    d->leds.cfg->headlights.brightness = brightness;
    d->leds.cfg->taillights.brightness = brightness;
    d->leds.cfg->front.brightness = idle_brightness;
    d->leds.cfg->rear.brightness = idle_brightness;

}

// Handler for incoming app commands
static void on_command_received(unsigned char *buffer, unsigned int len) {
    Data *d = (Data *) ARG;
    uint8_t magicnr = buffer[0];
    uint8_t command = buffer[1];

    if (len < 2) {
        VESC_IF->printf("Received response packet too short.");
        return;
    }
    if (magicnr != FLOAT_MAGIC && magicnr != FLOAT_ACCESSORIES_MAGIC) {
        VESC_IF->printf("Invalid Package ID: %u", magicnr);
        return;
    }

    switch (magicnr) {
    case FLOAT_MAGIC:
        switch (command) {
        case COMMAND_GET_ALLDATA: {
            cmd_recv_all_data(d, buffer, len);
            return;
        }
        case COMMAND_HUMIDITY: {
            d->humidity_esc_supported = true;
            return;
        }
        case COMMAND_LCM_LIGHT_INFO: {
            cmd_recv_light_info(d, buffer, len);
            return;
        }
        default: {
            VESC_IF->printf("Unknown command received: %u", command);
        }
        }
        break;
    case FLOAT_ACCESSORIES_MAGIC:
        break;
    default:
    }
}

static float app_get_debug(int index) {
    Data *d = (Data *) ARG;

    switch (index) {
    case (1):
        write_cfg_to_eeprom(d);
        return 1.0;
    default:
        return 0;
    }
}

// Register get_debug as a lisp extension
static lbm_value ext_dbg(lbm_value *args, lbm_uint argn) {
    if (argn != 1 || !VESC_IF->lbm_is_number(args[0])) {
        return VESC_IF->lbm_enc_sym_eerror;
    }

    return VESC_IF->lbm_enc_float(app_get_debug(VESC_IF->lbm_dec_as_i32(args[0])));
}

// Register get_debug as a lisp extension
static lbm_value ext_can_id(lbm_value *args, lbm_uint argn) {
    Data *d = (Data *) ARG;
    if (argn == 1 && VESC_IF->lbm_is_number(args[0])) {
        d->float_accessories_conf.can_id=VESC_IF->lbm_dec_as_i32(args[0]); //save can-id
        write_cfg_to_eeprom(d);
        //save
        return VESC_IF->lbm_enc_sym_true;
    }
    return VESC_IF->lbm_enc_i32(d->float_accessories_conf.can_id);
}

static void data_init(Data *d) {
    memset(d, 0, sizeof(Data));
    read_cfg_from_eeprom(d);

    d->fault_code = 0;
    d->amp_hours = 0.0f;
    d->amp_hours_charged = 0.0f;
    d->watt_hours = 0.0f;
    d->watt_hours_charged = 0.0f;
    d->setpoint = 0;
    d->beep_reason = 0;
    d->battery_percent_remaining = 0.0f;
    d->distance_abs = 0.0f;
    d->balance_current = 0.0f;
    d->odometer = 0;

    d->can_id = -1;
    d->can_id_bms = -1;

    d->humidity_esc_supported = false;
    d->cell_num = 0;
    d->light_control_sync = false;
    d->last_control_sync_packet_was_on = false;
    d->lcm_enabled = false;

    d->prev_brightness = 0.0;
    d->prev_idle_brightness = 0.0;
    d->prev_status_brightness = 0.0;

    // note, we also need to keep State and Footpad stuff upto date while polling can.
    // TODO uncomment when done testing
    state_init(&d->state);
    charging_init(&d->charging);
    remote_init(&d->remote);
    leds_init(&d->leds);
    imu_init(&d->imu);
    motor_data_reset(&d->motor);
    atr_reset(&d->atr);
    brake_tilt_reset(&d->brake_tilt);
    torque_tilt_reset(&d->torque_tilt);
    turn_tilt_reset(&d->turn_tilt);
    booster_reset(&d->booster);
}
// Called from Lisp on init to pass in the version info of the firmware
static lbm_value ext_set_fw_version(lbm_value *args, lbm_uint argn) {
    Data *d = (Data *) ARG;
    if (argn > 2) {
        d->fw_version_major = VESC_IF->lbm_dec_as_i32(args[0]);
        d->fw_version_minor = VESC_IF->lbm_dec_as_i32(args[1]);
        d->fw_version_beta = VESC_IF->lbm_dec_as_i32(args[2]);
    }
    return VESC_IF->lbm_enc_sym_true;
}

static lbm_value ext_set_can_ids(lbm_value *args, lbm_uint argn) {
    Data *d = (Data *) ARG;
    if (argn > 1) {
        d->can_id = VESC_IF->lbm_dec_as_i32(args[0]);
        d->can_id_bms = VESC_IF->lbm_dec_as_i32(args[1]);
    }
    return VESC_IF->lbm_enc_sym_true;
}

static lbm_value ext_set_bms_info(lbm_value *args, lbm_uint argn) {
    Data *d = (Data *) ARG;
    if (argn > 0) {
        d->cell_num = VESC_IF->lbm_dec_as_i32(args[0]);
    }
    return VESC_IF->lbm_enc_sym_true;
}

INIT_FUN(lib_info *info) {
    INIT_START
    VESC_IF->printf("Initializing " PACKAGE_NAME " " PACKAGE_VERSION " (%x)", GIT_HASH);

    Data *d = VESC_IF->malloc(sizeof(Data));
    if (!d) {
        VESC_IF->printf("Out of memory, startup failed.");
        return false;
    }
    data_init(d);

    info->stop_fun = stop;
    info->arg = d;

    d->main_thread = VESC_IF->spawn(main_thd, 1536, "Float Accessories Main", d);
    if (!d->main_thread) {
        VESC_IF->printf("Failed to spawn Float Accessories Main thread.");
        return false;
    }

    d->aux_thread = VESC_IF->spawn(aux_thd, 2048 * 2, "Float Accessories Aux", d);
    if (!d->aux_thread) {
        VESC_IF->printf("Failed to spawn Float Accessories Auxiliary thread.");
        VESC_IF->request_terminate(d->main_thread);
        return false;
    }

    // TODO: Threads for pubmote and BMS. Might do humidity in lisp though since it already has
    // simple i2c... Probably keep logger in lisp too, will need to grab some params
    // footpad_sensor_update(&d->footpad, &d->float_conf); hmmm, maybe we wanna wait for footpad to
    // be populated by on_command_recieve so we can not setup leds if both are pressed.
    leds_setup(
        &d->leds,
        &d->float_accessories_conf.hardware.leds,
        &d->float_accessories_conf.leds,
        d->footpad.state
    );
    VESC_IF->conf_custom_add_config(get_cfg, set_cfg, get_cfg_xml);
    VESC_IF->lbm_add_extension("ext-dbg", ext_dbg);
    VESC_IF->lbm_add_extension("ext-can-id", ext_can_id);
    VESC_IF->lbm_add_extension("ext-set-fw-version", ext_set_fw_version);
    VESC_IF->lbm_add_extension("ext-set-can-ids", ext_set_can_ids);
    VESC_IF->lbm_add_extension("ext-set-bms-info", ext_set_bms_info);
    VESC_IF->lbm_add_extension("ext-update-data", ext_update_data);

    VESC_IF->set_app_data_handler(on_command_received);
    return true;
}

// void fatal_error_terminate()
//{
//     stop(ARG);
// }
