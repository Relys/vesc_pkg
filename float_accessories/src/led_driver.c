// Copyright 2022 Benjamin Vedder <benjamin@vedder.se>
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

#include "led_driver.h"
#include "utils.h"
#include "vesc_c_if.h"

#include <string.h>

static inline uint8_t cgamma(uint8_t c) {
    return (c * c + c) / 256;   // same gamma you had
}

static inline uint8_t color_order_channels(LedColorOrder order) {
    switch (order) {
    case LED_COLOR_GRBW:
    case LED_COLOR_WRGB:
    case LED_COLOR_RGBW:
        return 4;
    case LED_COLOR_GRB:
    case LED_COLOR_RGB:
        return 3;
    }
    return 3; // silence warnings
}

void led_driver_init(LedDriver *driver) {
    driver->bitbuffer_length = 0;
    driver->bitbuffer = NULL;
    driver->single_strip = true;
    driver->flicker = 200;
    for (size_t i = 0; i < STRIP_COUNT; ++i) {
        driver->strips[i] = NULL;
        driver->strip_bitbuffs[i] = NULL;
    }
}

bool led_driver_setup(LedDriver *driver, CfgHwLeds *hw_config, const LedStrip **led_strips) {
    driver->bitbuffer_length = 0;

    size_t offsets[STRIP_COUNT] = {0};
    size_t total_bytes = 0;

    driver->configs[0].pin=hw_config->status.pin;
    driver->configs[0].strip_type = hw_config->status.strip_type;
    driver->configs[1].pin=hw_config->front.pin;
    driver->configs[1].strip_type = hw_config->front.strip_type;
    driver->configs[2].pin=hw_config->rear.pin;
    driver->configs[2].strip_type = hw_config->rear.strip_type;

    driver->flicker = hw_config->flicker*100;

    int first_pin = driver->configs[0].pin;
    for (size_t i = 1; i < STRIP_COUNT; ++i) {
        if (driver->configs[i].pin != first_pin) {
            driver->single_strip = false;
            break;
        }
    }
    if(driver->configs[0].strip_type != STRIP_NONE)
    {
        VESC_IF->rgbled_init(driver->configs[0].pin);
    }

    for (size_t i = 0; i < STRIP_COUNT; ++i) {
        const LedStrip *strip = led_strips[i];
        driver->strips[i] = strip;
        if (!strip) { driver->strip_bitbuffs[i] = NULL; continue; }

        const uint8_t ch = color_order_channels(strip->color_order);
        offsets[i] = total_bytes;
        size_t strip_highbeam_bytes = 0;
        switch(driver->configs[i].strip_type) {
            case  STRIP_LASERBEAMS:
            case  STRIP_LASERBEAMS_PINT:
            case  STRIP_LASERBEAMS_V3:
            case  STRIP_LASERBEAMS_PINT_V3:
            case  STRIP_FLASHFIRES:
                strip_highbeam_bytes = 1; // 1 LED for highbeam control
                break;
            case  STRIP_JETFLEET_H4:
            case  STRIP_JETFLEET_H4_NO_LIMIT:
            case  STRIP_JETFLEET_GT:
            case  STRIP_GTFO:
                strip_highbeam_bytes = 4;
                break;
            default:
                break;
        }
        total_bytes += (size_t)(strip->length+strip_highbeam_bytes) * ch; // TODO Ok, here's where we add bytes for Highbeam control
    }

    driver->bitbuffer_length = total_bytes; // in bytes now
    driver->bitbuffer = (uint8_t*)VESC_IF->malloc(total_bytes ? total_bytes : 1);
    if (!driver->bitbuffer && total_bytes) {
        return false;
    }
    if (driver->bitbuffer && total_bytes) {
        memset(driver->bitbuffer, 0, total_bytes);
    }

    for (size_t i = 0; i < STRIP_COUNT; ++i) {
        if (driver->strips[i]) {
            driver->strip_bitbuffs[i] = driver->bitbuffer + offsets[i];
        }
    }
    return true;
}

void led_driver_paint(LedDriver *driver, bool headlights_on, bool highbeams_on, bool forward, float highbeams_brightness, float highbeams_dim_ratio) {
    if (!driver->bitbuffer) return;
    
    for (size_t i = 0; i < STRIP_COUNT; ++i) {
        const LedStrip *strip = driver->strips[i];
        if (!strip) continue;

        uint8_t *out = driver->strip_bitbuffs[i];
        const uint8_t ch = color_order_channels(strip->color_order);

        size_t highbeam_leds=0;
        switch(driver->configs[i].strip_type){
            case  STRIP_LASERBEAMS:
            case  STRIP_LASERBEAMS_PINT:
            case  STRIP_LASERBEAMS_V3:
            case  STRIP_LASERBEAMS_PINT_V3:
            case  STRIP_FLASHFIRES:
                highbeam_leds=1; // 1 LED for highbeam control
                break;
            case  STRIP_JETFLEET_H4:
            case  STRIP_JETFLEET_H4_NO_LIMIT:
            case  STRIP_JETFLEET_GT:
            case  STRIP_GTFO:
                highbeam_leds=4;
                break;
            default:
                break;
        }
        int k=0;
        for (uint32_t j = 0; j < strip->length+highbeam_leds; ++j) {//We need to check if we have type of highbeam, and add the correct bytes here. Make sure we allocate the extra bytes needed in the led_driver_setup for the bitbuffer. The strips remain untouched.
            uint32_t color = 0x00000000;
            uint8_t w = 0;
            uint8_t r = 0;
            uint8_t g = 0;
            uint8_t b = 0;
            switch (driver->configs[i].strip_type) {
                case  STRIP_NONE:
                    break;
                case  STRIP_LASERBEAMS:
                case  STRIP_LASERBEAMS_PINT:
                case  STRIP_LASERBEAMS_V3:
                case  STRIP_LASERBEAMS_PINT_V3:
                case  STRIP_FLASHFIRES:
                    switch(j) {
                        case 0:
                            if((headlights_on && highbeams_on) && (i==1 && forward || i==2 && !forward))
                            {
                                uint8_t b = scale8(0xFF, highbeams_brightness);                          // scale blue by mapped factor
                                color = (uint32_t)b;
                            } else {
                                color = 0x00000000;
                            }
                            k++;
                            w = (color >> 24) & 0xFF;
                            r = (color >> 16) & 0xFF;
                            g = (color >>  8) & 0xFF;
                            b =  color        & 0xFF;
                            break;
                        default:
                            color = strip->data[j-k];   // 0xWWRRGGBB
                            float dim_ratio = 1.0f;
                            if((headlights_on && highbeams_on) && (i==1 && forward || i==2 && !forward))
                            {
                                dim_ratio = highbeams_dim_ratio;
                            }
                            w = cgamma(scale8((color >> 24) & 0xFF, dim_ratio));
                            r = cgamma(scale8((color >> 16) & 0xFF, dim_ratio));
                            g = cgamma(scale8((color >>  8) & 0xFF, dim_ratio));
                            b = cgamma(scale8( color        & 0xFF, dim_ratio));
                            break;
                    }
                    break;
                case  STRIP_JETFLEET_H4:
                case  STRIP_JETFLEET_H4_NO_LIMIT:
                    switch(j) {
                        case 3:
                        case 8:
                        case 14:
                        case 19:
                            if((headlights_on && highbeams_on) && (i==1 && forward || i==2 && !forward))
                            {
                                float m = 0.0f;
                                if (driver->configs[i].strip_type == STRIP_JETFLEET_H4_NO_LIMIT) {
                                    m = map_range(highbeams_brightness, 0.6f, 1.0f);
                                } else {
                                    m = map_range(highbeams_brightness, 0.6f, 0.8f);
                                }
                                uint8_t b = scale8(0xFF, m);                          // scale blue by mapped factor
                                color = (uint32_t)b;
                            } else {
                                color = 0x00000000;
                            }
                            k++;
                            w = (color >> 24) & 0xFF;
                            r = (color >> 16) & 0xFF;
                            g = (color >>  8) & 0xFF;
                            b =  color        & 0xFF;
                            break;
                        default:
                            color = strip->data[j-k];   // 0xWWRRGGBB
                            float cap_scale = 1.0f;
                            if (driver->configs[i].strip_type == STRIP_JETFLEET_H4) {
                                float cap = 0.8f;
                                cap_scale = (strip->brightness > cap) ? (cap / strip->brightness) : 1.0f;
                            }

                            float dim_ratio = 1.0f;
                            if((headlights_on && highbeams_on) && (i==1 && forward || i==2 && !forward))
                            {
                                dim_ratio = highbeams_dim_ratio;
                            }
                            float s = cap_scale * dim_ratio;   // total linear scale
                            w = cgamma(scale8((color >> 24) & 0xFF, s));
                            r = cgamma(scale8((color >> 16) & 0xFF, s));
                            g = cgamma(scale8((color >>  8) & 0xFF, s));
                            b = cgamma(scale8( color        & 0xFF, s));
                            break;
                    }
                    break;
                case  STRIP_JETFLEET_GT:
                    switch(j) {
                        case 1:
                        case 4:
                        case 10:
                        case 13:
                            if((headlights_on && highbeams_on) && (i==1 && forward || i==2 && !forward))
                            {
                                float m = map_range(highbeams_brightness, 0.6f, 1.0f);
                                uint8_t b = scale8(0xFF, m);                          // scale blue by mapped factor
                                color = (uint32_t)b;
                            } else {
                                color = 0x00000000;
                            }
                            k++;
                            w = (color >> 24) & 0xFF;
                            r = (color >> 16) & 0xFF;
                            g = (color >>  8) & 0xFF;
                            b =  color        & 0xFF;
                            break;
                        default:
                            color = strip->data[j-k];   // 0xWWRRGGBB
                            float dim_ratio = 1.0f;
                            if((headlights_on && highbeams_on) && (i==1 && forward || i==2 && !forward))
                            {
                                dim_ratio = highbeams_dim_ratio;
                            }
                            w = cgamma(scale8((color >> 24) & 0xFF, dim_ratio));
                            r = cgamma(scale8((color >> 16) & 0xFF, dim_ratio));
                            g = cgamma(scale8((color >>  8) & 0xFF, dim_ratio));
                            b = cgamma(scale8( color        & 0xFF, dim_ratio));
                            break;
                    }
                    break;
                case  STRIP_GTFO:
                    switch(j) {
                        case 3:
                        case 6:
                        case 9:
                        case 13:
                            if((headlights_on && highbeams_on) && (i==1 && forward || i==2 && !forward))
                            {
                                float m = map_range(highbeams_brightness, 0.4f, 1.0f);
                                uint8_t b = scale8(0xFF, m);                          // scale blue by mapped factor
                                color = (uint32_t)b;
                            } else {
                                color = 0x00000000;
                            }
                            k++;
                            w = (color >> 24) & 0xFF;
                            r = (color >> 16) & 0xFF;
                            g = (color >>  8) & 0xFF;
                            b =  color        & 0xFF;
                            break;
                        default:
                            color = strip->data[j-k];   // 0xWWRRGGBB
                            float dim_ratio = 1.0f;
                            if((headlights_on && highbeams_on) && (i==1 && forward || i==2 && !forward))
                            {
                                dim_ratio = highbeams_dim_ratio;
                            }
                            w = cgamma(scale8((color >> 24) & 0xFF, dim_ratio));
                            r = cgamma(scale8((color >> 16) & 0xFF, dim_ratio));
                            g = cgamma(scale8((color >>  8) & 0xFF, dim_ratio));
                            b = cgamma(scale8( color        & 0xFF, dim_ratio));
                            break;
                    }
                    break;
                default:
                    color = strip->data[j-k];   // 0xWWRRGGBB
                    w = cgamma((color >> 24) & 0xFF);
                    r = cgamma((color >> 16) & 0xFF);
                    g = cgamma((color >>  8) & 0xFF);
                    b = cgamma( color        & 0xFF);
                    break;
            }

            switch (strip->color_order) {
            case LED_COLOR_GRB:   out[0]=g; out[1]=r; out[2]=b; break;
            case LED_COLOR_RGB:   out[0]=r; out[1]=g; out[2]=b; break;
            case LED_COLOR_GRBW:  out[0]=g; out[1]=r; out[2]=b; out[3]=w; break;
            case LED_COLOR_WRGB:  out[0]=w; out[1]=r; out[2]=g; out[3]=b; break;
            case LED_COLOR_RGBW:  out[0]=r; out[1]=g; out[2]=b; out[3]=w; break;
            }
            out += ch;
        }
        if(!driver->single_strip && driver->configs[i].strip_type != STRIP_NONE) {
            const size_t bytes = (size_t)(strip->length + highbeam_leds) * ch;
            VESC_IF->rgbled_init(driver->configs[i].pin);
            VESC_IF->sleep_us(driver->flicker);
            VESC_IF->rgbled_update(driver->strip_bitbuffs[i], bytes);
        }
    }

    if(driver->single_strip && driver->configs[0].strip_type != STRIP_NONE) {
        VESC_IF->rgbled_update(driver->bitbuffer, driver->bitbuffer_length);
    }
}

void led_driver_destroy(LedDriver *driver) {
    if (driver->bitbuffer) {
        VESC_IF->free(driver->bitbuffer);
        driver->bitbuffer = NULL;
    }
    driver->bitbuffer_length = 0;
    for (size_t i = 0; i < STRIP_COUNT; ++i) {
        driver->strip_bitbuffs[i] = NULL;
        driver->strips[i] = NULL;
    }
}