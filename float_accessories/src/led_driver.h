#ifndef LED_DRIVER_H
#define LED_DRIVER_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include "conf/datatypes.h"
#include "led_strip.h"

#define STRIP_COUNT 3   // status, front, rear
typedef struct {
    uint8_t *bitbuffer;
    uint32_t bitbuffer_length; 
    uint16_t flicker;
    bool single_strip;
    
    const LedStrip *strips[STRIP_COUNT];      // Strip pointers
    uint8_t *strip_bitbuffs[STRIP_COUNT];    // Per-strip buffer pointers
    
    // Configuration
    CfgLedStrip configs[STRIP_COUNT];         // Strip configurations
} LedDriver;

// Public API
void led_driver_init(LedDriver *driver);
bool led_driver_setup(LedDriver *driver, CfgHwLeds *hw_cfg, const LedStrip **strips);
void led_driver_paint(LedDriver *driver, bool headlights_on, bool highbeams_on, bool forward);
void led_driver_destroy(LedDriver *driver);

#endif // LED_DRIVER_H