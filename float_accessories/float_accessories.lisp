; float-accessories.lisp
; Smart LED Control, Tilt Remote and stock OW BMS bridge for VESC Express
; Version 3.5.18
; 5/22/2025
; Copyright 2024 Syler Clayton <syler.clayton@gmail.com>
; Special Thanks: Benjamin Vedder, surfdado, NuRxG, Siwoz, lolwheel (OWIE), ThankTheMaker (rESCue), 4_fools & marcos (avaspark), auden_builds (pubmote)
; Contributors: shambler01
; gr33tz: outlandnish, exphat, datboig42069
; Beta Testers: Pickles
@const-start
(import "lib/utils.lisp" 'utils)
(read-eval-program utils)
(import "lib/settings.lisp" 'settings)
(read-eval-program settings)
(import "lib/can.lisp" 'can)
(read-eval-program can)
(import "lib/logger.lisp" 'logger)
(read-eval-program logger)
(import "lib/led.lisp" 'led)
(read-eval-program led)
(import "lib/led_patterns.lisp" 'led-patterns)
(read-eval-program led-patterns)
(import "lib/bms.lisp" 'bms)
(read-eval-program bms)
(import "lib/pubmote.lisp" 'pubmote)
(read-eval-program pubmote)

(defun main () {
    (setup)
    (init)
    (print (str-merge "Boot complete in " (str-from-n (/ (systime) 1000000.0) "%.3f") "s since power-on"))
})
(defun setup () {
    (var fw-num (+ (first (sysinfo 'fw-ver)) (* (second (sysinfo 'fw-ver)) 0.01)))
    (event-register-handler (spawn event-handler))
    (event-enable 'event-data-rx)
    (event-enable 'event-esp-now-rx)
    (if (!= (str-cmp (to-str (sysinfo 'hw-type)) "hw-express") 0) {
        (exit-error "Not running on hw-express")
    })

    (if (< fw-num 6.05) (exit-error "hw-express needs to be running 6.05"))

    ; Restore settings if magic header does not match
    ; as that probably means something else is in eeprom
    (if (not-eq (read-val-eeprom 'magic) magic-header) (restore-config) (load-config))
    (var crc (config-crc read-cfg-len))
    (if (!= crc (to-i (read-val-eeprom 'crc)) ) {
        (send-msg  (str-merge "Error: crc corrupt. Got " (str-from-n (read-val-eeprom 'crc)) ". Expected " (str-from-n crc)))
        (restore-config)
    } {
        (if (> cfg-len read-cfg-len) {
            ;check if crcs match and update default params only for new ones. Make sure they get updated in eeprom, and active variables and then save the crc
            ; Initialize only the new parameters (those beyond read-cfg-len)
            (var count 0)
            (loopforeach setting eeprom-addrs {
                (if (and (>= count read-cfg-len) (< count cfg-len)) {
                    (var name (first setting))
                    (var default-value (ix setting 3))
                    ;(write-val-eeprom name default-value)
                    (set-config name default-value)
                })
                (setq count (+ count 1))
            })
            (save-config)
        })
    })
})

(defun init () {
    ; Spawn the event handler thread and pass the ID it returns to C
    (if (= (get-config 'led-enabled) 1) {
        (setq led-context-id (spawn led-loop))
    }); start the led loop as soon as possible once checks are done. once CAN bus comes online it will start responding, and since this is multi-process now leds won't freeze when can is scanning. :)
    (setq can-context-id (spawn can-loop))
    (if (> (conf-get 'wifi-mode) 0) {
        (setq wifi-enabled-on-boot t)
        (if (= (get-config 'pubmote-enabled) 1){
            (setq pubmote-context-id (spawn pubmote-loop))
        })
    })
    (if (= (get-config 'bms-enabled) 1){
        (setq bms-context-id (spawn bms-loop))
    })

    (if (= (get-config 'humidity-enabled) 1) (setq humidity-context-id (spawn humidity-loop)))

    (if (= (get-config 'log-enabled) 1) (setq log-context-id (spawn 50 log-loop)))
})

; Save the environment as a binary image for fast boot on subsequent power-cycles.
; On the very next boot the reader is skipped and main() is called directly.
(if (is-606-or-newer) {
    (image-save)
})
; Start immediately on this (first) boot too.
(main)
@const-end