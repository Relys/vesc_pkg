; float-accessories.lisp
; Smart LED Control, Tilt Remote and stock OW BMS bridge for VESC Express
; Version 1.1
; 4/7/2024
; Copyright 2024 Syler Clayton <syler.clayton@gmail.com>
; Special Thanks: Benjamin Vedder, surfdado, NuRxG, Siwoz, lolwheel (OWIE), ThankTheMaker (rESCue), 4_fools & marcos (avaspark), auden_builds (pubmote)
; gr33tz: outlandnish, exphat, datboig42069
; Beta Testers: Koddex, Pickles

(import "lib/led.lisp" 'led)
(read-eval-program led)
(import "lib/led_patterns.lisp" 'led-patterns)
(read-eval-program led-patterns)
(import "lib/settings.lisp" 'settings)
(read-eval-program settings)
(import "lib/utils.lisp" 'utils)
(read-eval-program utils)
(import "lib/can.lisp" 'can)
(read-eval-program can)
(import "lib/bms.lisp" 'bms)
(read-eval-program bms)
(import "lib/pubmote.lisp" 'pubmote)
(read-eval-program pubmote)
(defun main () {
    (setup)
    (init)
})

(defun setup () {
    (event-register-handler (spawn event-handler))
    (event-enable 'event-data-rx)
    (event-enable 'event-esp-now-rx)
    (if (!= (str-cmp (to-str (sysinfo 'hw-type)) "hw-express") 0) {
        (exit-error "Not running on hw-express")
    })
    (var fw-num (+ (first (sysinfo 'fw-ver)) (* (second (sysinfo 'fw-ver)) 0.01)))
    (if (< fw-num 6.05) (exit-error "hw-express needs to be running 6.05"))

    ; Restore settings if version number does not match
    ; as that probably means something else is in eeprom
    (if (not-eq (read-val-eeprom 'ver-code) config-version) (restore-config) (load-config))
    (var crc (config-crc))
    (if (!= crc (to-i (read-val-eeprom 'crc)) ){ (send-msg  (str-merge "Error: crc corrupt. Got " (str-from-n (read-val-eeprom 'crc)) ". Expected " (str-from-n crc))) (restore-config) })
})

(defun init (){
    ; Spawn the event handler thread and pass the ID it returns to C
    (if (= (get-config 'led-enabled) 1) (setq led-context-id (spawn led-loop))); start the led loop as soon as possible once checks are done. once CAN bus comes online it will start responding, and since this is multi-process now leds won't freeze when can is scanning. :)
    (setq can-context-id (spawn can-loop))
    (if (> (conf-get 'wifi-mode) 0) {
        (setq wifi-enabled-on-boot t)
        (if (= (get-config 'pubmote-enabled) 1) (setq pubmote-context-id (spawn pubmote-loop)))
    })
    (if (= (get-config 'bms-enabled) 1) (setq bms-context-id (spawn bms-loop)))
})
; Start the main
(main)