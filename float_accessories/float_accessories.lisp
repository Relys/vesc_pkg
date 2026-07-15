; float-accessories.lisp
; Smart LED Control, Tilt Remote and stock OW BMS bridge for VESC Express
; Version 1.1
; 4/7/2024
; Copyright 2024 Syler Clayton <syler.clayton@gmail.com>
; Special Thanks: Benjamin Vedder, surfdado, NuRxG, Siwoz, lolwheel (OWIE), ThankTheMaker (rESCue), 4_fools & marcos (avaspark), auden_builds (pubmote)
; gr33tz: outlandnish, exphat, datboig42069
; Beta Testers: Pickles
@const-start
(import "lib/utils.lisp" 'utils)
(read-eval-program utils)
(import "lib/settings-vars.lisp" 'settings-vars)
(read-eval-program settings-vars)
(import "lib/settings.lisp" 'settings)
(read-eval-program settings)
(import "lib/can.lisp" 'can)
(read-eval-program can)
(import "lib/logger.lisp" 'logger)
(read-eval-program logger)
(import "lib/led-vars.lisp" 'led-vars)
(read-eval-program led-vars)
(import "lib/led.lisp" 'led)
(read-eval-program led)
(import "lib/led_patterns.lisp" 'led-patterns)
(read-eval-program led-patterns)
(import "lib/bms-vars.lisp" 'bms-vars)
(read-eval-program bms-vars)
(import "lib/bms.lisp" 'bms)
(read-eval-program bms)
(import "lib/pubmote-consts.lisp" 'pubmote-consts)
(read-eval-program pubmote-consts)
(import "lib/pubmote-vars.lisp" 'pubmote-vars)
(read-eval-program pubmote-vars)
(import "lib/pubmote-utils.lisp" 'pubmote-utils)
(read-eval-program pubmote-utils)
(import "lib/pubmote.lisp" 'pubmote)
(read-eval-program pubmote)
(import "lib/commands.lisp" 'commands)
(read-eval-program commands)

(defun main () {
    (setup)
    (init)
    (print (str-merge "Boot complete in " (str-from-n (/ (systime) 1000000.0) "%.3f") "s since power-on"))
})
(defun spawn-with-restart (name stack-size func) {
    (var monitor-fn (fn () 
        (loopwhile t {
            (if stack-size
                (spawn-trap stack-size func)
                (spawn-trap func))
            (recv   ((exit-error (? tid) (? e))
                        (print (str-merge name " error: " (to-str e)))
                    )
                    ((exit-ok (? tid) (? v)) 'ok))
            (sleep 1.0)
        })
    ))
    (if stack-size
        (spawn stack-size monitor-fn)
        (spawn monitor-fn))
})

; The event handler must be registered with the worker thread's id each time it
; is (re)spawned: events are delivered to the registered thread's mailbox, and
; a recv only consumes matching messages. Registering the monitor thread (whose
; recv only matches exit messages) would silently swallow every event.
(defun spawn-event-handler-with-restart () {
    (spawn (fn ()
        (loopwhile t {
            (event-register-handler (spawn-trap event-handler))
            (recv   ((exit-error (? tid) (? e))
                        (print (str-merge "event-handler error: " (to-str e)))
                    )
                    ((exit-ok (? tid) (? v)) 'ok))
            (sleep 1.0)
        })
    ))
})

(defun setup () {
    (var fw-num (+ (first (sysinfo 'fw-ver)) (* (second (sysinfo 'fw-ver)) 0.01)))
    (spawn-event-handler-with-restart)
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
        (setq led-context-id (spawn-with-restart "led-loop" nil led-loop))
    }); start the led loop as soon as possible once checks are done. once CAN bus comes online it will start responding, and since this is multi-process now leds won't freeze when can is scanning. :)
    (setq can-context-id (spawn-with-restart "can-loop" nil can-loop))
    (if (= (get-config 'pubmote-enabled) 1){
        (setup-pubmote
            VEHICLE_TYPE_ONEWHEEL
            (fn (jsy jsx bt-c bt-z is-rev) {
                (setq pubmote-last-jsy jsy)
                (setq pubmote-last-jsx jsx)
                (setq pubmote-last-bt-c bt-c)
                (setq pubmote-last-bt-z bt-z)
                (setq pubmote-last-is-rev is-rev)
                (if (>= (get-config 'can-id) 0) {
                    (can-cmd (get-config 'can-id) (str-replace (to-str (list jsy jsx bt-c bt-z is-rev)) "(" "(set-remote-state "))
                })
            })
            (fn () {
                (list fault-code pitch-angle roll-angle state switch-state vin rpm speed tot-current duty-cycle-now distance-abs fet-temp-filtered motor-temp-filtered odometer battery-percent-remaining)
            })
            (fn (text) {
                (send-msg text)
            })
            (fn (name) {
                (get-config name)
            })
            (fn (name val) {
                (set-config name val)
            })
            (fn () {
                (atomic {
                    (write-val-eeprom 'pubmote-remote-mac-a (get-config 'pubmote-remote-mac-a))
                    (write-val-eeprom 'pubmote-remote-mac-b (get-config 'pubmote-remote-mac-b))
                    (write-val-eeprom 'pubmote-secret-code (get-config 'pubmote-secret-code))
                    (write-val-eeprom 'crc (config-crc cfg-len))
                })
            })
        )
        (setq pubmote-context-id (spawn-with-restart "pubmote-loop" nil pubmote-loop))
    })
    (if (= (get-config 'bms-enabled) 1){
        (setq bms-context-id (spawn-with-restart "bms-loop" nil bms-loop))
    })

    (if (= (get-config 'humidity-enabled) 1) (setq humidity-context-id (spawn-with-restart "humidity-loop" nil humidity-loop)))

    (if (= (get-config 'log-enabled) 1) (setq log-context-id (spawn-with-restart "log-loop" 50 log-loop)))
})

; Save the environment as a binary image for fast boot on subsequent power-cycles.
; On the very next boot the reader is skipped and main() is called directly.
(if (is-606-or-newer) {
    (image-save)
})
; Start immediately on this (first) boot too.
(main)
@const-end