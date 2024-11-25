;@const-symbol-strings
@const-start
(def can-loop-delay)
(def fault-code 0)
(def pitch-angle 0)
(def roll-angle 0)
(def state 0)
(def switch-state 0)
(def handtest-mode nil)
(def rpm 0)
(def speed 0)
(def tot-current 0)
(def duty-cycle-now 0)
(def distance-abs -1)
(def fet-temp-filtered 0)
(def motor-temp-filtered 0)
(def odometer -1)
(def odometer-init -1)
(def battery-percent-remaining 0.0)
(def can-id -1)
(def bms-can-id -1)
(def bms-is-charging nil)
(def bms-charger-just-plugged nil)
(def bms-charger-plug-in-time 0)
(def bms-check-voltage-timer 0)
(def vin -1)
(def vin-prev -1)
(def vin-sample -1)
(def vin-chatter 0)
(def last-running-state-time 0)
(def refloat-battery-percent nil)

(def FLOAT_MAGIC 101)
(def FLOAT_ACCESSORIES_MAGIC 102)

(def float-cmds '(
    (COMMAND_GET_INFO . 0)
    (COMMAND_GET_ALLDATA . 10)
    (COMMAND_LCM_GET_BATTERY . 29)
))

(def float-accessories-cmds '(
    (COMMAND_GET_INFO . 0)
    (COMMAND_RUN_LISP . 1)
    (COMMAND_BMS_STATUS . 2)
))

(def discover-can-id -1)

(defun running-state (){
    (let ret (and (>= state 1) (<= state 5)))
})

(defun can-loop (){
    (setq can-loop-delay (get-config 'can-loop-delay))
    (var next-run-time (secs-since 0))
    (var loop-start-time 0)
    (var loop-end-time 0)
    (var can-loop-delay-sec (/ 1.0 can-loop-delay))
    (init-can)
    (loopwhile t {
        (setq loop-start-time  (secs-since 0))
        (float-cmd can-id (list (assoc float-cmds 'COMMAND_LCM_GET_BATTERY)))
        (float-cmd can-id (list (assoc float-cmds 'COMMAND_GET_ALLDATA) 3))

        (if (>= bms-can-id 0){
            (var prev-charging-state bms-is-charging)
            (setq bms-is-charging (and (> (get-bms-val 'bms-v-charge) 10.0) (< (get-bms-val 'bms-i-in-ic) 0.1)))

            (if (and bms-is-charging (not prev-charging-state)){
                    (setq bms-charger-just-plugged t)
                    (setq bms-charger-plug-in-time (secs-since 0))
            })
            ; Check if we're within 5 seconds of initial plug-in and charging started
            (if (not (and bms-charger-just-plugged (<= (- (secs-since 0) bms-charger-plug-in-time) 5))){
                ; Reset the flag if more than 5 seconds have passed
                (setq bms-charger-just-plugged nil)
            })
        }{;if we don't have a smart BMS, we can guage if it's charging by sampling voltage over time when the board is not running.
            (if (running-state){
                (setq bms-is-charging nil)
            }{
                (if (!= vin vin-prev){
                    (setq vin-chatter (+ vin-chatter 1))
                })
                (if (>= (- (secs-since 0) bms-check-voltage-timer) 30){
                    (if (and (>= (- vin vin-sample) 0.0) (>= vin-chatter (get-config 'vin-chatter-threshold)) (< battery-percent-remaining 99.0)){
                        (setq bms-is-charging t)
                    }{
                        (setq bms-is-charging nil)
                    })
                    (setq vin-chatter 0)
                    (setq bms-check-voltage-timer (secs-since 0))
                    (setq vin-sample vin)
                })
                (setq vin-prev vin)
            })
        })

        (setq loop-end-time (secs-since 0))
        (var actual-loop-time (- loop-end-time loop-start-time))
        (var time-to-wait (- next-run-time (secs-since 0)))
        (if (> time-to-wait 0) {
            (yield (* time-to-wait 1000000))
        } {
            (setq next-run-time (secs-since 0))
        })
        (setq next-run-time (+ next-run-time can-loop-delay-sec))
    })
})

(defunret init-can () {
    (var can-devices '())
    (var original-can-id (get-config 'can-id ))
    (set-config 'can-id -1)
    (var init-time (systime))
    (loopwhile (<= (secs-since init-time) 10) {
        (if (and (>= original-can-id 0) (<= (secs-since init-time) 5)){
            (setq can-devices (list original-can-id))
        }{
            (setq can-devices (can-scan))
        })
        (loopforeach can-id can-devices {
            (setq discover-can-id can-id)
            (float-cmd can-id (list (assoc float-cmds 'COMMAND_GET_ALLDATA) 3))
            (yield 500000)
            (if (>= (get-config 'can-id ) 0) {
                (if (not-eq (get-config 'can-id ) original-can-id) {
                    (write-val-eeprom 'can-id (get-config 'can-id ))
                    (write-val-eeprom 'crc (config-crc))
                })
                (return 1)
            })
        })
    })
    (return 0)
})

(defun float-cmd (can-id cmd) {
    (send-data (append (list FLOAT_MAGIC) cmd) 2 can-id)
})

(defun float-command-rx (data) {
    ;(print-hex data)
    ;Support for saving config/code exec from qml
    (if (and (> (buflen data) 1) (= (bufget-u8 data 0) FLOAT_ACCESSORIES_MAGIC)) {
        (match (cossa float-accessories-cmds (bufget-u8 data 1))
            ;(COMMAND_GET_INFO {
            ;})
            (COMMAND_RUN_LISP {
                (bufcpy data 0 data 2 (-(buflen data) 2))
                (buf-resize data -2)
                (eval (read data))
            })
            (COMMAND_BMS_STATUS {
                (var send-buffer (bufcreate 3))
                (bufset-u8 send-buffer 0 FLOAT_ACCESSORIES_MAGIC)
                (bufset-u8 send-buffer 1 (assoc 'COMMAND_BMS_STATUS float-accessories-cmds))
                (bufset-u8 send-buffer 2 bms-status)
                (send-data send-buffer 2 can-id)
                (free send-buffer)
            })
            (_ nil) ; Ignore other commands
        )
    })

    ; Only process data if data is long enough and magic number is correct
    (if (and (> (buflen data) 1) (= (bufget-u8 data 0) FLOAT_MAGIC)) {
        (setq can-last-activity-time (systime))
        (match (cossa float-cmds (bufget-u8 data 1))
                (COMMAND_LCM_GET_BATTERY {
                    (if  (> (buflen data) 2){
                        (setq battery-percent-remaining (bufget-f32 data 2))
                        (setq refloat-battery-percent t)
                    })
                })
                (COMMAND_GET_ALLDATA {
                (if (< can-id 0){
                    (set-config 'can-id discover-can-id)
                    (setq can-id discover-can-id)
                    (setq bms-can-id (get-bms-val 'bms-can-id))
                })
                    (if  (> (buflen data) 3){
                        (var mode (bufget-u8 data 2))

                        (if (= mode 69) {
                            (setq fault-code (bufget-u8 data 3))
                        }{
                            (setq fault-code 0)
                            (if (>= (buflen data) 32) {
                                (def roll-angle (/ (to-float (bufget-i16 data 7)) 10))
                                (var state-byte (bufget-u8 data 9))
                                (setq state (bitwise-and state-byte 0x0F))
                                (var switch-state-byte (bufget-u8 data 10))
                                (var switch-state-temp (bitwise-and switch-state-byte 0x0F))
                                (setq handtest-mode (= (bitwise-and switch-state-byte 0x08) 0x08))
                                (var footpad-adc1-t (bufget-u8 data 11))
                                (var footpad-adc2-t (bufget-u8 data 12))
                                (cond
                                    ((= switch-state-temp 0) {
                                        (setq switch-state 0)
                                    })
                                    ((= switch-state-temp 1) {
                                        (if (> footpad-adc1-t footpad-adc2-t){
                                            (setq switch-state 1)
                                        }{
                                            (setq switch-state 2)
                                        })
                                    })
                                    ((= switch-state-temp 2) {
                                        (setq switch-state 3)
                                    })
                                )
                                (setq pitch-angle (/ (to-float (bufget-i16 data 19)) 10))
                                (setq vin (/ (to-float (bufget-i16 data 22)) 10))
                                (if (= vin-prev -1){ (setq vin-prev vin) (setq vin-sample vin) })
                                (setq rpm (/ (to-float  (bufget-i16 data 24)) 10))
                                (setq speed (/ (to-float (bufget-i16 data 26)) 10))
                                (setq tot-current (/ (to-float (bufget-i16 data 28)) 10))
                                (setq duty-cycle-now (/ (to-float (- (bufget-u8 data 32) 128)) 100))
                                (if (and (>= mode 2) (>= (buflen data) 39)) {
                                    ;(def distance-abs-t (bufget-f32-auto data 34))
                                    (setq fet-temp-filtered (/ (bufget-u8 data 38) 2.0))
                                    (setq motor-temp-filtered (/ (bufget-u8 data 39) 2.0))
                                })
                                (if (and (>= mode 3) (>= (buflen data) 52)) {
                                    (setq odometer (bufget-u32 data 41))
                                    (if (= odometer-init -1) (setq odometer-init odometer))
                                    (setq distance-abs (- odometer odometer-init))
                                    (if (not refloat-battery-percent) (setq battery-percent-remaining (/ (to-float (bufget-u8 data 53)) 2)))
                                })
                            })
                        })
                    })
                })
                (_ nil)
            )
    })
})
@const-end