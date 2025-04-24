;@const-symbol-strings
@const-start
(def can-loop-delay)
(def fault-code 0)
(def pitch-angle 0)
(def roll-angle 0)
(def state 0)
(def sat-t 0)
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
(def can-id -1)
(def bms-can-id -1)
(def bms-is-charging nil)
(def bms-charger-just-plugged nil)
(def bms-charger-plug-in-time 0)
(def vin -1)
(def last-running-state-time 0)
(def battery-percent-remaining 0.0)

(def FLOAT_MAGIC 101)
(def FLOAT_ACCESSORIES_MAGIC 102)

(def float-cmds '(
    (COMMAND_GET_INFO . 0)
    (COMMAND_GET_ALLDATA . 10)
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
        (float-cmd can-id (list (assoc float-cmds 'COMMAND_GET_ALLDATA) 3))

        (if (or (>= bms-can-id 0) (< (secs-since bms-last-activity-time) 1)){
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
                                (setq roll-angle (/ (to-float (bufget-i16 data 7)) 10))
                                (var state-byte (bufget-u8 data 9))
                                (setq state (bitwise-and state-byte 0x0F))
                                (setq sat-t (shr state-byte 4))
                                (var switch-state-byte (bufget-u8 data 10))
                                (setq switch-state (bitwise-and switch-state-byte 0x0F))
                                ;(var beep-reason-t (shr switch-state-byte 4))
                                (setq handtest-mode (= (bitwise-and switch-state-byte 0x08) 0x08))
                                (var footpad-adc1-t (/ (to-float (bufget-u8 data 11)) 50))
                                (var footpad-adc2-t (/ (to-float (bufget-u8 data 12)) 50))
                                (if (= switch-state 2) {
                                    (setq switch-state 3)
                                })
                                (if (= switch-state 1) {
                                    (if (> footpad-adc2-t footpad-adc1-t) {
                                        (setq switch-state 2)
                                    })
                                })
                                (setq pitch-angle (/ (to-float (bufget-i16 data 19)) 10))
                                (setq vin (/ (to-float (bufget-i16 data 22)) 10))
                                (setq rpm (/ (to-float  (bufget-i16 data 24)) 10))
                                (setq speed (/ (to-float (bufget-i16 data 26)) 10))
                                (setq tot-current (/ (to-float (bufget-i16 data 28)) 10))
                                (setq duty-cycle-now (/ (to-float (- (bufget-u8 data 32) 128)) 100))
                                (if (>= mode 2) {
                                    (setq distance-abs (bufget-f32 data 34))
                                    (setq fet-temp-filtered (/ (bufget-u8 data 38) 2.0))
                                    (setq motor-temp-filtered (/ (bufget-u8 data 39) 2.0))
                                })
                                (if (>= mode 3) {
                                    (setq odometer (bufget-u32 data 41))
                                    (setq battery-percent-remaining (/ (to-float (bufget-u8 data 53)) 2))
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
