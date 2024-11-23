;@const-symbol-strings
@const-start
(def can-loop-delay)  ; Loop delay in microseconds (100ms)
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


(def FLOAT_MAGIC 101) ; Magic number used by float
(def FLOAT_ACCESSORIES_MAGIC 102)

(def float-cmds '(
    (COMMAND_GET_INFO . 0)
    (COMMAND_GET_ALLDATA . 10)
    (COMMAND_LCM_POLL . 24)
    (COMMAND_LCM_LIGHT_INFO  . 25)
    (COMMAND_LCM_GET_BATTERY . 29)
    (COMMAND_LIGHTS_CONTROL . 202)
))

(def discover-can-id -1)

;(defun watchdog () {
    ;restart spawn process
;})

(defun running-state (){
    ;check if we're running and also provide 2sec delay for a fault state if it's going over a duty cycle to prevent headlights flickering. This may be already fixed with wheelslip for direction.
    ;(if (and (>= state 1) (< state 5)) (setq last-running-state-time (systime)))
    ;(let ret (or (and (>= state 1) (<= state 5)) (and  (>= state 8) (<= state 9) (> (secs-since last-running-state-time) 2) (>= duty-cycle-now 1))))
    (let ret (and (>= state 1) (<= state 5)))
})

(defun can-loop (){
    (setq can-loop-delay (get-config 'can-loop-delay))
    (var next-run-time (secs-since 0))  ; Set first run time
    (var loop-start-time 0)
    (var loop-end-time 0)
    (var can-loop-delay-sec (/ 1.0 can-loop-delay))
    (init-can)
    (loopwhile t {
        (setq loop-start-time  (secs-since 0))
        ;(float-cmd can-id (list (assoc float-cmds 'COMMAND_LCM_POLL)))
        ;(float-cmd can-id (list (assoc float-cmds 'COMMAND_LIGHTS_CONTROL)))
        ;(float-cmd can-id (list (assoc float-cmds 'COMMAND_LCM_GET_BATTERY)))
        (float-cmd can-id (list (assoc float-cmds 'COMMAND_GET_ALLDATA) 3))
        ;(get-vesc-status-msg)

        (if (>= bms-can-id 0){
            (var prev-charging-state bms-is-charging)
            (setq bms-is-charging (and (> (get-bms-val 'bms-v-charge) 10.0) (< (get-bms-val 'bms-i-in-ic) 0.1)))
            ;(print "hi")
            ;(print (get-bms-val 'bms-v-charge))
            ;(print (get-bms-val 'bms-i-in-ic))

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

        ; Capture end time and calculate actual loop time
        (setq loop-end-time (secs-since 0))
        (var actual-loop-time (- loop-end-time loop-start-time))

        ; Timing control
        (var time-to-wait (- next-run-time (secs-since 0)))  ; Calculate remaining time to wait in seconds

        ;(print (str-merge "Loop start: " (str-from-n loop-start-time "%.3f")))
        ;(print (str-merge "Loop end: " (str-from-n loop-end-time "%.3f")))
        ;(print (str-merge "Time to wait: " (str-from-n time-to-wait "%.3f")))

        ; Adjust for negative time-to-wait
        (if (> time-to-wait 0) {
            (yield (* time-to-wait 1000000))  ; Convert seconds to microseconds for yield
        } {
            ; If running behind, sync next-run-time with current time to avoid drift
            (setq next-run-time (secs-since 0))
        })

        ; Update next-run-time for the next iteration
        (setq next-run-time (+ next-run-time can-loop-delay-sec))

        ; Log actual loop time for debugging
        ;(print (str-merge "Actual loop time: " (str-from-n actual-loop-time "%.3f") " s"))
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
            (float-cmd can-id (list (assoc float-cmds 'COMMAND_GET_INFO)))
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

;(defun get-vesc-status-msg (){
    ;(setq tot-current (canget-current can-id))
    ;(setq distance-abs (canget-dist can-id))
    ;(setq speed (canget-speed can-id))
    ;(setq fet-temp-filtered  (canget-temp-fet can-id))
    ;(setq motor-temp-filtered (canget-temp-motor can-id))
    ;(setq vin (canget-vin can-id))
    ;(if (= vin-prev -1){ (setq vin-prev vin) (setq vin-sample vin) })
;})

(defun float-cmd (can-id cmd) {
    (send-data (append (list FLOAT_MAGIC) cmd) 2 can-id)
})

(defun float-command-rx (data) {
    ;(print "hi")
    ;(print (map (fn (x) (bufget-u8 data x)) (range (buflen data))))
    ;Support for saving config/code exec from qml

        (if (and (> (buflen data) 1) (= (bufget-u8 data 0) FLOAT_ACCESSORIES_MAGIC)) {
            (bufcpy data 0 data 1 (-(buflen data) 1))
            ;(print (read data))
            (eval (read data))
        })
    ; Only process data if data is long enough and magic number is correct
    (if (and (> (buflen data) 1) (= (bufget-u8 data 0) FLOAT_MAGIC)) {
        (setq can-last-activity-time (systime))
        (match (cossa float-cmds (bufget-u8 data 1))
            (COMMAND_GET_INFO {
                (set-config 'can-id discover-can-id)
                (setq can-id discover-can-id)
                (setq bms-can-id (get-bms-val 'bms-can-id))
            })
            ;(COMMAND_LIGHTS_CONTROL { ;TODO Maybe add float app too for backwards config?
                ;(var led-state (bufget-u8 data 2))
                ;(setq led-on (bits-dec-int led-state 0 1))  ; Get the LSB
                ;(setq led-highbeam-on (bits-dec-int led-state 1 1))  ; Get the second bit
            ;})
            ;(COMMAND_LCM_POLL {
                ;(if (> (buflen data) 13){
                        ;(var send-state (bufget-u8 data 2))
                        ;(setq fault-code (bufget-u8 data 3))
                        ;(var third-byte (bufget-u8 data 4))

                        ; Parse send-state
                        ;(setq state (bitwise-and send-state 0x0F))  ; Lower 4 bits
                        ;(setq switch-state (bitwise-and (shr send-state 4) 0x07))  ; Bits 4-6
                        ;(setq handtest-mode (= (bitwise-and send-state 0x80) 0x80))  ; Bit 7

                        ; Parse the third byte based on state
                        ;(if (and (>= state 1) (<= state 5)) {
                            ;(setq pitch-angle 0.0)
                            ;(setq duty-cycle-now (to-float third-byte))
                       ; }{
                            ;(setq pitch-angle (to-float (if (> third-byte 127)
                                                            ;(- third-byte 256)
                                                            ;third-byte)))
                            ;(setq duty-cycle-now 0.0)
                        ;})

                        ; Parse the rest of the data
                        ;(setq rpm (/ (to-float (bufget-i16 data 5)) 10))
                        ;(var tot-current-in (/ (to-float (bufget-i16 data 7)) 10))
                        ;(print tot-current-in)
                        ;(setq vin (/ (to-float (bufget-i16 data 9)) 10))
                        ;(setq led-brightness (/ (bufget-u8 data 11) 100.0))
                        ;(setq led-brightness-idle (/ (bufget-u8 data 12) 100.0))
                        ;(setq led-brightness-status (/ (bufget-u8 data 13) 100.0))

                        ; You can add extra processing here if needed
                    ;})
            ;})
                ;(COMMAND_LCM_GET_BATTERY {
                    ;(if  (> (buflen data) 2){
                        ;(setq battery-percent-remaining (bufget-f32 data 2))
                    ;})
                ;})
                (COMMAND_GET_ALLDATA {
                    (if  (> (buflen data) 3){
                        (var mode (bufget-u8 data 2))

                        (if (= mode 69) {
                            (setq fault-code (bufget-u8 data 3))
                        }{
                            (setq fault-code 0)
                            (if (>= (buflen data) 32) {
                                ;(def pid-value-t (/ (to-float (bufget-i16 data 3)) 10))
                                ;(def balance-pitch-t (/ (to-float (bufget-i16 data 5)) 10))
                                (def roll-angle (/ (to-float (bufget-i16 data 7)) 10))

                                (var state-byte (bufget-u8 data 9))
                                (setq state (bitwise-and state-byte 0x0F))
                                ;(def sat-t (shr state-byte 4))

                                (var switch-state-byte (bufget-u8 data 10))
                                (setq switch-state (bitwise-and switch-state-byte 0x0F))
                                ;(def beep-reason-t (shr switch-state-byte 4))
                                (setq handtest-mode (= (bitwise-and switch-state-byte 0x08) 0x08))

                                (def footpad-adc1-t (/ (bufget-u8 data 11) 50))
                                (def footpad-adc2-t (/ (bufget-u8 data 12) 50))

                                (if (= switch-state 2) {
                                    (setq switch-state 3)
                                })

                                (if (= switch-state 1) {
                                    (if (> footpad-adc2-t footpad-adc1-t) {
                                        (setq switch-state 2)
                                    })
                                })

                                ; Setpoints
                                ;(def setpoint-t (- (bufget-u8 data 13) 128))
                                ;(def atr-offset-t (- (bufget-u8 data 14) 128))
                                ;(def braketilt-offset-t (- (bufget-u8 data 15) 128))
                                ;(def torquetilt-offset-t (- (bufget-u8 data 16) 128))
                                ;(def turntilt-interpolated-t (- (bufget-u8 data 17) 128))
                                ;(def inputtilt-interpolated-t (- (bufget-u8 data 18) 128))

                                (setq pitch-angle (/ (to-float (bufget-i16 data 19)) 10))
                                ;(def applied-booster-current-t (- (bufget-u8 data 21) 128))

                                ; Motor stuff
                                (setq vin (/ (to-float (bufget-i16 data 22)) 10))
                                (if (= vin-prev -1){ (setq vin-prev vin) (setq vin-sample vin) })
                                (setq rpm (/ (to-float  (bufget-i16 data 24)) 10))
                                (setq speed (/ (to-float (bufget-i16 data 26)) 10))
                                (setq tot-current (/ (to-float (bufget-i16 data 28)) 10))
                                ;(def tot-current-in-t (/ (to-float (bufget-i16 data 30)) 10))
                                (setq duty-cycle-now (/ (to-float (- (bufget-u8 data 32) 128)) 100))
                                ;(print (canget-duty can-id))
                                ;(print duty-cycle-now)
                                ;(def foc-id-t (/ (bufget-u8 data 33) 3.0))

                                (if (and (>= mode 2) (>= (buflen data) 39)) {
                                    ;(def distance-abs-t (bufget-f32-auto data 34))
                                    (setq fet-temp-filtered (/ (bufget-u8 data 38) 2.0))
                                    (setq motor-temp-filtered (/ (bufget-u8 data 39) 2.0))
                                    ;(def batt-temp-t (/ (bufget-u8 data 40) 2.0)) ; Always 0 in the C code
                                })

                                (if (and (>= mode 3) (>= (buflen data) 52)) {
                                    (setq odometer (bufget-u32 data 41))
                                    (if (= odometer-init -1) (setq odometer-init odometer))
                                    (setq distance-abs (- odometer odometer-init))
                                    ;(def amp-hours-t (/ (to-float (bufget-i16 data 45)) 10))
                                    ;(def amp-hours-charged-t (/ (to-float (bufget-i16 data 47)) 10))
                                    ;(def watt-hours-t (to-float (bufget-i16 data 49)))
                                    ;(def watt-hours-charged-t (to-float (bufget-i16 data 51)))
                                    (setq battery-percent-remaining (/ (to-float (bufget-u8 data 53)) 2))
                                    ;(print battery-percent-remaining)
                                })

                                ;(if (and (>= mode 4) (>= (buflen data) 56)) {
                                    ;(def charging-current-t (/ (to-float (bufget-i16 data 54)) 10))
                                    ;(def charging-voltage-t (/ (to-float (bufget-i16 data 56)) 10))
                                ;})
                            })
                        })
                    })
                })
                (_ nil) ; Ignore other commands
            )
    })
})
@const-end