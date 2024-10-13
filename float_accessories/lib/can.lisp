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
(def input-voltage-filtered 0)
(def speed 0)
(def tot-current 0)
(def duty-cycle-now 0)
(def distance-abs -1)
(def fet-temp-filtered 0)
(def motor-temp-filtered 0)
(def odometer-initial -1)
(def odometer -1)
(def battery-level 0)
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
    ;check if we're running and also provide 1sec delay for sensor fault state or if it's going over certian rpm threshold
    (let ret (or (and (>= state 1) (<= state 5)) (and (>= state 8) (<= state 9) (>= rpm 100))))
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
        (float-cmd can-id (list (assoc float-cmds 'COMMAND_LCM_POLL)))
        ;(float-cmd can-id (list (assoc float-cmds 'COMMAND_LIGHTS_CONTROL)))
        (float-cmd can-id (list (assoc float-cmds 'COMMAND_LCM_GET_BATTERY)));TODO maybe use SOC if bms is detected?
        (get-vesc-status-msg)

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

        ;Let's only call COMMAND_GET_ALLDATA once and then we can calculate current trip by adding distance abs
        (if (or (< odometer-initial 0) (< distance-abs 0)){
            (float-cmd (get-config 'can-id) (list (assoc float-cmds 'COMMAND_GET_ALLDATA) 3))
            (setq odometer odometer-initial)
        }{
            (setq odometer (+ odometer-initial distance-abs))
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

(defun get-vesc-status-msg (){
    (setq tot-current (canget-current can-id))
    (setq distance-abs (canget-dist can-id))
    (setq speed (canget-speed can-id))
    (setq fet-temp-filtered  (canget-temp-fet can-id))
    (setq motor-temp-filtered (canget-temp-motor can-id))
    (setq vin (canget-vin can-id))
    (if (= vin-prev -1){ (setq vin-prev vin) (setq vin-sample vin) })
})

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
            (COMMAND_LCM_POLL {
                (if (> (buflen data) 13){
                        (var send-state (bufget-u8 data 2))
                        (setq fault-code (bufget-u8 data 3))
                        (var third-byte (bufget-u8 data 4))

                        ; Parse send-state
                        (setq state (bitwise-and send-state 0x0F))  ; Lower 4 bits
                        (setq switch-state (bitwise-and (shr send-state 4) 0x07))  ; Bits 4-6
                        (setq handtest-mode (= (bitwise-and send-state 0x80) 0x80))  ; Bit 7

                        ; Parse the third byte based on state
                        (if (and (>= state 1) (<= state 5)) {
                            (setq pitch-angle 0.0)
                            (setq duty-cycle-now (to-float third-byte))
                        }{
                            (setq pitch-angle (to-float (if (> third-byte 127)
                                                            (- third-byte 256)
                                                            third-byte)))
                            (setq duty-cycle-now 0.0)
                        })

                        ; Parse the rest of the data
                        (setq rpm (/ (to-float (bufget-i16 data 5)) 10))
                        ;(var tot-current-in (/ (to-float (bufget-i16 data 7)) 10))
                        ;(print tot-current-in)
                        (setq input-voltage-filtered (/ (to-float (bufget-i16 data 9)) 10))
                        ;(setq led-brightness (/ (bufget-u8 data 11) 100.0))
                        ;(setq led-brightness-idle (/ (bufget-u8 data 12) 100.0))
                        ;(setq led-brightness-status (/ (bufget-u8 data 13) 100.0))

                        ; You can add extra processing here if needed
                    })
            })
                (COMMAND_LCM_GET_BATTERY {
                    (if  (> (buflen data) 2){
                        (setq battery-percent-remaining (bufget-f32 data 2))
                    })
                })
                (COMMAND_GET_ALLDATA {
                    (var mode (bufget-u8 data 2))
                    ;(setq speed (/ (to-float (bufget-i16 data 26)) 10))
                    ;(if (>= mode 2) {
                        ;(setq roll-angle (/ (to-float (bufget-i16 data 7)) 10))
                        ;(setq distance-abs (bufget-f32 data 34))
                        ;(setq fet-temp-filtered (/ (bufget-u8 data 38) 2.0))
                        ;(setq motor-temp-filtered (/ (bufget-u8 data 39) 2.0))
                    ;})
                    (if (and (>= mode 3) (>= (buflen data) 42) ) {
                        (setq odometer-initial (bufget-u32 data 41)) ;meters
                        ;(setq battery-level (/ (bufget-u8 data 53) 2.0))
                    })
                    ;(print roll-angle)
                    ;(print distance-abs)
                    ;(print fet-temp-filtered)
                    ;(print motor-temp-filtered)
                    ;(print odometer)
                    ;(print battery-level)
                })
                (_ nil) ; Ignore other commands
            )
    })
    ;(free data)
})
@const-end