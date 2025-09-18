;@const-symbol-strings
;Future interesting functions
;(conf-detect-foc canFwd maxLoss minCurrIn maxCurrIn openloopErpm slErpm)
;(conf-set) 'can-status-rate-hz 'foc-fw-duty-start 'foc-fw-current-max  'foc-offsets-cal-on-boot 'foc-sl-erpm-start 'foc-observer-gain 'foc-f-zv 'si-battery-ah 'si-battery-cells 'si-wheel-diameter  'si-gear-ratio  'si-motor-poles 'motor-type 'foc-sensor-mode 'l-current-min 'l-current-max 'l-abs-current-max 'l-min-vin 'l-max-vin 'l-battery-cut-start 'l-battery-cut-end 'l-temp-motor-start 'l-temp-motor-end 'l-temp-accel-dec 'bms-limit-mode 'bms-t-limit-start 'bms-t-limit-end 'bms-vmin-limit-start 'bms-vmin-limit-end 'bms-vmax-limit-start 'bms-vmax-limit-end
;(stats 'stat-speed-max) ; Maximum speed in m/s
;(stats-reset)

;(event-enable 'event-shutdown) ; -> event-shutdown
;(lbm-set-quota quota)
;(timeout-reset)
;GNSS stuff

;(reboot)
@const-start

(def FLOAT_MAGIC 101)
(def float-cmds '(
    (COMMAND_GET_INFO . 0)
    (COMMAND_GET_ALLDATA . 10)
    (COMMAND_HUMIDITY . 51)
))

(def FLOAT_ACCESSORIES_MAGIC_QML 103)
(def float-accessories-qml-cmds '(
    (COMMAND_QML_POP_UP . 0)
    (COMMAND_QML_MSG . 1)
    (COMMAND_QML_STATUS . 2)
))


(def FLOAT_ACCESSORIES_MAGIC 104)
(def float-accessories-cmds '(
    (COMMAND_GET_INFO . 0)
    (COMMAND_PUBMOTE_PAIR_START . 1)
    (COMMAND_PUBMOTE_PAIR_ACCEPT . 2)
    (COMMAND_PUBMOTE_PAIR_REJECT . 3)
    (COMMAND_GET_STATUS . 4)
    (COMMAND_REBOOT . 5)
))

(defun status () {
    (var payload (list 
        FLOAT_ACCESSORIES_MAGIC_QML 
        (assoc float-accessories-qml-cmds 'COMMAND_QML_STATUS)
        (if (< (secs-since can-last-activity-time) 1) 1 0) 
        (if (< (secs-since pubmote-last-activity-time) 1) 1 0) 
        (if wifi-enabled-on-boot (wifi-get-chan) -1) 
        (if pubmote-enabled 1 0)
        pubmote-version-major 
        pubmote-version-minor
        pubmote-version-patch
        (if (ix esp-now-remote-mac-a 0) (ix esp-now-remote-mac-a 0) 0)
        (if (ix esp-now-remote-mac-a 1) (ix esp-now-remote-mac-a 1) 0)
        (if (ix esp-now-remote-mac-a 2) (ix esp-now-remote-mac-a 2) 0)
        (if (ix esp-now-remote-mac-a 3) (ix esp-now-remote-mac-a 3) 0)
        (if (ix esp-now-remote-mac-b 0) (ix esp-now-remote-mac-b 0) 0)
        (if (ix esp-now-remote-mac-b 1) (ix esp-now-remote-mac-b 1) 0)
    ))
    ;(print (bytes-to-mac-str esp-now-remote-mac-a esp-now-remote-mac-b))
    (send-data payload)
})

(defun print-hex (data)
    (print
        (map (fn (x) (bufget-u8 data x)) (range (buflen data)))
    )
)

(def can-last-activity-time 0)
(def pubmote-last-activity-time 0)

(def mode 0)
(def fault-code 0)
(def pitch-angle 0.0)
(def roll-angle 0.0)
(def state 0)
(def switch-state 0)
(def vin 0.0)
(def rpm 0.0)
(def speed 0.0)
(def duty-cycle-now 0.0)
(def distance-abs 0.0)
(def battery-percent-remaining 0.0)
(def tot-current 0.0)
(def fet-temp-filtered 0.0)
(def motor-temp-filtered 0.0)
(def odometer 0u32)
(def footpad-adc1-t 0.0)
(def footpad-adc2-t 0.0)

(defun unpack-uint32-to-bytes (packed-value)
(list (to-byte (shr packed-value 24))
                (to-byte (shr (bitwise-and packed-value 0xFF0000) 16))
                (to-byte (shr (bitwise-and packed-value 0xFF00) 8))
                (to-byte (bitwise-and packed-value 0xFF))))
                


(defun pack-bytes-to-uint32 (byte-list)
(to-u32 (+ (shl (to-u32 (ix byte-list 0)) 24)
                     (shl (to-u32 (ix byte-list 1)) 16)
                     (shl (to-u32 (ix byte-list 2)) 8)
                     (to-u32 (ix byte-list 3))))
)

(defun bytes-to-mac-str (hi-u32-bytes lo-u32-bytes)
(str-merge (str-from-n (ix hi-u32-bytes 0) "%X") ":" (str-from-n (ix hi-u32-bytes 1) "%X") ":" (str-from-n (ix hi-u32-bytes 2) "%X") ":" (str-from-n (ix hi-u32-bytes 3) "%X") ":" (str-from-n (ix lo-u32-bytes 0) "%X") ":" (str-from-n (ix lo-u32-bytes 1) "%X") ))
(defun event-handler ()
    (loopwhile t
        (recv
            ((event-esp-now-rx (? src) (? des) (? data) (? rssi)) (pubmote-rx src des data rssi))
            ((event-data-rx . (? data)) (float-command-rx data))
            (_ nil)
        )
    )
)
(defunret init-can (fw-num cfg-can-id) {
    (var init-time (systime))
    (var can-devices (list))
    (loopwhile (<= (secs-since init-time) 20) {
        (cond
            ((<= (secs-since init-time) 10) { ;try saved can id first
                (if (>= cfg-can-id 0) {
                    (setq can-devices (list cfg-can-id))
                }{
                    (setq can-devices (can-list-devs))
                })
            })
            ((<= (secs-since init-time) 15) { ; then see if ESC is broadcasting status
                (setq can-devices (can-list-devs))
            })
            (_ (setq can-devices (can-scan))) ; finally resort to the congestive task of scanning the can bus.
        )
        (loopforeach i can-devices {
            ;check if can-ping is here and if so use that instead
            (var found-can-id nil)
            (var res (can-ping i))
            (setq found-can-id (and (not-eq res nil) (= res 0)))

            (if (eq found-can-id t) {
                (if (not (= can-id cfg-can-id)) {
                    (ext-can-id i)
                })
                (return i)
            })
            (sleep 0.5)
        })
        (sleep 0.5)
    })
    (return nil)
})
(defun float-command-rx (data) {
    ;(print-hex data)
    ;(print "hi")
    ;(ext-float-rx data)
    ; Only process data if data is long enough and magic number is correct

    (if (and (> (buflen data) 1) (= (bufget-u8 data 0) FLOAT_ACCESSORIES_MAGIC)) {
        (match (cossa float-accessories-cmds (bufget-u8 data 1))
            ;(COMMAND_GET_INFO {
            ;})
            ;(COMMAND_RUN_LISP {
            ;    (bufcpy data 0 data 2 (-(buflen data) 2))
            ;    (buf-resize data -2)
            ;    (eval (read data))
            ;})
                (COMMAND_PUBMOTE_PAIR_START {
                    (if (>(buflen data) 3){
                        (var pairing-code (bufget-i16 data 2))
                        (print pairing-code)
                        (pair-pubmote-start pairing-code)
                    })
                })
                (COMMAND_PUBMOTE_PAIR_ACCEPT {
                    (pair-pubmote-accept)
                })
                (COMMAND_PUBMOTE_PAIR_REJECT {
                    (pair-pubmote-reject)
                })
                (COMMAND_GET_STATUS {
                    (status)
                })
                (COMMAND_REBOOT {
                    (send-msg "Rebooting...")
                    (sleep 1.0)
                    (reboot)
                })
            (_ nil) ; Ignore other commands
        )
    })

    (if (and (> (buflen data) 1) (= (bufget-u8 data 0) FLOAT_MAGIC)) {
        (match (cossa float-cmds (bufget-u8 data 1))
                (COMMAND_GET_ALLDATA {
                    (setq can-last-activity-time (systime))
                                        (if  (> (buflen data) 3){
                        (setq mode (bufget-u8 data 2))

                        (if (= mode 69) {
                            (setq fault-code (bufget-u8 data 3))
                        }{
                            (setq fault-code 0)
                            (if (>= (buflen data) 32) {
                                (setq roll-angle (/ (to-float (bufget-i16 data 7)) 10))
                                (var state-byte (bufget-u8 data 9))
                                (setq state (bitwise-and state-byte 0x0F))
                                ;(setq sat-t (shr state-byte 4))
                                (var switch-state-byte (bufget-u8 data 10))
                                (setq switch-state (bitwise-and switch-state-byte 0x07))
                                ;(var beep-reason-t (shr switch-state-byte 4))
                                ;(setq handtest-mode (= (bitwise-and switch-state-byte 0x08) 0x08))
                                (setq footpad-adc1-t (/ (to-float (bufget-u8 data 11)) 50))
                                (setq footpad-adc2-t (/ (to-float (bufget-u8 data 12)) 50))
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
                                ;(setq bat-current (/ (to-float (bufget-i16 data 30)) 10))
                                (setq duty-cycle-now (/ (to-float (- (bufget-u8 data 32) 128)) 100))
                                (if (>= mode 2) {
                                    (setq distance-abs (bufget-f32 data 34))
                                    (setq fet-temp-filtered (/ (bufget-u8 data 38) 2.0))
                                    (setq motor-temp-filtered (/ (bufget-u8 data 39) 2.0))
                                })
                                (if (>= mode 3) {
                                    (setq odometer (bufget-u32 data 41))
                                    (setq battery-percent-remaining (/ (to-float (bufget-u8 data 53)) 200))
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



(defun send-msg (text){
    (var temp (bufcreate (+(length text) 2)))
    (bufset-u8 temp 0 FLOAT_ACCESSORIES_MAGIC_QML)
    (bufset-u8 temp 1 (assoc float-accessories-qml-cmds 'COMMAND_QML_MSG) )
    (bufcpy temp 2 text 0 (length text))
    (send-data temp )
    (free temp)
})

(def has-si7021 nil)
(def has-aht20 nil)

(defunret init-humidity () {
    (i2c-start 'rate-400k (get-config 'humidity-sda-pin) (get-config 'humidity-slc-pin))
    (setq has-si7021 (i2c-detect-addr 0x40))
    (setq has-aht20 (i2c-detect-addr 0x38))

    (if (or has-si7021 has-aht20) {
        (print "Sensors detected:")
        (if has-si7021 { (print "- Using Si7021 logic at 0x40") })
        (if has-aht20  { (print "- Using AHT20 logic at 0x38") })

        (if has-aht20 {
            (sleep 0.04)
            (i2c-tx-rx 0x38 '(0xBE 0x08 0x00))
        })
        (if has-si7021 {
            (i2c-tx-rx 0x40 '(2 0x10 0))
            (i2c-tx-rx 0x40 '(0))
        })
        (return true)
    })
    (send-msg "No humidity sensor detected.")
    (return false)
})

(defun humidity-loop () {
    (if (init-humidity) {
        (var rx-si7021 (bufcreate 4))
        (var rx-aht20 (bufcreate 6))
        (loopwhile t {
            (if has-si7021 {
                (i2c-tx-rx 0x40 '() rx-si7021)
                (i2c-tx-rx 0x40 (list 0x0F 0x01))
                (i2c-tx-rx 0x40 '(0))

                (setq hum (* (/ (bufget-u16 rx-si7021 2 'little-endian) 65536.0) 100.0))
                (setq hum-temp (- (* (/ (bufget-u16 rx-si7021 0 'little-endian) 65536.0) 165.0) 40.5))

                ;(print (str-merge "[Si7021] Humidity: " (str-from-n hum "%.2f%%")
                ;                   ", Temp: " (str-from-n (+ (* hum-temp 1.8) 32) "%.2fF")
                ;                   " / " (str-from-n hum-temp "%.2fC")))
            })

            (if has-aht20 {
                (i2c-tx-rx 0x38 '(0xAC 0x33 0x00))
                (sleep 0.075)
                (i2c-tx-rx 0x38 '() rx-aht20)

                (var hum-raw (+ (shl (bufget-u8 rx-aht20 1) 12)
                                (shl (bufget-u8 rx-aht20 2) 4)
                                (shr (bufget-u8 rx-aht20 3) 4)))
                (var temp-raw (+ (shl (bitwise-and (bufget-u8 rx-aht20 3) 0x0F) 16)
                                 (shl (bufget-u8 rx-aht20 4) 8)
                                 (bufget-u8 rx-aht20 5)))

                (setq hum (* (/ hum-raw 1048576.0) 100.0))
                (setq hum-temp (- (* (/ temp-raw 1048576.0) 200.0) 50.0))
                ;(print (str-merge "[AHT20] Humidity: " (str-from-n hum "%.2f%%")
                ;                   ", Temp: " (str-from-n (+ (* hum-temp 1.8) 32) "%.2fF")
                ;                   " / " (str-from-n hum-temp "%.2fC")))
            })
            (sleep 1)
        })
        (free rx-si7021)
        (free rx-aht20)
    })
})
