;@const-symbol-strings
@const-start


(defun recv-control (in-led-on in-led-highbeam-on in-led-brightness in-led-brightness-highbeam in-led-brightness-idle in-led-brightness-status in-bms-charge-state) {
    (setq led-on (to-i in-led-on))
    (setq led-highbeam-on (to-i in-led-highbeam-on))
    (setq led-brightness (to-float in-led-brightness))
    (setq led-brightness-highbeam (to-float in-led-brightness-highbeam))
    (setq led-brightness-idle (to-float in-led-brightness-idle))
    (setq led-brightness-status (to-float in-led-brightness-status))

    (set-config 'led-on (to-i in-led-on))
    (set-config 'led-highbeam-on (to-i in-led-highbeam-on))
    (set-config 'led-brightness (to-float in-led-brightness))
    (set-config 'led-brightness-highbeam (to-float in-led-brightness-highbeam))
    (set-config 'led-brightness-idle (to-float in-led-brightness-idle))
    (set-config 'led-brightness-status (to-float in-led-brightness-status))

    (if (and (= (get-config 'bms-enabled) 1) (> bms-type 1) (!= bms-charge-state in-bms-charge-state) ) {
        (setq bms-charge-state (if (= bms-charge-state 1) 1 0))
        (setq bms-user-cmd 0x64)
    })
})

(defun bms-trigger-factory-init () {
    (if (and (= (get-config 'bms-enabled) 1) (> bms-type 1) (= bms-rs485-chip 1) ) {
        (setq bms-user-cmd 0x0e)
    })
})

(defun send-control () {
    (var config-string "control ")

    (setq config-string (
        str-merge
        config-string
        (str-from-n (to-i led-on) "%d ")
        (str-from-n (to-i led-highbeam-on) "%d ")
        (str-from-n led-brightness "%.2f ")
        (str-from-n led-brightness-highbeam "%.2f ")
        (str-from-n led-brightness-idle "%.2f ")
        (str-from-n led-brightness-status "%.2f ")
        (str-from-n (to-i bms-charge-state) "%d ")
    ))

    (send-data config-string)
})

(defun recv-config (in-led-enabled in-bms-enabled in-pubmote-enabled in-led-on in-led-highbeam-on in-led-mode in-led-mode-idle in-led-mode-status
    in-led-mode-startup in-led-mode-button in-led-mode-footpad in-led-mall-grab-enabled in-led-brake-light-enabled in-led-brake-light-min-amps
    in-idle-timeout in-idle-timeout-shutoff in-led-brightness in-led-brightness-highbeam in-led-brightness-idle in-led-brightness-status
    in-led-status-pin in-led-status-num in-led-status-type in-led-status-reversed in-led-front-pin in-led-front-num in-led-front-type
    in-led-front-reversed in-led-front-strip-type in-led-rear-pin in-led-rear-num in-led-rear-type in-led-rear-reversed in-led-rear-strip-type
    in-led-button-pin in-led-button-strip-type in-led-footpad-pin in-led-footpad-num in-led-footpad-type in-led-footpad-reversed
    in-led-footpad-strip-type in-bms-rs485-di-pin in-bms-rs485-ro-pin in-bms-rs485-dere-pin in-bms-wakeup-pin in-bms-override-soc in-bms-rs485-chip
    in-led-loop-delay in-bms-loop-delay in-pubmote-loop-delay in-can-loop-delay in-led-max-blend-count in-led-startup-timeout
    in-led-dim-on-highbeam-ratio in-bms-type in-led-status-strip-type in-bms-charge-only in-led-fix in-led-show-battery-charging
    in-led-front-highbeam-pin in-led-rear-highbeam-pin in-bms-buff-size in-led-max-brightness in-soc-type in-cell-type in-led-update-not-running
    in-log-enabled in-log-rate in-log-append-gnss in-humidity-enabled in-humidity-sda-pin in-humidity-slc-pin
) {

    (var prev-led-enabled (get-config 'led-enabled))
    (set-config 'led-enabled (to-i in-led-enabled))
    (set-config 'bms-enabled (to-i in-bms-enabled))
    (set-config 'pubmote-enabled (to-i in-pubmote-enabled))

    ; Only stop LED loop if LEDs are being disabled
    (if (and (>= led-context-id 0) (!= (to-i in-led-enabled) 1)) {
        (let ((start-time (systime)) (timeout-val 2000000)) ; 2 sec timeout

        (setq led-exit-flag t)

            (loopwhile (and led-exit-flag (< (- (systime) start-time) timeout-val))
                (yield 10000)) ; 10 ms wait

            ; Check if exited due to timeout
            (if led-exit-flag {
                (send-msg "WARNING: LED loop did not exit in time.")
            })
        )
        (setq led-context-id -1)
    })

    (if (>= bms-context-id 0) {
        (let ((start-time (systime)) (timeout-val 2000000)) ; 2 sec timeout

            (setq bms-exit-flag t)

            (loopwhile (and bms-exit-flag (< (- (systime) start-time) timeout-val))
                (yield 10000))

            ; Check if exited due to timeout
            (if bms-exit-flag {
                (send-msg "WARNING: BMS loop did not exit in time.")
            })
        )
    })

    (if (or (!= (to-i soc-type) (to-i in-soc-type)) (!= (to-i cell-type) (to-i in-cell-type))) {
        ; Apply battery calculation
        (apply-battery-config in-soc-type in-cell-type)  
    })

    (set-config 'led-on (to-i in-led-on))
    (set-config 'led-highbeam-on (to-i in-led-highbeam-on))
    (set-config 'led-mode (to-i in-led-mode))
    (set-config 'led-mode-idle (to-i in-led-mode-idle))
    (set-config 'led-mode-status (to-i in-led-mode-status))
    (set-config 'led-mode-startup (to-i in-led-mode-startup))
    (set-config 'led-mode-button (to-i in-led-mode-button))
    (set-config 'led-mode-footpad (to-i in-led-mode-footpad))
    (set-config 'led-mall-grab-enabled (to-i in-led-mall-grab-enabled))
    (set-config 'led-brake-light-enabled (to-i in-led-brake-light-enabled))
    (set-config 'led-brake-light-min-amps (to-float in-led-brake-light-min-amps))
    (set-config 'idle-timeout (to-i in-idle-timeout))
    (set-config 'idle-timeout-shutoff (to-i in-idle-timeout-shutoff))
    (set-config 'led-brightness (to-float in-led-brightness))
    (set-config 'led-brightness-highbeam (to-float in-led-brightness-highbeam))
    (set-config 'led-brightness-idle (to-float in-led-brightness-idle))
    (set-config 'led-brightness-status (to-float in-led-brightness-status))

    (set-config 'led-status-num (to-i in-led-status-num))
    (set-config 'led-status-type (to-i in-led-status-type))
    (set-config 'led-status-reversed (to-i in-led-status-reversed))

    (set-config 'led-front-num (to-i in-led-front-num))
    (set-config 'led-front-type (to-i in-led-front-type))
    (set-config 'led-front-reversed (to-i in-led-front-reversed))
    (set-config 'led-front-strip-type (to-i in-led-front-strip-type))

    (set-config 'led-rear-num (to-i in-led-rear-num))
    (set-config 'led-rear-type (to-i in-led-rear-type))
    (set-config 'led-rear-reversed (to-i in-led-rear-reversed))
    (set-config 'led-rear-strip-type (to-i in-led-rear-strip-type))

    (set-config 'led-button-strip-type (to-i in-led-button-strip-type))

    (set-config 'led-footpad-num (to-i in-led-footpad-num))
    (set-config 'led-footpad-type (to-i in-led-footpad-type))
    (set-config 'led-footpad-reversed (to-i in-led-footpad-reversed))
    (set-config 'led-footpad-strip-type (to-i in-led-footpad-strip-type))
    (var bms-rs485-di-pin-prev (get-config 'bms-rs485-di-pin))
    (var bms-rs485-ro-pin-prev (get-config 'bms-rs485-ro-pin))
    (var bms-rs485-dere-pin-prev (get-config 'bms-rs485-dere-pin))
    (var bms-wakeup-pin-prev (get-config 'bms-wakeup-pin))

    (set-config 'bms-rs485-di-pin (to-i in-bms-rs485-di-pin))
    (set-config 'bms-rs485-ro-pin (to-i in-bms-rs485-ro-pin))
    (set-config 'bms-rs485-dere-pin (to-i in-bms-rs485-dere-pin))
    (set-config 'bms-wakeup-pin (to-i in-bms-wakeup-pin))
    (set-config 'bms-override-soc (to-i in-bms-override-soc))
    (set-config 'bms-rs485-chip (to-i in-bms-rs485-chip))

    (set-config 'led-loop-delay (to-i in-led-loop-delay))
    (set-config 'bms-loop-delay (to-i in-bms-loop-delay))
    (set-config 'pubmote-loop-delay (to-i in-pubmote-loop-delay))
    (set-config 'can-loop-delay (to-i in-can-loop-delay))
    (set-config 'led-max-blend-count (to-i in-led-max-blend-count))
    (set-config 'led-startup-timeout (to-i in-led-startup-timeout))
    (set-config 'led-dim-on-highbeam-ratio (to-float in-led-dim-on-highbeam-ratio))

    (set-config 'bms-type (to-i in-bms-type))
    (set-config 'led-status-strip-type (to-i in-led-status-strip-type))
    (set-config 'bms-charge-only (to-i in-bms-charge-only))
    (set-config 'led-fix (to-i in-led-fix))
    (set-config 'led-show-battery-charging (to-i in-led-show-battery-charging))
    (set-config 'bms-buff-size (to-i in-bms-buff-size))
    (set-config 'led-max-brightness (to-float in-led-max-brightness))
    (set-config 'soc-type (to-i in-soc-type))
    (set-config 'cell-type (to-i in-cell-type))
    (set-config 'led-update-not-running  (to-i in-led-update-not-running))

    (set-config 'log-enabled  (to-i in-log-enabled))
    (set-config 'log-rate (to-i in-log-rate))
    (set-config 'log-append-gnss (to-i in-log-append-gnss))
    (set-config 'humidity-enabled (to-i in-humidity-enabled))
    ; Stop humidity loop if pins changed so it can be restarted with new config
    (if (and (>= humidity-context-id 0) (or (!= (to-i (get-config 'humidity-sda-pin)) (to-i in-humidity-sda-pin)) (!= (to-i (get-config 'humidity-slc-pin)) (to-i in-humidity-slc-pin)))) {
        (let ((start-time (systime)) (timeout-val 2000000))
            (setq humidity-exit-flag t)
            (loopwhile (and humidity-exit-flag (< (- (systime) start-time) timeout-val))
                (yield 10000))
            (if humidity-exit-flag {
                (send-msg "WARNING: Humidity loop did not exit in time.")
            })
        )
        (setq humidity-context-id -1)
    })
    (set-config 'humidity-sda-pin (to-i in-humidity-sda-pin))
    (set-config 'humidity-slc-pin (to-i in-humidity-slc-pin))


    (set-config 'led-status-pin (to-i in-led-status-pin))
    (set-config 'led-front-pin (to-i in-led-front-pin))
    (set-config 'led-rear-pin (to-i in-led-rear-pin))
    (set-config 'led-button-pin (to-i in-led-button-pin))
    (set-config 'led-footpad-pin (to-i in-led-footpad-pin))
    (set-config 'led-front-highbeam-pin (to-i in-led-front-highbeam-pin))
    (set-config 'led-rear-highbeam-pin (to-i in-led-rear-highbeam-pin))

    ; If LED loop is running and LEDs remain enabled, reinit in-place
    (if (and (>= led-context-id 0) (= (get-config 'led-enabled) 1)) {
        (setq led-reinit-flag t)
    })
    ; If LEDs are newly enabled, spawn the loop
    (if (and (= led-context-id -1) (= (get-config 'led-enabled) 1)) {
        (setq led-context-id (spawn led-loop))
    })
    (setq bms-context-id (if (= (get-config 'bms-enabled) 1) (spawn bms-loop) -1))

    (if (= (to-i in-humidity-enabled) 1) {
        (if (= humidity-context-id -1) (setq humidity-context-id (spawn humidity-loop)))
    })

    ; Stop pubmote if it was running and is now disabled
    (if (and (>= pubmote-context-id 0) (!= (to-i in-pubmote-enabled) 1)) {
        (let ((start-time (systime)) (timeout-val 2000000))
            (setq pubmote-exit-flag t)
            (loopwhile (and pubmote-exit-flag (< (- (systime) start-time) timeout-val))
                (yield 10000))
            (if pubmote-exit-flag {
                (send-msg "WARNING: Pubmote loop did not exit in time.")
            })
        )
        (setq pubmote-context-id -1)
    })
    (if (= (to-i in-pubmote-enabled) 1) {
        (if (= pubmote-context-id -1) (setq pubmote-context-id (spawn pubmote-loop)))
    })

    (if (= in-log-enabled 1) {
        (if (= log-context-id -1) {
            (setq log-context-id (spawn log-loop))
        }{
            (start-log (get-config 'log-append-gnss) (get-config 'log-rate))
        })
    }{
        (stop-log)
    })

    (save-config)
    (send-config)

})

(defun send-keys (key-list counter-list) {
    (print "Received key: ")
    (print key-list)
    (setq key-list (split-list key-list 4))
    (set-config 'bms-key-a (pack-bytes-to-uint32 (ix key-list 0)))
    (set-config 'bms-key-b (pack-bytes-to-uint32 (ix key-list 1)))
    (set-config 'bms-key-c (pack-bytes-to-uint32 (ix key-list 2)))
    (set-config 'bms-key-d (pack-bytes-to-uint32 (ix key-list 3)))
    (print "Received counter: ")
    (print counter-list)
    (setq counter-list (split-list counter-list 4))
    (set-config 'bms-counter-a (pack-bytes-to-uint32 (ix counter-list 0)))
    (set-config 'bms-counter-b (pack-bytes-to-uint32 (ix counter-list 1)))
    (set-config 'bms-counter-c (pack-bytes-to-uint32 (ix counter-list 2)))
    (set-config 'bms-counter-d (pack-bytes-to-uint32 (ix counter-list 3)))
    (save-config)
})

(defun accept-tos() {
    (atomic {
        (set-config 'accept-tos 1)
        (write-val-eeprom 'accept-tos 1)
        (write-val-eeprom 'crc (config-crc cfg-len))
    })
})

(defun send-config () {

    (var config-string "settings ")

    (loopforeach setting eeprom-addrs {
        (let
            ((name (first setting)) (type (third setting))) {
                (var value (read-val-eeprom name))

                (setq config-string
                    (str-merge config-string
                        (cond
                            ((eq type 'b) (str-from-n value "%d "))
                            ((eq type 'i) (str-from-n value "%d "))
                            ((eq type 'f) (str-from-n value "%.2f "))
                        )
                    )
                )
            }
        )
    })

        (send-data config-string)
        (send-status "Settings loaded")
})

(defunret get-config (name) {
    (ix runtime-vals (first (assoc eeprom-addrs name)))
})

(defun set-config (name value) {
    (setix runtime-vals (first (assoc eeprom-addrs name)) value)
})

(defun save-config () {
    (atomic {
        (loopforeach setting eeprom-addrs {
            (var name (first setting))
            (if (not-eq name 'crc) {
                (write-val-eeprom name (get-config name))
            })
        })

        (write-val-eeprom 'crc (config-crc cfg-len))
        (send-status "Settings saved")
    })
})

(defunret config-crc (len) {
    (var i 0)
	(var crclen (* (- len 1) 4))
	(var crcbuf (bufcreate crclen))
    (var j 0)
	(loopforeach setting eeprom-addrs {
        (if (>= j len) {(break)})
        (var name (first setting))

        (if (not-eq name 'crc) {
            (bufset-i32 crcbuf (* i 4) (get-config name))
            (setq i (+ i 1))
        })
        (setq j (+ j 1))
	})
    (var crc (crc16 crcbuf))
    (free crcbuf)
    (return crc)
})

(defun load-config () {
    (setq read-cfg-len 0)
    (loopforeach setting eeprom-addrs {
        (var name (first setting))
        (var val (read-val-eeprom name))
        (set-config name val)
        (if val (setq read-cfg-len (+ read-cfg-len 1)) {(break)})
    })
})

(defun restore-config () {
    (var is-s3-hw (if (= (str-cmp (sysinfo 'hw-name) "Avaspark RGB S3") 0) t nil))
    (var is-tw (= (str-cmp (sysinfo 'hw-name) "Twilight Lord LCM") 0))

    (atomic {
        (loopforeach setting eeprom-addrs {
            (var name (first setting))
            (var default-value (if (eq name 'magic) magic-header (ix setting 3)))
            (match name
                (magic (setq default-value magic-header))
                (led-status-pin (setq default-value (if is-s3-hw 9 7)))
                (led-front-pin (setq default-value (if is-s3-hw 15 8)))
                (led-front-highbeam-pin (setq default-value (if is-s3-hw 14 -1)))
                (led-front-strip-type (setq default-value (if is-s3-hw 7 0)))
                (led-rear-pin (setq default-value (if is-s3-hw 12 9)))
                (led-rear-highbeam-pin (setq default-value (if is-s3-hw 13 -1)))
                (led-rear-strip-type (setq default-value (if is-s3-hw 7 0)))
                (humidity-enabled (setq default-value (if is-tw 1 0)))
                (humidity-sda-pin (setq default-value (if is-tw 10 -1)))
                (humidity-slc-pin (setq default-value (if is-tw 8 -1)))
                (_ (setq default-value (ix setting 3)))
            )
            (write-val-eeprom name default-value)
        })
        (write-val-eeprom 'crc (config-crc cfg-len))
    })
        (load-config)
        (send-status "Settings restored")
})

(defun print-config ()
    (loopforeach it eeprom-addrs (print (list (first it) (read-val-eeprom (first it)))))
)

(defun read-val-eeprom (name)
    (let
        (
            (addr (first (assoc eeprom-addrs name)))
            (type (second (assoc eeprom-addrs name)))
        )
        (cond
            ((eq type 'i) (eeprom-read-i addr))
            ((eq type 'f) (eeprom-read-f addr))
            ((eq type 'b) (eeprom-read-i addr))
        )
    )
)

(defun write-val-eeprom (name val)
    (let
        (
            (addr (first (assoc eeprom-addrs name)))
            (type (second (assoc eeprom-addrs name)))
        )
        (cond
            ((eq type 'i) (eeprom-store-i addr val))
            ((eq type 'f) (eeprom-store-f addr val))
            ((eq type 'b) (eeprom-store-i addr val))
        )
    )
)

(defun status () {
    (var status-string "float-stats ")
    (setq status-string (str-merge status-string (str-from-n (if (< (secs-since can-last-activity-time) 1) 1 0) "%d ")))
    (setq status-string (str-merge status-string (str-from-n (is-pubmote-connected) "%d ")))
    (setq status-string (str-merge status-string (str-from-n (if (< (secs-since bms-last-activity-time) 1) 1 0) "%d ")))
    (setq status-string (str-merge status-string (str-from-n bms-status "%d ")))
    (setq status-string (str-merge status-string (str-from-n bms-battery-type "%d ")))
    (setq status-string (str-merge status-string (str-from-n bms-battery-cycles "%d ")))
    (setq status-string (str-merge status-string (str-from-n (if (> (conf-get 'wifi-mode) 0) (wifi-get-chan) -1) "%d ")))
    (setq status-string (str-merge status-string (str-from-n hum "%.0f ")))
    (setq status-string (str-merge status-string (str-from-n hum-temp "%.2f ")))
    (setq status-string (str-merge status-string (str-from-n (get-bms-val 'bms-hum) "%.0f ")))
    (setq status-string (str-merge status-string (str-from-n (get-bms-val 'bms-temp-hum) "%.0f ")))
    (setq status-string (str-merge status-string (str-from-n (if log-running 1 0) "%d ")))
    (send-data status-string)

    (if (= (is-pubmote-connected) 1) {
        (send-data (str-merge "pubmote-info " (to-str (ix pubmote-version 0)) "." (to-str (ix pubmote-version 1)) "." (to-str (ix pubmote-version 2))))
    })
})

(defun input-state () {
    (var input-string "input-state ")
    (setq input-string (str-merge input-string (str-from-n (is-pubmote-connected) "%d ")))
    (setq input-string (str-merge input-string (str-from-n pubmote-last-jsy "%.3f ")))
    (setq input-string (str-merge input-string (str-from-n pubmote-last-jsx "%.3f ")))
    (setq input-string (str-merge input-string (str-from-n pubmote-last-bt-c "%d ")))
    (setq input-string (str-merge input-string (str-from-n pubmote-last-bt-z "%d ")))
    (setq input-string (str-merge input-string (str-from-n pubmote-last-is-rev "%d")))
    (send-data input-string)
})

@const-end