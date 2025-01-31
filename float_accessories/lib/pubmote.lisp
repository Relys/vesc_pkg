;@const-symbol-strings

@const-start
(def wifi-enabled-on-boot nil)
(def pubmote-loop-delay)  ; Loop delay in microseconds (100ms)
(def pairing-state 0)
(def esp-now-remote-mac '())
(def pubmote-pairing-timer 31)
(def uni-mac '(255 255 255 255 255 255)) ; Universal mac (all devices)
(defunret init-pubmote () {
    ; Escape without wifi
    (if (not wifi-enabled-on-boot){
        (send-msg "WiFi was disabled on boot. Please enable and reboot to use Pubmote.")
        (return false)
    })
    ; Station mode and connecting should disable auto reconnect to stop channel hopping
    (if (and (eq (conf-get 'wifi-mode) 1) (not-eq (wifi-status) 'connected)) {
        (wifi-auto-reconnect false) ; don't try to auto-reconnect if wifi is in station mode
        (wifi-disconnect)
    })
    (setq esp-now-remote-mac (append (unpack-uint32-to-bytes (get-config 'esp-now-remote-mac-a)) (take (unpack-uint32-to-bytes (get-config 'esp-now-remote-mac-b)) 2)))
    ; Read as bytes, convert to i so we can compare lists
    (loopfor i 0 (< i(length esp-now-remote-mac)) (+ i 1) {
        (setix esp-now-remote-mac i (to-i (ix esp-now-remote-mac i)))
    })
    (esp-now-start)
    (esp-now-del-peer esp-now-remote-mac)
    (esp-now-add-peer esp-now-remote-mac)
    (esp-now-del-peer uni-mac)
    (esp-now-add-peer uni-mac)
    (return true)
})

;todo more robust pairing process. Needs to keep sending packets as a missed packet can lead to invalid state machine.
(defunret pair-pubmote (pairing) {
    (if (eq (conf-get 'wifi-mode) 0) {
        (send-msg "WiFi is disabled. Please enable and reboot.")
        (return false)
    })
    (cond
        ((>= pairing 0) {
            (set-config 'esp-now-secret-code (to-i32 pairing))
            (setq pubmote-pairing-timer (systime))
            (setq pairing-state 1)
        })
        ((eq pairing -1) { ;pairing accepted
            (set-config 'esp-now-remote-mac-a (pack-bytes-to-uint32 (take esp-now-remote-mac 4)))
            (set-config 'esp-now-remote-mac-b (pack-bytes-to-uint32 (append (drop esp-now-remote-mac 4) '(0 0))))
            (write-val-eeprom 'esp-now-remote-mac-a (get-config 'esp-now-remote-mac-a))
            (write-val-eeprom 'esp-now-remote-mac-b (get-config 'esp-now-remote-mac-b))
            (write-val-eeprom 'esp-now-secret-code (get-config 'esp-now-secret-code))
            (write-val-eeprom 'crc (config-crc))
            (init-pubmote)
            (var tmpbuf (bufcreate 4))
            (bufset-i32 tmpbuf 0 -1)
            (esp-now-send esp-now-remote-mac tmpbuf)
            (free tmpbuf)
            (setq pairing-state 0)
        })
        ((eq pairing -2) { ;paring rejected
            (set-config 'esp-now-remote-mac-a -1)
            (write-val-eeprom 'esp-now-remote-mac-a (get-config 'esp-now-remote-mac-a) -1)
            (write-val-eeprom 'crc (config-crc))
            (var tmpbuf (bufcreate 4))
            (bufset-i32 tmpbuf 0 -2)
            (esp-now-send esp-now-remote-mac tmpbuf)
            (free tmpbuf)
            (setq esp-now-remote-mac '())
            (setq pairing-state 0)
        })
    )
    (return true)
})

(defun pubmote-loop () {
    (if (init-pubmote){
        (setq pubmote-loop-delay (get-config 'pubmote-loop-delay))
        (var next-run-time (secs-since 0))
        (var loop-start-time 0)
        (var loop-end-time 0)
        (var pubmote-loop-delay-sec (/ 1.0 pubmote-loop-delay))
        (var data (bufcreate 32))
        (var channel-locked 0)
        (loopwhile t {
            (if (get-config 'pubmote-enabled) {
                ;; Station mode and disconnected and not already flagged
                (if (and (eq (conf-get 'wifi-mode) 1) (not-eq (wifi-status) 'connected) (not channel-locked)) {
                    (setq channel-locked 1)
                    (wifi-auto-reconnect false)
                    (wifi-disconnect)
                    (esp-now-start)
                })
                ;; Station mode and disconnected and already flagged and more than 5 seconds since last pubmote rx
                (if (and (eq (conf-get 'wifi-mode) 1) channel-locked (> (secs-since pubmote-last-activity-time) 5)) {
                    (setq channel-locked 0)
                    (wifi-auto-reconnect true)
                    (wifi-connect)
                    (esp-now-start)
                })
                (setq loop-start-time (secs-since 0))
                (if pubmote-exit-flag {
                    (break)
                })
                (if (and (> (secs-since pubmote-pairing-timer) 30 ) (>= pairing-state 1)) {
                    (pair-pubmote -2)
                }) ;timeout pairing process after 30 seconds
                (if (eq pairing-state 1) {
                    (var pairing-data (bufcreate 6))
                    (var local-mac (get-mac-addr))
                    (looprange i 0 (buflen pairing-data) {
                        (bufset-u8 pairing-data i (ix local-mac i))
                    })
                    ;(bufset-u8 data 0 69)
                    ;(print "sending pairing info")
                    (esp-now-send uni-mac pairing-data)
                    (free pairing-data)
                })
                (if (and (eq pairing-state 0) (!= (get-config 'esp-now-remote-mac-a) -1) (>= (get-config 'can-id) 0)) {
                    (bufset-u8 data 0 69) ; Mode
                    (bufset-u8 data 1 fault-code)
                    (bufset-i16 data 2 (floor (* pitch-angle 10)))
                    (bufset-i16 data 4 (floor (* roll-angle 10)))
                    (bufset-u8 data 6 state)
                    (bufset-u8 data 7 switch-state)
                    (bufset-i16 data 8 (floor (* vin 10)))
                    (bufset-i16 data 10 (floor rpm))
                    (bufset-i16 data 12 (floor (* speed 10)))
                    (bufset-i16 data 14 (floor (* tot-current 10)))
                    (bufset-u8 data 16 (floor (* (+ (abs duty-cycle-now) 0.5) 100)))
                    (bufset-f32 data 17 distance-abs 'little-endian)
                    (bufset-u8 data 21 (floor (* fet-temp-filtered 2)))
                    (bufset-u8 data 22 (floor (* motor-temp-filtered 2)))
                    (bufset-u32 data 23 odometer)
                    (bufset-u8 data 27 (floor (* battery-percent-remaining 2)))
                    (bufset-i32 data 28 (get-config 'esp-now-secret-code))
                    (esp-now-send esp-now-remote-mac data)
                })
                (setq loop-end-time (secs-since 0))
                (var actual-loop-time (- loop-end-time loop-start-time))

                (var time-to-wait (- next-run-time (secs-since 0)))

                (if (> time-to-wait 0) {
                    (yield (* time-to-wait 1000000))
                }{
                    (setq next-run-time (secs-since 0))
                })
                (setq next-run-time (+ next-run-time pubmote-loop-delay-sec))
            })
        })
        (free data)
        (setq pubmote-exit-flag nil)
    })
})

(defun pubmote-rx (src des data rssi) {
    (if (get-config 'pubmote-enabled){
        (if (and (eq pairing-state 0) (eq esp-now-remote-mac src) (eq (buflen data) 16) (eq (bufget-i32 data 0 'little-endian) (get-config 'esp-now-secret-code))) {
            (atomic {
                (setq pubmote-last-activity-time (systime))
                ;(print (list "Received" src des data rssi))
                (var jsy (bufget-f32 data 4 'little-endian))
                (var jsx (bufget-f32 data 8 'little-endian))
                (var bt-c (bufget-u8 data 12))
                (var bt-z (bufget-u8 data 13))
                (var is-rev (bufget-u8 data 14))
                ;(print (list jsy jsx bt-c bt-z is-rev))
                ;(rcode-run-noret (get-config 'can-id) `(set-remote-state ,jsy ,jsx ,bt-c ,bt-z ,is-rev))
                (if (>= (get-config 'can-id) 0) {
                    (can-cmd (get-config 'can-id) (str-replace (to-str(list jsy jsx bt-c bt-z is-rev)) "(" "(set-remote-state "))
                })
            })
        }{
            (if (eq pairing-state 1) {
                (setq esp-now-remote-mac src)
                (esp-now-add-peer esp-now-remote-mac)
                (var tmpbuf (bufcreate 4))
                (bufset-i32 tmpbuf 0 (get-config 'esp-now-secret-code))
                (esp-now-send esp-now-remote-mac tmpbuf)
                (free tmpbuf)
                (esp-now-del-peer esp-now-remote-mac)
            })
        })
    })
})
@const-end