;@const-symbol-strings

@const-start
(def wifi-enabled-on-boot nil)
(def pubmote-loop-delay)  ; Loop delay in microseconds (100ms)
(def pairing-state 0)
(def esp-now-remote-mac '())
(def pubmote-pairing-timer 31)
(defunret init-pubmote () {
    (if (not wifi-enabled-on-boot){
        (send-msg "WiFi was disabled on boot. Please enable and reboot to use pubmote.")
        (return false)
    })
    ;(wifi-set-chan (wifi-get-chan))
    (setq esp-now-remote-mac (append (unpack-uint32-to-bytes (get-config 'esp-now-remote-mac-a)) (take (unpack-uint32-to-bytes (get-config 'esp-now-remote-mac-b)) 2)));
    (esp-now-start)
    (esp-now-del-peer esp-now-remote-mac)
    (esp-now-add-peer esp-now-remote-mac)
    ;(print (list "starting" (get-mac-addr) (wifi-get-chan)))
    ;(print esp-now-remote-mac)
    (return true)
})
(defunret pair-pubmote (pairing) {
    (if (= (conf-get 'wifi-mode) 0) {
        (send-msg "WiFi is disabled. Please enable and reboot.")
        (return false)
    })
    (cond
        ((>= pairing 0) {
            (set-config 'esp-now-secret-code (to-i32 pairing))
            (setq pubmote-pairing-timer (systime))
            (setq pairing-state 1)
        })
        ((= pairing -1) { ;paring accepted
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
        ((= pairing -2) { ;paring rejected
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
        (var next-run-time (secs-since 0))  ; Set first run time
        (var loop-start-time 0)
        (var loop-end-time 0)
        (var pubmote-loop-delay-sec (/ 1.0 pubmote-loop-delay))
        (var data (bufcreate 32))
        (loopwhile t {
            (if (get-config 'pubmote-enabled){
                (setq loop-start-time  (secs-since 0))
                (if pubmote-exit-flag {
                    (break)
                })
                (if (and (> (secs-since pubmote-pairing-timer) 30 ) (>= pairing-state 2)){
                    (pair-pubmote -2)
                }) ;timeout pairing process after 30 seconds
                (if (= pairing-state 1){
                    (esp-now-del-peer esp-now-remote-mac)
                    (setq esp-now-remote-mac '(255 255 255 255 255 255))
                    (esp-now-add-peer esp-now-remote-mac)
                    (var pairing-data (bufcreate 6))
                    (var local-mac (get-mac-addr))
                    (looprange i 0 (buflen pairing-data) {
                        (bufset-u8 pairing-data i (ix local-mac i))
                    })
                    ;(bufset-u8 data 0 69)
                    (esp-now-send esp-now-remote-mac pairing-data) ;TODO client side
                    (free pairing-data)
                    (esp-now-del-peer esp-now-remote-mac)
                    (setq pairing-state 2)
                })
                ;(if (and (= pairing-state 0) (!= (get-config 'esp-now-remote-mac-a) -1) (>= (get-config 'can-id) 0)){
                    (if (and (= pairing-state 0) (!= (get-config 'esp-now-remote-mac-a) -1)){
                    (bufset-u8 data 0 69) ; Mode
                    (bufset-u8 data 1 fault-code)
                    (bufset-i16 data 2 (floor (* pitch-angle 10)))
                    (bufset-i16 data 4 (floor (* roll-angle 10)))
                    (bufset-u8 data 6 state)
                    (bufset-u8 data 7 switch-state)
                    (bufset-i16 data 8 (floor (* input-voltage-filtered 10)))
                    (bufset-i16 data 10 (floor rpm))
                    (bufset-i16 data 12 (floor (* speed 10)))
                    (bufset-i16 data 14 (floor (* tot-current 10)))
                    (bufset-u8 data 16 (floor (* (+ duty-cycle-now 0.5) 100)))
                    (bufset-f32 data 17 distance-abs 'little-endian)
                    (bufset-u8 data 21 (floor (* fet-temp-filtered 2)))
                    (bufset-u8 data 22 (floor (* motor-temp-filtered 2)))
                    (bufset-u32 data 23 odometer)
                    (bufset-u8 data 27 (floor (* battery-percent-remaining 2)))
                    (bufset-i32 data 28 (get-config 'esp-now-secret-code)) ;TODO client side buffers changed
                    (esp-now-send esp-now-remote-mac data)
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
                }{
                    ; If running behind, sync next-run-time with current time to avoid drift
                    (setq next-run-time (secs-since 0))
                })

                ; Update next-run-time for the next iteration
                (setq next-run-time (+ next-run-time pubmote-loop-delay-sec))

                ; Log actual loop time for debugging
                ;(print (str-merge "Actual loop time: " (str-from-n actual-loop-time "%.3f") " s"))
            })
        })
        (free data)
        (setq pubmote-exit-flag nil)
    })
})

(defun pubmote-rx (src des data rssi) {
    (if (get-config 'pubmote-enabled){
        (if (= pairing-state 2) {
            ;(print (get-mac-addr))
            ;(print src)
            ;(print (get-config 'esp-now-secret-code))
            (setq esp-now-remote-mac src)
            (esp-now-add-peer esp-now-remote-mac)
            (var tmpbuf (bufcreate 4))
            (bufset-i32 tmpbuf 0 (get-config 'esp-now-secret-code))
            (esp-now-send esp-now-remote-mac tmpbuf) ;TODO client side
            (free tmpbuf)
            (esp-now-del-peer esp-now-remote-mac)
            ;(setq pairing-state 0)
            ;(pair-pubmote -1)
        }{
            (if (and (= pairing-state 0) (= (buflen data) 16) (= (bufget-i32 data 0 'little-endian) (get-config 'esp-now-secret-code))) { ;TODO client side buffers changed
                (setq pubmote-last-activity-time (systime))
                ;(print (list "Received" src des data rssi))
                (var jsx (bufget-f32 data 4 'little-endian))
                (var jsy (bufget-f32 data 8 'little-endian))
                (var bt-c (bufget-u8 data 12))
                (var bt-z (bufget-u8 data 13))
                (var is-rev (bufget-u8 data 14))
                (print (list jsy jsx bt-c bt-z is-rev))
                ;(rcode-run-noret (get-config 'can-id) `(set-remote-state ,jsy ,jsx ,bt-c ,bt-z ,is-rev))
                (if (>= (get-config 'can-id) 0) {
                    (can-cmd (get-config 'can-id) (str-replace (to-str(list jsy jsx bt-c bt-z is-rev)) "(" "(set-remote-state "))
                })
            })
        })
    })
})
@const-end