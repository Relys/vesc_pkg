@const-start
;@const-symbol-strings
(def pubmote-loop-delay 8)  ; Loop delay in microseconds (100ms)
(def pairing-state 0)
(def esp-now-remote-mac '())
(def pubmote-pairing-timer 31)
(def uni-mac '(255 255 255 255 255 255)) ; Universal mac (all devices)
(def channel-locked 0)
(def channel-locked 0)
(def channel-locked-timeout 10) ; How many seconds of no activity to wait before unlocking locked wifi channel
(def pubmote-version-major 0)
(def pubmote-version-minor 0)
(def pubmote-version-patch 0)
(def rem-cmds '(
    ; Remote version commands
    (REM_VERSION . 0)
    ; Receiver version commands
    (REM_VERSION_REC. 5)
    ; Bonding commands
    (REM_PAIR_INIT . 10)
    (REM_PAIR_BOND . 11)
    (REM_PAIR_COMPLETE . 12)
    ; Remote specific commands
    (REM_SET_CORE_DATA . 100)
    ; Receiver specific commands
    (REM_SET_INPUT_STATE . 150)
))
(defunret init-pubmote () {
    ; Escape without wifi
    (if (not wifi-enabled-on-boot) {
        (send-msg "WiFi was disabled on boot. Please enable and reboot to use Pubmote.")
        (return false)
    })

    ;TODO poll cfg from C code
    (var hi-u32 (to-u32 (ext-cfg 1)))
    (var lo-u32 (to-u32 (ext-cfg 2)))
    (setq esp-now-secret-code (to-i32 (ext-cfg 3)))
    (setq pubmote-enabled (= (ext-cfg 4) 1))
    (setq esp-now-remote-mac-a (unpack-uint32-to-bytes hi-u32))
    (setq esp-now-remote-mac-b (unpack-uint32-to-bytes lo-u32))
    (setq esp-now-remote-mac (append esp-now-remote-mac-a (take esp-now-remote-mac-b 2)))
    ; Read as bytes, convert to i so we can compare lists
    (loopfor i 0 (< i (length esp-now-remote-mac)) (+ i 1) {
        (setix esp-now-remote-mac i (to-i (ix esp-now-remote-mac i)))
    })

    (esp-now-start)
    (esp-now-del-peer esp-now-remote-mac)
    (esp-now-add-peer esp-now-remote-mac)
    (esp-now-del-peer uni-mac)
    (esp-now-add-peer uni-mac)
    (return true)
})

(defun pair-pubmote-start (pairing-code) {
    (setq esp-now-secret-code (to-i32 pairing-code))
    (setq pubmote-pairing-timer (systime))
    (setq pairing-state 1)
})

(defun lock-channel (reason) {
    (print (str-merge "Channel switching disabled. Reason: " reason))
    (setq channel-locked (wifi-get-chan))
    (wifi-disconnect)
    (wifi-auto-reconnect nil)
})

(defun unlock-channel (reason) {
    (print (str-merge "Channel switching enabled. Reason: " reason))
    (setq channel-locked 0)
    (wifi-auto-reconnect true)
    (wifi-connect (conf-get `wifi-sta-ssid) (conf-get `wifi-sta-key))
})

(defun is-station-mode () {
    (eq (conf-get 'wifi-mode) 1)
})

(defun is-wifi-connected () {
    (eq (wifi-status) 'connected)
})

(defun should-lock-channel () {
    ; Channel is not locked
    ; Station mode
    ; Wifi is not connected
    (and (eq channel-locked 0) (is-station-mode) (not (is-wifi-connected)))
})

(defun should-unlock-channel (last-activity-time) {
    ; Channel is locked
    ; Station mode
    ; Last activity time is not set or more than set time passed since last rx
    (if (and (> channel-locked 0) (is-station-mode) (> (secs-since last-activity-time) channel-locked-timeout)) {
        (unlock-channel (str-from-n pubmote-last-activity-time "Last activity time greater than set time"))
    })
})

(defun should-send-message () {
    (var hi-u32  (pack-bytes-to-uint32 esp-now-remote-mac-a))
    (var lo-u32  (pack-bytes-to-uint32 esp-now-remote-mac-b))
    (and (= pairing-state 0)  (!= hi-u32 0) (!= lo-u32 0) (>= can-id 0))
})

(defun should-process-message (src data) {
    (and (= pairing-state 0) (eq esp-now-remote-mac src) (= (bufget-i32 data 1 'little-endian) esp-now-secret-code))
})

(defun reset-last-activity-time () {
    (setq pubmote-last-activity-time (systime))
})
@const-end

(def pubmote-enabled false)
(def esp-now-remote-mac-a)
(def esp-now-remote-mac-b)
(def esp-now-secret-code)
(defun pair-pubmote-accept () {
    (setq esp-now-remote-mac-a (take esp-now-remote-mac 4))
    (setq esp-now-remote-mac-b (append (drop esp-now-remote-mac 4) '(0 0)))
    (atomic {
        (ext-cfg (pack-bytes-to-uint32 esp-now-remote-mac-a) (pack-bytes-to-uint32 esp-now-remote-mac-b) esp-now-secret-code) ;Make sure this is ok
    })
    (init-pubmote)
    (var tmpbuf (bufcreate 2))
    (bufset-u8 tmpbuf 0 (assoc rem-cmds 'REM_PAIR_COMPLETE))
    (bufset-u8 tmpbuf 1 1)
    (print "Sending pairing success message")
    (esp-now-send esp-now-remote-mac tmpbuf)
    (free tmpbuf)
    (setq pairing-state 0)
})

(defun pair-pubmote-reject () {
    (setq esp-now-remote-mac-a  '(0b 0b 0b 0b))
    (setq esp-now-remote-mac-b  '(0b 0b 0b 0b))
    (atomic {
        (ext-cfg 0 0 0)
    })
    (var tmpbuf (bufcreate 2))
    (bufset-u8 tmpbuf 0 (to-byte (assoc rem-cmds 'REM_PAIRING_COMPLETE)))
    (bufset-u8 tmpbuf 1 0)
    (print "Sending pairing rejected message")
    (esp-now-send esp-now-remote-mac tmpbuf)
    (free tmpbuf)
    (setq esp-now-remote-mac '())
    (setq pairing-state 0)

    ; Unlock wifi channel hopping
    (should-unlock-channel pubmote-last-activity-time)
})

(defun pubmote-loop () {
    (if (init-pubmote) {
        (var next-run-time (secs-since 0))
        (var loop-start-time 0)
        (var loop-end-time 0)
        (var pubmote-loop-delay-sec (/ 1.0 pubmote-loop-delay))
        (var data (bufcreate 33))

        (loopwhile t {

            ;TODO poll vars from C needed to send.
            (if pubmote-enabled {
                (should-unlock-channel pubmote-last-activity-time)

                (setq loop-start-time  (secs-since 0))

                ; Timeout pairing process after 30 seconds
                (if (and (> (secs-since pubmote-pairing-timer) 30 ) (>= pairing-state 1)) {
                    (pair-pubmote-reject)
                })

                (if (= pairing-state 1) {
                    ; Update last activity time for pairing duration
                    (print "Set last activity time from pubmote-loop: Pairing search")
                    (setq pubmote-last-activity-time (systime))

                    (if (should-lock-channel) {
                        (lock-channel "Begin pairing")
                    })

                    (var pairing-data (bufcreate 7))

                    (bufset-u8 pairing-data 0 (to-byte (assoc rem-cmds 'REM_PAIR_INIT)))
                    (var local-mac (get-mac-addr))

                    (looprange i 0 (- (buflen pairing-data) 1) {
                        (bufset-u8 pairing-data (+ i 1) (ix local-mac i))
                    })

                    ; (bufset-u8 data 0 69)
                    ; (print "Sending pairing mac address")
                    (esp-now-send uni-mac pairing-data)
                    (free pairing-data)
                })

                ; Bond in progress
                (if (= pairing-state 2) {
                    ; Update last activity time for pairing duration
                    (print "Set last activity time from pubmote-loop: Bond in progress")
                    (setq pubmote-last-activity-time (systime))
                })
                (if (should-send-message) {
                    (bufset-u8 data 0 (to-byte (assoc rem-cmds 'REM_SET_CORE_DATA)))
                    (bufset-i32 data 1 esp-now-secret-code)
                    (bufset-u8 data 5 fault-code)
                    (bufset-i16 data 6 (floor (* pitch-angle 10)))
                    (bufset-i16 data 8 (floor (* roll-angle 10)))
                    (bufset-u8 data 10 state)
                    (bufset-u8 data 11 switch-state)
                    (bufset-i16 data 12 (floor (* vin 10)))
                    (bufset-i16 data 14 (floor rpm))
                    (bufset-i16 data 16 (floor (* speed 10)))
                    (bufset-i16 data 18 (floor (* tot-current 10)))
                    (bufset-u8 data 20 (floor (* (+ (abs duty-cycle-now) 0.5) 100)))
                    (bufset-f32 data 21 distance-abs 'little-endian)
                    (bufset-u8 data 25 (floor (* fet-temp-filtered 2)))
                    (bufset-u8 data 26 (floor (* motor-temp-filtered 2)))
                    (bufset-u32 data 27 odometer)
                    (bufset-u8 data 31 (floor (* battery-percent-remaining 200)))
                    ; (print "Sending board state to remote")
                    (esp-now-send esp-now-remote-mac data)
                })
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
        (free data)
    })
})

(defun pubmote-rx (src des data rssi) {
    (if (and pubmote-enabled wifi-enabled-on-boot) {
        (if (should-lock-channel) {
            ; Update last activity time in case it does not establish a connection
            (setq pubmote-last-activity-time (systime))

            (lock-channel "ESP-NOW packet received")
        })

        (var cmd (bufget-u8 data 0))

        (match (cossa rem-cmds cmd)
            (REM_VERSION {
                (if (and (should-process-message src data) (= (buflen data) 8)) {
                    (reset-last-activity-time)

                    (setq pubmote-version-major (bufget-u8 data 5))
                    (setq pubmote-version-minor (bufget-u8 data 6))
                    (setq pubmote-version-patch (bufget-u8 data 7))
                    (print (str-merge "Remote version: " (to-str pubmote-version-major) "." (to-str pubmote-version-minor) "." (to-str pubmote-version-patch)))
                })

            })

            (REM_VERSION_REC {
                (if (should-process-message src data) {
                    (reset-last-activity-time)

                    (var tmpbuf (bufcreate 8))
                    (bufset-u8 tmpbuf 0 (to-byte (assoc rem-cmds 'REM_VERSION_REC)))
                    (bufset-i32 tmpbuf 1 esp-now-secret-code)

                    (var version (get-version))
                    (bufset-u8 tmpbuf 5 (first version))
                    (bufset-u8 tmpbuf 6 (second version))
                    (bufset-u8 tmpbuf 7 (third version))

                    (esp-now-send esp-now-remote-mac tmpbuf)
                    (free tmpbuf)
                })
            })

            ; Bonding command
            (REM_PAIR_BOND {
                (if (= pairing-state 1) {
                    ; Add the peer and save
                    (setq esp-now-remote-mac src)
                    (esp-now-add-peer esp-now-remote-mac)
                    (var tmpbuf (bufcreate 5))
                    (bufset-u8 tmpbuf 0 (to-byte (assoc rem-cmds 'REM_PAIR_BOND)))
                    (bufset-i32 tmpbuf 1 esp-now-secret-code)
                    ; Send pairing code
                    (print "Responding with pairing code")
                    (esp-now-send esp-now-remote-mac tmpbuf)
                    (free tmpbuf)
                    (esp-now-del-peer esp-now-remote-mac)

                    ; Set pairing state to bonding
                    (setq pairing-state 2)
                })
            })

            (REM_SET_INPUT_STATE {
                ; Remote is paired and data was received
                (if (and (should-process-message src data) (= (buflen data) 17)) {
                    (reset-last-activity-time)

                    ;(print (list "Received" src des data rssi))
                    (var jsy (bufget-f32 data 5 'little-endian))
                    (var jsx (bufget-f32 data 9 'little-endian))
                    (var bt-c (bufget-u8 data 13))
                    (var bt-z (bufget-u8 data 14))
                    (var is-rev (bufget-u8 data 15))
                    ; (print (list jsy jsx bt-c bt-z is-rev))
                    ; (rcode-run-noret can-id `(set-remote-state ,jsy ,jsx ,bt-c ,bt-z ,is-rev))

                    (if (and (>= can-id 0) (!= state 15)) {;Also check if refloat is enabled before sending this to run on vesc...TODO Might look into sending raw can frame instead of lisp too
                        (can-cmd can-id (str-replace (to-str(list jsy jsx bt-c bt-z is-rev)) "(" "(set-remote-state "))
                    })
                } {
                   (print "Conditions not met for set remote state")
                })
            })

            ; No matching command
            (_ {
            (print (str-join (list "No command found: " (to-str cmd))))
            })
        )
    })
})