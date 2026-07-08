;@const-symbol-strings

@const-start

(def pubmote-loop-delay)  ; Loop delay in microseconds (100ms)
(def pairing-state 0)
(def esp-now-remote-mac '())
(def pubmote-pairing-timer 31)
(def pubmote-pairing-timer-timeout 30) ; How many seconds to wait before aborting pairing
(def uni-mac '(255 255 255 255 255 255)) ; Universal mac (all devices)
(def channel-locked 0)
(def channel-locked-timeout 10) ; How many seconds of no activity to wait before unlocking locked wifi channel
(def pubmote-version-major 0)
(def pubmote-version-minor 0)
(def pubmote-version-patch 0)

(def jsx-btn-seen-zero nil)
(def jsx-btn-prev nil)
(def jsx-click-count 0)
(def jsx-last-release-time (systime))
(def btn-horn-prev 0)
(def bt-z-beep-start (systime))
(def bt-z-beep-fired nil)

(defunret mac-match (a b) {
    (if (!= (length a) (length b)) (return nil))
    (looprange i 0 (length a) {
        (if (!= (ix a i) (ix b i)) (return nil))
    })
    (return t)
})

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

    (setq esp-now-remote-mac (append (unpack-uint32-to-bytes (get-config 'esp-now-remote-mac-a)) (take (unpack-uint32-to-bytes (get-config 'esp-now-remote-mac-b)) 2)))

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

        ; Pairing accepted
        ((= pairing -1) {
            (set-config 'esp-now-remote-mac-a (pack-bytes-to-uint32 (take esp-now-remote-mac 4)))
            (set-config 'esp-now-remote-mac-b (pack-bytes-to-uint32 (append (drop esp-now-remote-mac 4) '(0 0))))
            (atomic {
                (write-val-eeprom 'esp-now-remote-mac-a (get-config 'esp-now-remote-mac-a))
                (write-val-eeprom 'esp-now-remote-mac-b (get-config 'esp-now-remote-mac-b))
                (write-val-eeprom 'esp-now-secret-code (get-config 'esp-now-secret-code))
                (write-val-eeprom 'crc (config-crc cfg-len))
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

        ; Pairing rejected
        ((= pairing -2) {
            (set-config 'esp-now-remote-mac-a -1)
            (atomic {
                (write-val-eeprom 'esp-now-remote-mac-a (get-config 'esp-now-remote-mac-a) -1)
                (write-val-eeprom 'crc (config-crc cfg-len))
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
    )

    (return true)
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
    (and (= pairing-state 0) (!= (get-config 'esp-now-remote-mac-a) -1) (>= can-id 0))
})

(defun pubmote-loop () {
    (if (init-pubmote) {
        (setq pubmote-loop-delay (get-config 'pubmote-loop-delay))
        (var next-run-time (secs-since 0))
        (var loop-start-time 0)
        (var loop-end-time 0)
        (var pubmote-loop-delay-sec (/ 1.0 pubmote-loop-delay))
        (var data (bufcreate 33))

        (loopwhile t {
            (if (get-config 'pubmote-enabled) {
                ; Check last pubmote activity
                (should-unlock-channel pubmote-last-activity-time)

                (setq loop-start-time  (secs-since 0))

                ; Escape as needed
                (if pubmote-exit-flag {
                    (break)
                })

                ; Timeout pairing process after set time has passed
                (if (and (> (secs-since pubmote-pairing-timer) pubmote-pairing-timer-timeout) (>= pairing-state 1)) {
                    (pair-pubmote -2)
                })

                ; Pairing search 
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

                ; Connected, send data
                (if (should-send-message) {                
                    (bufset-u8 data 0 (to-byte (assoc rem-cmds 'REM_SET_CORE_DATA)))
                    (bufset-i32 data 1 (get-config 'esp-now-secret-code))
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

(defun should-process-message (src data) {
    (and (= pairing-state 0) (mac-match esp-now-remote-mac src) (= (bufget-i32 data 1 'little-endian) (get-config 'esp-now-secret-code)))
})

(defun reset-last-activity-time () {
    (setq pubmote-last-activity-time (systime))
})

(defun pubmote-rx (src des data rssi) {
    (if (and (get-config 'pubmote-enabled) wifi-enabled-on-boot) {
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
                    (bufset-i32 tmpbuf 1 (get-config 'esp-now-secret-code))

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
                    (bufset-i32 tmpbuf 1 (get-config 'esp-now-secret-code))

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

                    (var jsy (bufget-f32 data 5 'little-endian))
                    (var jsx (bufget-f32 data 9 'little-endian))
                    (var bt-c (bufget-u8 data 13))
                    (var bt-z (bufget-u8 data 14))
                    (var is-rev (bufget-u8 data 15))

                    (send-data (str-merge "pubmote-input "
                        (str-from-n jsy "%.3f") " "
                        (str-from-n jsx "%.3f") " "
                        (str-from-n (if jsx-btn-seen-zero (if (< jsx 0) 1 0) 0)) " "
                        (str-from-n (to-i bt-z)) " "
                        (str-from-n (to-i is-rev)) " "
                        (str-from-n blinker-state) " "
                        (str-from-n jsx-click-count) " "
                        (str-from-n horn-fire-count)))

                    ; bt_z hold > 0.8s = horn
                    (if (= bt-z 1) {
                        (if (= btn-horn-prev 0) {
                            (setq bt-z-beep-start (systime))
                            (setq bt-z-beep-fired nil)
                        })
                        (if (and (not bt-z-beep-fired) (> (secs-since bt-z-beep-start) 0.8)) {
                            (trigger-beep)
                            (setq bt-z-beep-fired t)
                        })
                    }{
                        (setq bt-z-beep-fired nil)
                    })
                    (setq btn-horn-prev bt-z)

                    ; js_x X button click counter: 1=left blinker, 2=right blinker, 3+=horn
                    ; jsx protocol: 0.0=not pressed, -1.0=pressed
                    ; jsx-btn-seen-zero prevents false triggers if remote starts with jsx<0
                    (if (>= jsx 0) (setq jsx-btn-seen-zero t))
                    (if jsx-btn-seen-zero {
                        (var btn-pressed (< jsx 0))
                        (if (and btn-pressed (not jsx-btn-prev)) {
                            (setq jsx-click-count (+ jsx-click-count 1))
                            (setq jsx-last-release-time (systime))
                        })
                        (if (and (not btn-pressed) jsx-btn-prev) {
                            (setq jsx-last-release-time (systime))
                        })
                        (setq jsx-btn-prev btn-pressed)

                        (if (and (> jsx-click-count 0) (not btn-pressed)
                                 (> (secs-since jsx-last-release-time) 0.4)) {
                            (if (= jsx-click-count 1) {
                                (set-blinker (if (= blinker-state (blinker-l)) 0 (blinker-l)))
                            } (if (= jsx-click-count 2) {
                                (set-blinker (if (= blinker-state (blinker-r)) 0 (blinker-r)))
                            } {
                                (if (>= can-id 0) (trigger-beep))
                            }))
                            (setq jsx-click-count 0)
                        })
                    })

                    (if (and (>= can-id 0) (> (secs-since horn-last-start-time) 1.0)) {
                        (can-cmd can-id (str-replace (to-str (list jsy jsx bt-c bt-z is-rev)) "(" "(set-remote-state "))
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
@const-end