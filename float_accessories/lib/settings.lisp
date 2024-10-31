;@const-symbol-strings

; Settings version
(def config-version 437i32)
; Persistent settings

; Format: (label . (offset type default-value current-value))
(def eeprom-addrs '(
    (ver-code                  . (0  i config-version -1))
    (crc                       . (1 i 27217 -1))
    (can-id                    . (2  i -1 -1))  ; if can-id < 0 then it will scan for one and pick the first.
    (accept-tos                . (3 b 0 -1))
    (led-enabled               . (4 b 1 -1))
    (bms-enabled               . (5 b 0 -1));TODO
    (pubmote-enabled           . (6 b 0 -1));TODO
    (led-on                    . (7  b 1 -1))
    (led-highbeam-on           . (8  b 1 -1))
    (led-mode                  . (9  i 0 -1))
    (led-mode-idle             . (10  i 5 -1))
    (led-mode-status           . (11 i 0 -1))
    (led-mode-startup          . (12 i 5 -1))
    (led-mode-button           . (13 i 0 -1))
    (led-mode-footpad          . (14 i 0 -1)) ;TODO
    (led-mall-grab-enabled     . (15 b 1 -1))
    (led-brake-light-enabled   . (16 b 0 -1))
    (led-brake-light-min-amps  . (17 f -4.0 -1))
    (idle-timeout              . (18 i 1 -1))
    (idle-timeout-shutoff      . (19 i 600 -1))
    (led-brightness            . (20 f 0.6 -1))
    (led-brightness-highbeam   . (21 f 0.5 -1))
    (led-brightness-idle       . (22 f 0.3 -1))
    (led-brightness-status     . (23 f 0.6 -1))
    (led-status-pin            . (24 i 7 -1))
    (led-status-num            . (25 i 10 -1))
    (led-status-type           . (26 i 0 -1))
    (led-status-reversed       . (27 b 0 -1))
    (led-front-pin             . (28 i 8 -1))
    (led-front-num             . (29 i 18 -1))
    (led-front-type            . (30 i 0 -1))
    (led-front-reversed        . (31 b 0 -1))
    (led-front-strip-type      . (32 b 2 -1))
    (led-rear-pin              . (33 i 9 -1))
    (led-rear-num              . (34 i 18 -1))
    (led-rear-type             . (35 i 0 -1))
    (led-rear-reversed         . (36 b 0 -1))
    (led-rear-strip-type       . (37 b 2 -1))
    (led-button-pin            . (38 b -1 -1))
    (led-button-strip-type     . (39 b 0 -1))
    (led-footpad-pin           . (40 i -1 -1)) ;TODO
    (led-footpad-num           . (41 i 0 -1)) ;TODO
    (led-footpad-type          . (42 i 0 -1));TODO
    (led-footpad-reversed      . (43 b 0 -1));TODO
    (led-footpad-strip-type    . (44 b 0 -1));TODO
    (esp-now-remote-mac-a      . (45 i -1 -1))
    (esp-now-remote-mac-b      . (46 i -1 -1))
    (esp-now-secret-code       . (47 i -1 -1))
    (bms-rs485-di-pin          . (48 i -1 -1))
    (bms-rs485-do-pin          . (49 i -1 -1));TODO add new stuff
    (bms-rs485-dere-pin        . (50 i -1 -1));TODO add new stuff
    (bms-wakeup-pin            . (51 i -1 -1))
    (bms-override-soc          . (52 i 0 -1))
    (bms-rs485-chip            . (53 b 0 -1))
    (bms-key-a                 . (54 i -1 -1))
    (bms-key-b                 . (55 i -1 -1))
    (bms-key-c                 . (56 i -1 -1))
    (bms-key-d                 . (57 i -1 -1))
    (bms-counter-a             . (58 i -1 -1))
    (bms-counter-b             . (59 i -1 -1))
    (bms-counter-c             . (60 i -1 -1))
    (bms-counter-d             . (61 i -1 -1))
    (led-loop-delay            . (62 i 20 -1))
    (bms-loop-delay            . (63 i 20 -1))
    (pubmote-loop-delay        . (64 i 20 -1))
    (can-loop-delay            . (65 i 20 -1))
    (led-max-blend-count       . (66 i 4 -1))
    (led-startup-timeout       . (67 i 20 -1))
    (led-dim-on-highbeam-ratio . (68 f 0.0 -1))
    (bms-type                  . (69 i 0 -1))
    (led-status-strip-type     . (70 i 1 -1))
    (bms-charge-only           . (71 b 0 -1))
    (led-fix                   . (72 i 1 -1))
    (led-show-battery-charging . (73 b 0 -1))
    (vin-chatter-threshold     . (74 i 20 -1))
    (led-front-headlight-pin   . (75 i -1 -1))
    (led-rear-headlight-pin    . (76 i -1 -1))
))

;(move-to-flash eeprom-addrs)
@const-start
(def bms-context-id -1)
(def bms-exit-flag nil)
(def bms-last-activity-time (systime))
(def pubmote-context-id -1)
(def pubmote-exit-flag nil)
(def pubmote-last-activity-time (systime))
(def led-context-id -1)
(def led-exit-flag nil)
(def led-last-activity-time (systime))
(def can-context-id -1)
(def can-last-activity-time (systime))

(defun recv-led-control (in-led-on in-led-highbeam-on in-led-brightness in-led-brightness-highbeam in-led-brightness-idle in-led-brightness-status) {
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
})

(defun send-led-control (){
  (var config-string "led-control ")
  (setq config-string (str-merge config-string (str-from-n (to-i led-on) "%d ") (str-from-n (to-i led-highbeam-on) "%d ") (str-from-n led-brightness "%.2f ") (str-from-n led-brightness-highbeam "%.2f ") (str-from-n led-brightness-idle "%.2f ") (str-from-n led-brightness-status "%.2f ")))
  (send-data config-string)
})

;to safely restart things we send exit commands and then check pins for validity and then respawn the process if it's enabled.
(defun recv-config (in-led-enabled in-bms-enabled in-pubmote-enabled in-led-on in-led-highbeam-on in-led-mode in-led-mode-idle in-led-mode-status in-led-mode-startup in-led-mode-button in-led-mode-footpad in-led-mall-grab-enabled
                    in-led-brake-light-enabled in-led-brake-light-min-amps in-idle-timeout in-idle-timeout-shutoff in-led-brightness in-led-brightness-highbeam in-led-brightness-idle in-led-brightness-status in-led-status-pin in-led-status-num
                    in-led-status-type in-led-status-reversed in-led-front-pin in-led-front-num in-led-front-type in-led-front-reversed in-led-front-strip-type
                    in-led-rear-pin in-led-rear-num in-led-rear-type in-led-rear-reversed in-led-rear-strip-type in-led-button-pin in-led-button-strip-type in-led-footpad-pin in-led-footpad-num in-led-footpad-type in-led-footpad-reversed
                    in-led-footpad-strip-type in-bms-rs485-di-pin in-bms-rs485-do-pin in-bms-rs485-dere-pin in-bms-wakeup-pin in-bms-override-soc in-bms-rs485-chip in-led-loop-delay in-bms-loop-delay in-pubmote-loop-delay in-can-loop-delay in-led-max-blend-count in-led-startup-timeout in-led-dim-on-highbeam-ratio in-bms-type in-led-status-strip-type in-bms-charge-only in-led-fix in-led-show-battery-charging in-vin-chatter-threshold in-led-front-headlight-pin in-led-rear-headlight-pin) {
    (if (>= led-context-id 0){
    (let ((start-time (systime))
        (timeout 100000))  ; Timeout in milliseconds (1 seconds)
    (setq led-exit-flag t)
    (while (and led-exit-flag
              (< (- (systime) start-time) timeout))
        (yield 10000))

    ; Check if we exited due to timeout
    (if (>= (- (systime) start-time) timeout)
    (setq led-exit-flag nil)))
})
    (atomic {
    (set-config 'led-enabled (to-i in-led-enabled))
    (set-config 'bms-enabled (to-i in-bms-enabled))
    (set-config 'pubmote-enabled (to-i in-pubmote-enabled));TODO
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
    (var bms-rs485-do-pin-prev (get-config 'bms-rs485-do-pin))
    (var bms-rs485-dere-pin-prev (get-config 'bms-rs485-dere-pin))
    (var bms-wakeup-pin-prev (get-config 'bms-wakeup-pin))

    (set-config 'bms-rs485-di-pin (to-i in-bms-rs485-di-pin))
    (set-config 'bms-rs485-do-pin (to-i in-bms-rs485-do-pin))
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
    (set-config 'vin-chatter-threshold (to-i in-vin-chatter-threshold))
    (set-config 'led-front-headlight-pin (to-i in-led-front-headlight-pin))
    (set-config 'led-rear-headlight-pin (to-i in-led-rear-headlight-pin))
    (if (or (!= (to-i in-bms-rs485-di-pin) bms-rs485-di-pin-prev) (!= (to-i in-bms-rs485-do-pin) bms-rs485-do-pin-prev) (!= (to-i in-bms-rs485-dere-pin) bms-rs485-dere-pin-prev) (!= (to-i in-bms-wakeup-pin) bms-wakeup-pin-prev) ) {
        ;(if (init-bms) )
        ;Todo deal with validating and resetting bms pins

    })
   })
    (if (>= in-led-front-pin 0) {
        (if (not-eq (first (trap (rgbled-init in-led-front-pin in-led-front-type))) 'exit-ok) {
            (send-msg "Invalid Pin: led-front-pin")
        }{
            (set-config 'led-front-pin (to-i in-led-front-pin))
        })
    }{
        (set-config 'led-front-pin -1)
    })
    (if (>= in-led-rear-pin 0) {
        (if (not-eq (first (trap (rgbled-init in-led-rear-pin in-led-rear-type))) 'exit-ok) {
            (send-msg "Invalid Pin: led-rear-pin")
        }{
            (set-config 'led-rear-pin (to-i in-led-rear-pin))
        })
    }{
        (set-config 'led-rear-pin -1)
    })
    (if (>= in-led-status-pin 0) {
        (if (not-eq (first (trap (rgbled-init in-led-status-pin in-led-status-type))) 'exit-ok) {
            (send-msg "Invalid Pin: led-status-pin")
        }{
            (set-config 'led-status-pin (to-i in-led-status-pin))
        })
    }{
        (set-config 'led-status-pin -1)
    })
    (if (>= in-led-button-pin 0) {
        (if (not-eq (first (trap (rgbled-init in-led-button-pin 0))) 'exit-ok) {
            (send-msg "Invalid Pin: led-button-pin")
        }{
            (set-config 'led-button-pin (to-i in-led-button-pin))
        })
    }{
        (set-config 'led-button-pin -1)
    })
    (if (>= in-led-footpad-pin 0) {
        (if (not-eq (first (trap (rgbled-init in-led-footpad-pin in-led-footpad-type))) 'exit-ok) {
            (send-msg "Invalid Pin: led-footpad-pin")
        }{
            (set-config 'led-footpad-pin (to-i in-led-footpad-pin))
        })
    }{
        (set-config 'led-footpad-pin -1)
    })
    (rgbled-deinit)
    (setq led-context-id (if (= (get-config 'led-enabled) 1) (spawn led-loop) -1))
    ;(setq bms-context-id (if (= (get-config 'bms-enabled) 1) (spawn bms-loop) -1))

    (if (= in-pubmote-enabled 1) {
        (if (= pubmote-context-id -1) (setq pubmote-context-id (spawn pubmote-loop)))
    })
})

(defun send-keys (key-list counter-list){

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

(defun accept-tos(){
(atomic {
    (set-config 'accept-tos 1)
    (write-val-eeprom 'accept-tos 1)
    (write-val-eeprom 'crc (config-crc))
    })
})

(defun set-config (name value) {
    (let ((pair (assoc eeprom-addrs name)))
        (if pair
            (loopforeach item eeprom-addrs {
                (if (eq (car item) name)
                    (setcdr item (list (car (cdr item))
                        (car (cdr (cdr item)))
                        (car (cdr (cdr (cdr item))))
                        value))
                )
            })
            (setq eeprom-addrs (cons (cons name (list 0 'type 0 value)) eeprom-addrs))
        )
    )
})

(defun send-config () {
(atomic {
  (var config-string "settings ")
  (loopforeach setting eeprom-addrs {
    (let ((name (first setting))
          (type (third setting))) {
      (var value (read-val-eeprom name))
      (setq config-string (str-merge config-string
        (cond
          ((eq type 'b) (str-from-n value "%d "))
          ((eq type 'i) (str-from-n value "%d "))
          ((eq type 'f) (str-from-n value "%.2f ")))))
    })
  })
  (send-data config-string)
  (send-status "Settings Read!")
  })
})

; Helper functions to get and set variable values
(defunret get-config (name) {
    (var pair (assoc eeprom-addrs name))
    (if pair
        (return (car (cdr (cdr (cdr pair)))))
        (return nil)
    )
})

(defun save-config () {
(atomic {
    (loopforeach setting eeprom-addrs {
        (var name (first setting))
        ;(print (get-config name))
        (if (not-eq name 'crc) {
            (write-val-eeprom name (get-config name))
        })
    })
    (write-val-eeprom 'crc (config-crc))
    (send-status "Settings Saved!")
    })
})

(defunret config-crc () {
    (var i 0)
	(var crclen (* (- (length eeprom-addrs) 1) 4))
	(var crcbuf (bufcreate crclen))
	(loopforeach setting eeprom-addrs {
            (var name (first setting))
            (if (not-eq name 'crc) {
                (bufset-i32 crcbuf (* i 4) (get-config name))
                ;(print name)
                ;(print i)
                ;(print (get-config name))
                ;(print (bufget-i32 crcbuf (* i 4)))
                (setq i (+ i 1))
            })
	})
    (var crc (crc16 crcbuf))
    ;(print (buflen crcbuf))
    ;(print crc)
    (free crcbuf)
    (return crc)
})

(defun load-config () {
    (loopforeach setting eeprom-addrs {
        (var name (first setting))
        (var val (read-val-eeprom name))
        ;(print val)
        (set-config name val)
    })
})

(defun restore-config () {
(atomic {
    (loopforeach setting eeprom-addrs {
        (var name (first setting))
        (var default-value (if (eq name 'ver-code) config-version (ix setting 3)))
        (write-val-eeprom name default-value)
    })
	(load-config)
	(send-status "Settings Restored!")
})
})

(defun print-config ()
    (loopforeach it eeprom-addrs
        (print (list (first it) (read-val-eeprom (first it))))
))

(defun read-val-eeprom (name)
    (let (
        (addr (first (assoc eeprom-addrs name)))
        (type (second (assoc eeprom-addrs name)))
    )
    (cond
        ((eq type 'i) (eeprom-read-i addr))
        ((eq type 'f) (eeprom-read-f addr))
        ((eq type 'b) (eeprom-read-i addr))
)))

(defun write-val-eeprom (name val)
    (let (
        (addr (first (assoc eeprom-addrs name)))
        (type (second (assoc eeprom-addrs name)))
    )
    (cond
        ((eq type 'i) (eeprom-store-i addr val))
        ((eq type 'f) (eeprom-store-f addr val))
        ((eq type 'b) (eeprom-store-i addr val))
)))
(defun status () {
    (var status-string "float-stats ")
    (setq status-string (str-merge status-string (str-from-n (if (< (secs-since can-last-activity-time) 1) 1 0) "%d ")))
    (setq status-string (str-merge status-string (str-from-n (if (< (secs-since pubmote-last-activity-time) 1) 1 0) "%d ")))
    (setq status-string (str-merge status-string (str-from-n (if (< (secs-since bms-last-activity-time) 1) 1 0) "%d ")))
    (send-data status-string)
})

@const-end