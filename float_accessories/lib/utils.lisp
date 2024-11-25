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

(defun max (a b) (if (> a b) a b))
(defun min (a b) (if (< a b) a b))

(defun print-hex (data) (print (map (fn (x) (bufget-u8 data x)) (range (buflen data)))))

(defun event-handler ()
    (loopwhile t
        (recv
            ((event-esp-now-rx (? src) (? des) (? data) (? rssi)) (pubmote-rx src des data rssi))
            ((event-data-rx . (? data)) (float-command-rx data))
            (_ nil)
        )
    )
)

(defun send-msg (text)
    (send-data (str-merge "msg " text))
)

(defun send-status (text)
    (send-data (str-merge "status " text))
)

(defun mklist (len val) (map (fn (x) val) (range len)))

(defun split-list (lst n)
  (if (eq lst nil)
      nil
      (cons (take lst n)
            (split-list (drop lst n) n))))

(defunret pack-bytes-to-uint32 (byte-list) {
  (return (to-u32 (+ (shl (to-u32 (ix byte-list 0)) 24)
                     (shl (to-u32 (ix byte-list 1)) 16)
                     (shl (to-u32 (ix byte-list 2)) 8)
                     (to-u32 (ix byte-list 3)))))
})
(defunret unpack-uint32-to-bytes (packed-value) {
  (return (list (to-byte (shr packed-value 24))
                (to-byte (shr (bitwise-and packed-value 0xFF0000) 16))
                (to-byte (shr (bitwise-and packed-value 0xFF00) 8))
                (to-byte (bitwise-and packed-value 0xFF))))
})

(defun swap-rg (color-list) {
    (looprange led-index 0 (length color-list) {
        (var color (color-split (ix color-list led-index) 1))
        (var new-color (color-make (ix color 1) (ix color 0) (ix color 2) (ix color 3)))
        (setix color-list led-index new-color)
    })
})