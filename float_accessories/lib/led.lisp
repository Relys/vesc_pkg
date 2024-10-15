;@const-symbol-strings
;Buffers
(def led-button-buffer)
(def led-footpad-buffer)
(def led-status-buffer)
(def led-front-buffer)
(def led-rear-buffer)
(def led-combined-buffer)
@const-start

(def led-loop-delay)  ; Loop delay in microseconds (100ms)
;config vars
(def led-enabled)
(def led-on)
(def led-highbeam-on)
(def led-mode)
(def led-mode-idle)
(def led-mode-status)
(def led-mode-startup)
(def led-mode-button)
(def led-mode-footpad)
(def led-mall-grab-enabled)
(def led-brake-light-enabled)
(def led-brake-light-min-amps)
(def idle-timeout)
(def idle-timeout-shutoff)
(def led-brightness 0.0)
(def led-brightness-highbeam 0.0)
(def led-brightness-idle 0.0)
(def led-brightness-status 0.0)
(def led-status-pin)
(def led-status-num)
(def led-status-type)
(def led-status-reversed)
(def led-front-pin)
(def led-front-num)
(def led-front-type)
(def led-front-reversed)
(def led-front-strip-type)
(def led-rear-pin)
(def led-rear-num)
(def led-rear-type)
(def led-rear-reversed)
(def led-rear-strip-type)
(def led-button-pin)
(def led-button-strip-type)
(def led-footpad-pin)
(def led-footpad-num)
(def led-footpad-type)
(def led-footpad-reversed)
(def led-footpad-strip-type)

(def led-max-blend-count 0.0)  ; how many times to blend before new led buffer
(def led-startup-timeout)
(def led-dim-on-highbeam-ratio 0.0)
(def led-status-strip-type)
;runtime vars
(def led-current-brightness 0.0)
(def led-status-color '())
(def led-front-color '())
(def led-rear-color '())
(def led-button-color '())
(def led-footpad-color '())
(def next-run-time)  ; Set first run time after initial delay
(def direction)
(def led-mall-grab)
(def prev-led-front-color '())
(def prev-led-rear-color '())
(def target-led-front-color '())
(def target-led-rear-color '())
(def prev-led-button-color '())
(def target-led-button-color '())
(def combined-pins nil)
(def led-fix 1)
(def led-show-battery-charging 0)

(defun load-led-settings () {
; Setting up all variables with get-config to fetch from EEPROM
    (setq led-enabled (get-config 'led-enabled))
    (setq led-on (get-config 'led-on))
    (setq led-highbeam-on (get-config 'led-highbeam-on))
    (setq led-mode (get-config 'led-mode))
    (setq led-mode-idle (get-config 'led-mode-idle))
    (setq led-mode-status (get-config 'led-mode-status))
    (setq led-mode-startup (get-config 'led-mode-startup))
    (setq led-mode-button (get-config 'led-mode-button))
    (setq led-mode-footpad (get-config 'led-mode-footpad))
    (setq led-mall-grab-enabled (get-config 'led-mall-grab-enabled))
    (setq led-brake-light-enabled (get-config 'led-brake-light-enabled))
    (setq led-brake-light-min-amps (get-config 'led-brake-light-min-amps))
    (setq idle-timeout (get-config 'idle-timeout))
    (setq idle-timeout-shutoff (get-config 'idle-timeout-shutoff))
    (setq led-brightness (get-config 'led-brightness))
    (setq led-brightness-highbeam (get-config 'led-brightness-highbeam))
    (setq led-brightness-idle (get-config 'led-brightness-idle))
    (setq led-brightness-status (get-config 'led-brightness-status))
    (setq led-status-pin (get-config 'led-status-pin))
    (setq led-status-num (get-config 'led-status-num))
    (setq led-status-type (get-config 'led-status-type))
    (setq led-status-reversed (get-config 'led-status-reversed))
    (setq led-front-pin (get-config 'led-front-pin))
    (setq led-front-num (get-config 'led-front-num))
    (setq led-front-type (get-config 'led-front-type))
    (setq led-front-reversed (get-config 'led-front-reversed))
    (setq led-front-strip-type (get-config 'led-front-strip-type))
    (setq led-rear-pin (get-config 'led-rear-pin))
    (setq led-rear-num (get-config 'led-rear-num))
    (setq led-rear-type (get-config 'led-rear-type))
    (setq led-rear-reversed (get-config 'led-rear-reversed))
    (setq led-rear-strip-type (get-config 'led-rear-strip-type))
    (setq led-button-pin (get-config 'led-button-pin))
    (setq led-button-strip-type (get-config 'led-button-strip-type))
    (setq led-footpad-pin (get-config 'led-footpad-pin))
    (setq led-footpad-num (get-config 'led-footpad-num))
    (setq led-footpad-type (get-config 'led-footpad-type))
    (setq led-footpad-reversed (get-config 'led-footpad-reversed))
    (setq led-footpad-strip-type (get-config 'led-footpad-strip-type))
    (setq led-max-blend-count (get-config 'led-max-blend-count))
    (setq led-startup-timeout (get-config 'led-startup-timeout))
    (setq led-dim-on-highbeam-ratio (get-config 'led-dim-on-highbeam-ratio))
    (setq led-status-strip-type (get-config 'led-status-strip-type))
    (setq led-loop-delay (get-config 'led-loop-delay))
    (setq led-fix (get-config 'led-fix))
    (setq led-show-battery-charging (get-config 'led-show-battery-charging))
})

(defun init-led-vars () {
    (def blend-count led-max-blend-count)
    (setq combined-pins nil)
    (setq led-current-brightness 0.0)
    (setq led-status-color (mklist led-status-num 0))
    (setq led-front-color (mklist led-front-num 0))
    (setq led-rear-color (mklist led-rear-num 0))
    (setq led-footpad-color (mklist led-footpad-num 0))
    (setq led-button-color (mklist 1 0))
    (setq direction 1)
    (setq led-mall-grab 0)
    (setq prev-led-front-color (mklist led-front-num 0))
    (setq prev-led-rear-color (mklist led-rear-num 0))
    (setq target-led-front-color (mklist led-front-num 0))
    (setq target-led-rear-color (mklist led-rear-num 0))
    (setq prev-led-button-color (mklist 1 0))
    (setq target-led-button-color (mklist 1 0))
    (if (>= led-button-pin 0) {
        (setq led-button-buffer (rgbled-buffer 1 0))
    })
    ;Update footpad

    (var front-highbeam-leds 0)
    (var rear-highbeam-leds 0)
    (cond
        ((or (= led-front-strip-type 2) (= led-front-strip-type 3)) {
             (setq front-highbeam-leds (+ front-highbeam-leds 1))
        })
        ((or (= led-rear-strip-type 2) (= led-rear-strip-type 3)) {
             (setq rear-highbeam-leds (+ rear-highbeam-leds 1))
        })
        ((or (= led-front-strip-type 3) (= led-front-strip-type 4)) {
             (setq front-highbeam-leds (+ front-highbeam-leds 4))
        })
        ((or (= led-rear-strip-type 3) (= led-rear-strip-type 4)) {
             (setq rear-highbeam-leds (+ rear-highbeam-leds 4))
        })
    )
    (if (>= led-footpad-pin 0) {
        (setq led-footpad-buffer (rgbled-buffer led-footpad-num led-footpad-type))
    })

    (if (and (>= led-front-pin 0) (= led-status-pin led-front-pin) (= led-front-pin led-rear-pin)) {
        (var total-leds (+ led-status-num led-front-num front-highbeam-leds led-rear-num rear-highbeam-leds))
        (setq led-combined-buffer (rgbled-buffer total-leds led-status-type))
        (setq combined-pins t)
    }{
        ;LED front/back are on same pin
        (if (and (>= led-front-pin 0) (= led-front-pin led-rear-pin)) {
            (var total-leds (+ led-front-num front-highbeam-leds led-rear-num rear-highbeam-leds))
            (setq led-combined-buffer (rgbled-buffer total-leds led-front-type))
            (setq combined-pins t)
        }{
            (if (and (>= led-status-pin 0) (= led-status-pin led-rear-pin)) {
                (var total-leds (+ led-status-num led-rear-num rear-highbeam-leds))
                (setq led-combined-buffer (rgbled-buffer total-leds led-rear-type))
                (setq combined-pins t)
            }{
                ; LED strips are on separate pins
                (if (>= led-status-pin 0) {
                    (setq led-status-buffer (rgbled-buffer led-status-num led-status-type))
                })
                (if (>= led-rear-pin 0) {
                    (setq led-rear-buffer (rgbled-buffer (+ led-rear-num rear-highbeam-leds) led-rear-type))
                })
            })
            (if (>= led-front-pin 0) {
                (setq led-front-buffer (rgbled-buffer (+ led-front-num front-highbeam-leds) led-front-type))
            })
        })
    })
})

(defun led-loop () {
    (load-led-settings)
    (init-led-vars)
    (var next-run-time (secs-since 0))  ; Set first run time
    (var loop-start-time 0)
    (var loop-end-time 0)
    (var led-loop-delay-sec (/ 1.0 led-loop-delay))
    (loopwhile t {
        (setq loop-start-time (secs-since 0))

        (if led-exit-flag {
            (break)
        })

        (var idle-rpm-darkride 100)
        (if (= state 4) ; RUNNING_UPSIDEDOWN
            (setq idle-rpm-darkride (* idle-rpm-darkride -1))
        )
        (if (!= state 3){;Ignore direction changes during wheelslip. Can also add a timer if this isn't sufficent.
            (if (> rpm idle-rpm-darkride) {;deadzone
                (setq direction 1)
            })
            (if (< rpm (* idle-rpm-darkride -1)) {
                (setq direction -1)
            })
        })
        (if (= led-mall-grab-enabled 1) {
            ;check if we have pitch-angle instead of duty
            (if (and (not (and (>= state 1) (<= state 5))) (> pitch-angle 70)) (setq led-mall-grab 1) (setq led-mall-grab 0))
        })

        (if (or (running-state) (= led-mall-grab 1) (display-battery-charging)) {
            (setq led-last-activity-time (systime))
        } {
            (setq direction 1)
        })

        (update-leds (secs-since led-last-activity-time))
        (led-flush-buffers)

        ; Capture end time and calculate actual loop time
        (setq loop-end-time (secs-since 0))
        (var actual-loop-time (- loop-end-time loop-start-time))

        ; Timing control
        (var time-to-wait (- next-run-time (secs-since 0)))  ; Calculate remaining time to wait in seconds

       ; (print (str-merge "Loop start: " (str-from-n loop-start-time "%.3f")))
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
        (setq next-run-time (+ next-run-time led-loop-delay-sec))

        ; Log actual loop time for debugging
        ;(print (str-merge "Actual loop time: " (str-from-n actual-loop-time "%.3f") " s"))
    })
    ;Exit process
    (clear-leds)
    (led-flush-buffers)
    ;(rgbled-deinit)
    (setq led-exit-flag nil)
})

(defun reverse-led-strips () {
    (if (= led-status-reversed 1) {
        (setq led-status-color (reverse led-status-color))
    })
    (if (= led-front-reversed 1) {
        (setq led-front-color (reverse led-front-color))
    })
    (if (= led-rear-reversed 1) {
        (setq led-rear-color (reverse led-rear-color))
    })
    (if (= led-footpad-reversed 1) {
        (setq led-footpad-color (reverse led-footpad-color))
    })
})

(defun display-battery-charging () {
    (let ret (or bms-charger-just-plugged (and (= led-show-battery-charging 1) bms-is-charging)))
})

(defun led-flush-buffers () {
    (reverse-led-strips)
    ;Enable/disable high beams and lets dim the rest of the leds if the high beams are on to help temps if on seperate pins
    (var led-current-brightness-rear led-current-brightness)
    (var led-current-brightness-front led-current-brightness)
    (var led-dim-on-highbeam-brightness (* led-current-brightness led-dim-on-highbeam-ratio))
    (var front-color-highbeam 0x00)
    (var rear-color-highbeam 0x00)
    (var led-current-front-color '())
    (var led-current-rear-color '())
    (if (and (= led-on 1) (= led-highbeam-on 1) (running-state) (!= state 5)){
        (if (>= direction 0){
            (setq front-color-highbeam (to-i(* 0xFF led-brightness-highbeam)))
            (if (> led-dim-on-highbeam-brightness 0.0) (setq led-current-brightness-front led-dim-on-highbeam-brightness))
        })
        (if (< direction 0){
            (setq rear-color-highbeam (to-i(* 0xFF led-brightness-highbeam)))
            (if (> led-dim-on-highbeam-brightness 0.0) (setq led-current-brightness-rear led-dim-on-highbeam-brightness))
        })
    })
    (cond
        ((or (= led-front-strip-type 2) (= led-front-strip-type 3)) {
            (if (and (<= led-dim-on-highbeam-brightness 0.0) (>= direction 0) (= led-on 1) (= led-highbeam-on 1) (running-state) (!= state 5)){
                (setq led-current-front-color (append (list front-color-highbeam) (mklist led-front-num 0)))
            }{
                (setq led-current-front-color (append (list front-color-highbeam) (take led-front-color led-front-num)))
            })
        })
        ((or (= led-front-strip-type 4) (= led-front-strip-type 5)) {
            (var led-tmp (take led-front-color (length led-front-color)))
            (setq led-current-front-color (mklist (+ (length led-front-color)4) 0))
            (var led-tmp-index 0)
            (looprange k 0 (length led-front-color){
                (if (or (and (= led-front-strip-type 4) (or (= k 2) (= k 7) (= k 13) (= k 18))) (and (= led-front-strip-type 5) (or (= k 1) (= k 5) (= k 10) (= k 3)))) {
                    (setix led-current-front-color k front-color-highbeam)
                }{
                    (if (and (<= led-dim-on-highbeam-brightness 0.0) (>= direction 0) (= led-on 1) (= led-highbeam-on 1) (running-state) (!= state 5)){
                        (setix led-current-front-color k 0)
                    }{
                        (setix led-current-front-color k (ix led-tmp led-tmp-index))
                    })
                    (setq led-tmp-index (+ led-tmp-index 1))
                })
            })
        })
        (_ {
            (setq led-current-front-color led-front-color)
            (setq led-current-brightness-front led-current-brightness)
        })
    )
    (cond
        ((or (= led-rear-strip-type 2) (= led-rear-strip-type 3)) {
            (if (and (<= led-dim-on-highbeam-brightness 0.0) (< direction 0) (= led-on 1) (= led-highbeam-on 1) (running-state) (!= state 5)){
                (setq led-current-rear-color (append (list rear-color-highbeam) (mklist led-rear-num 0)))
            }{
                (setq led-current-rear-color (append (list rear-color-highbeam) (take led-rear-color led-rear-num)))
            })
        })
        ((or (= led-rear-strip-type 4) (= led-rear-strip-type 5)) {
            (var led-tmp (take led-rear-color (length led-rear-color)))
            (setq led-current-rear-color (mklist (+ (length led-rear-color) 4) 0))
            (var led-tmp-index 0)
            (looprange k 0 (length led-rear-color){
                (if (or (and (= led-rear-strip-type 4) (or (= k 2) (= k 7) (= k 13) (= k 18))) (and (= led-rear-strip-type 5) (or (= k 1) (= k 5) (= k 10) (= k 3)))) {
                    (setix led-current-rear-color k rear-color-highbeam)
                }{
                    (if (and (<= led-dim-on-highbeam-brightness 0.0) (< direction 0) (= led-on 1) (= led-highbeam-on 1) (running-state) (!= state 5)){
                        (setix led-current-rear-color k 0)
                    }{
                        (setix led-current-rear-color k (ix led-tmp led-tmp-index))
                    })
                    (setq led-tmp-index (+ led-tmp-index 1))
                })
            })
        })
        (_ {
            (setq led-current-rear-color led-rear-color)
            (setq led-current-brightness-rear led-current-brightness)
        })
    )

    ;update
    (if (and (> led-button-strip-type 0) (>= led-button-pin 0)) {
        (rgbled-color led-button-buffer 0 led-button-color led-current-brightness)
        (rgbled-init led-button-pin 0)
        (yield led-fix)
        (rgbled-update led-button-buffer)
    })
    ;Update footpad

    (if (and (> led-footpad-strip-type 0) (>= led-footpad-pin 0)) {
        (rgbled-color led-footpad-buffer 0 led-footpad-color led-current-brightness)
        (rgbled-init led-footpad-pin led-footpad-type)
        (yield led-fix)
        (rgbled-update led-footpad-buffer)
    })

    (if (and (>= led-status-strip-type 0) (>= led-front-strip-type 0) (>= led-rear-strip-type 0) (>= led-front-pin 0) (= led-status-pin led-front-pin) (= led-front-pin led-rear-pin)) {
        ; All LED strips are chained on the same pin
        (var led-combined-color (append led-status-color led-current-front-color led-current-rear-color))
        (var total-leds (length led-combined-color))
        (rgbled-color led-combined-buffer 0 led-combined-color led-current-brightness)
        (rgbled-init led-front-pin led-front-type)
        (yield led-fix)
        (rgbled-update led-combined-buffer)
    }{
        ;LED front/back are on same pin
        (if (and (> led-front-strip-type 0) (> led-rear-strip-type 0) (>= led-front-pin 0) (= led-front-pin led-rear-pin)) {
            (var led-combined-color (append led-current-front-color led-current-rear-color))
            (var total-leds (length led-combined-color))
            (rgbled-color led-combined-buffer 0 led-combined-color led-current-brightness)
            (rgbled-init led-front-pin led-front-type)
            (yield led-fix)
            (rgbled-update led-combined-buffer)
        }{
            (if (and (> led-status-strip-type 0) (> led-rear-strip-type 0) (>= led-status-pin 0) (= led-status-pin led-rear-pin)) {
                (if (!= led-status-type led-rear-type)
                    (swap-rg led-status-color); Fix for avaspark rgb when there's different types. e.g. stock GT RGBW
                )
                (var led-combined-color (append led-status-color led-current-rear-color))
                (var total-leds (length led-combined-color))
                (rgbled-color led-combined-buffer 0 led-combined-color led-current-brightness)
                (rgbled-init led-status-pin led-status-type)
                (yield led-fix)
                (rgbled-update led-combined-buffer)
            }{
                ; LED strips are on separate pins
                (if (and (> led-status-strip-type 0) (>= led-status-pin 0)) {
                    (rgbled-color led-status-buffer 0 led-status-color led-brightness-status)
                    (rgbled-init led-status-pin led-status-type)
                    (yield led-fix)
                    (rgbled-update led-status-buffer)
                })
                (if (and (> led-rear-strip-type 0)(>= led-rear-pin 0)) {
                    (rgbled-color led-rear-buffer 0 led-current-rear-color led-current-brightness-rear)
                    (rgbled-init led-rear-pin led-rear-type)
                    (yield led-fix)
                    (rgbled-update led-rear-buffer)
                })
            })
            (if (and (> led-front-strip-type 0) (>= led-front-pin 0)) {
                (rgbled-color led-front-buffer 0 led-current-front-color led-current-brightness-front)
                (rgbled-init led-front-pin led-front-type)
                (yield led-fix)
                (rgbled-update led-front-buffer)
            })
        })
    })
})

(defun update-status-leds () {
    (if (or (= state 15) handtest-mode) {
        (led-float-disabled led-status-color)
    }{
        (if (> rpm 250.0){
            (duty-cycle-pattern)
        }{
            (if (and (!= switch-state 1) (!= switch-state 2) (!= switch-state 3)){
                ;(if (display-battery-charging) {
                ;    ;Do something
                ;}{
                    (battery-pattern led-status-color)
                ;})
            }{;else
                (footpad-pattern switch-state)
            })
        })
    })
})

(defun update-button-led () {
    (cond
        ((= led-mode-button 0) {
            (rainbow-button)
        })
    )
})

(defun update-leds (last-activity-sec) {
    (var can-last-activity-time-sec (secs-since can-last-activity-time))
    (if (> (length led-status-color) 0){
        (if (= led-mode-status 0) (update-status-leds))
    })
    (var current-led-mode led-mode)
    (setq led-current-brightness led-brightness)
    (if (or (and (>= last-activity-sec idle-timeout) (<= can-last-activity-time-sec 1)) (= state 5)) {
        (setq current-led-mode led-mode-idle)
        (setq led-current-brightness led-brightness-idle)
    })

    (if (and (<= (secs-since 0) led-startup-timeout) (not (running-state) )) { (setq current-led-mode led-mode-startup)})
    ; Blend colors
    (var blend-ratio (/ blend-count led-max-blend-count))
    ;(if (<= current-led-mode 5){
        (looprange i 0 (length led-front-color) {
            (setix led-front-color i (color-mix (ix prev-led-front-color i) (ix target-led-front-color i)  blend-ratio))
        })
        (looprange i 0 (length led-rear-color) {
            (setix led-rear-color i (color-mix (ix prev-led-rear-color i) (ix target-led-rear-color i)  blend-ratio))
        })
    ;})
    (setix led-button-color 0 (color-mix (ix prev-led-button-color 0) (ix target-led-button-color 0)  blend-ratio))
    (setq blend-count (+ blend-count 1.0))
    ; Reset blend count and update colors when max count is reached
    (if (> blend-count led-max-blend-count) {
        (setq prev-led-front-color (take target-led-front-color (length target-led-front-color)))
        (setq prev-led-rear-color (take target-led-rear-color (length target-led-rear-color)))
        (setq prev-led-button-color (take target-led-button-color (length target-led-button-color)))
        (if (= led-on 1) { ;put this in update-leds so we can have led-button before so it always gets updated regardless of state
        (if (and (> (length led-front-color) 0) (> (length led-rear-color) 0)){
            (cond
                ((or (= state 15) handtest-mode) {
                    (clear-leds)
                    (led-float-disabled led-status-color)
                    (led-float-disabled led-front-color)
                })
                ((and (> last-activity-sec idle-timeout-shutoff) (< can-last-activity-time-sec 1) (!= state 5)){;make sure we dont' clear if we loose can bus
                    (clear-leds)
                })
                ((and (or (= current-led-mode 1) (= led-mall-grab 1)) (< can-last-activity-time-sec 1)) {
                    (battery-pattern led-front-color)
                    (battery-pattern led-rear-color)
                })
                ((or (= current-led-mode 0) (and (> can-last-activity-time-sec 1) (> (secs-since 0) led-startup-timeout))) {
                    (set-led-strip-color (if (> direction 0) led-front-color led-rear-color) 0xFFFFFFFFu32)
                    (set-led-strip-color (if (< direction 0) led-front-color led-rear-color) 0x00FF0000u32)
                    ;(print "hi")
                })
                ((= current-led-mode 2) {
                    (set-led-strip-color (if (> direction 0) led-front-color led-rear-color) 0x0000FFFFu32)
                    (set-led-strip-color (if (< direction 0) led-front-color led-rear-color) 0x00FF00FFu32)
                })
                ((= current-led-mode 3) {
                    (set-led-strip-color (if (> direction 0) led-front-color led-rear-color) 0x000000FFu32)
                    (set-led-strip-color (if (< direction 0) led-front-color led-rear-color) 0x0000FF00u32)
                })
                ((= current-led-mode 4) {
                    (set-led-strip-color (if (> direction 0) led-front-color led-rear-color) 0x00FFFF00u32)
                    (set-led-strip-color (if (< direction 0) led-front-color led-rear-color) 0x0000FF00u32)
                })
                ((= current-led-mode 5) {
                    (rainbow-pattern)
                })
                ((= current-led-mode 6) {
                    (strobe-pattern)
                })
                ((= current-led-mode 7) {
                    (rave-pattern 0)
                })
                ((= current-led-mode 8) {
                    (rave-pattern 1)
                })
                ((= current-led-mode 9) {
                    (knight-rider-pattern)
                })
                ((= current-led-mode 10) {
                    (felony-pattern)
                })
            )
        (if (and (= led-brake-light-enabled 1) (running-state) (!= state 5) (<= tot-current led-brake-light-min-amps)){
            (if (>= direction 0){
                (brake-pattern led-rear-color)
            }{
                (brake-pattern led-front-color)
            })
        })
        (if (display-battery-charging) {
            ;do something
            (battery-pattern led-front-color)
            (battery-pattern led-rear-color)
        })
        })
        }{
            (clear-leds)
        })
        ; Update button LED
        (update-button-led)
        (setq target-led-front-color (take led-front-color (length led-front-color)))
        (setq target-led-rear-color (take led-rear-color (length led-rear-color)))
        (setq target-led-button-color (take led-button-color (length led-button-color)))
        (setq blend-count 1.0)  ; Reset blend count for new transition
        ; Blend colors
        (var blend-ratio (if (> blend-count 0) (/ blend-count led-max-blend-count) 0.0))
        ;(if (<= current-led-mode 5){
            (looprange i 0 (length led-front-color) {
                (setix led-front-color i (color-mix (ix prev-led-front-color i) (ix target-led-front-color i)  blend-ratio))
                ;(print blend-ratio)
            })
            (looprange i 0 (length led-rear-color) {
                (setix led-rear-color i (color-mix (ix prev-led-rear-color i) (ix target-led-rear-color i)  blend-ratio))
            })
        ;})
        (setix led-button-color 0 (color-mix (ix prev-led-button-color 0) (ix target-led-button-color 0)  blend-ratio))
    })
})

(defun clear-leds () {
    (set-led-strip-color led-status-color 0x00)
    (set-led-strip-color led-front-color 0x00)
    (set-led-strip-color led-rear-color 0x00)
    (set-led-strip-color led-footpad-color 0x00)
})

;(defun number-in-list-p (number lst)
;  (if (eq lst nil)
;      nil
;    (if (eq number (car lst))
;        t
;      (number-in-list-p number (cdr lst)))))

@const-end
