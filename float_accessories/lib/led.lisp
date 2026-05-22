;@const-symbol-strings
;Buffers
(def led-button-buffer)
(def led-footpad-buffer)
(def led-status-buffer)
(def led-front-buffer)
(def led-rear-buffer)
(def led-combined-buffer)
@const-start

(def led-loop-delay)
;config vars
(def led-enabled)
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
(def led-max-brightness)
(def led-update-not-running)

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
(def next-run-time)
(def direction)
(def led-mall-grab)
(def prev-led-front-color '())
(def prev-led-rear-color '())
(def prev-led-footpad-color '())
(def target-led-front-color '())
(def target-led-rear-color '())
(def target-led-footpad-color '())
(def prev-led-button-color '())
(def target-led-button-color '())
(def combined-pins nil)
(def led-fix 1)
(def led-show-battery-charging 0)
(def led-front-highbeam-pin)
(def led-rear-highbeam-pin)

(defun load-led-settings () {
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
    (setq led-front-highbeam-pin (get-config 'led-front-highbeam-pin))
    (setq led-rear-highbeam-pin (get-config 'led-rear-highbeam-pin))
    (setq led-max-brightness (get-config 'led-max-brightness))
    (setq led-update-not-running (get-config 'led-update-not-running))
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
    (setq prev-led-footpad-color (mklist led-footpad-num 0))
    (setq target-led-front-color (mklist led-front-num 0))
    (setq target-led-rear-color (mklist led-rear-num 0))
    (setq target-led-footpad-color (mklist led-footpad-num 0))
    (setq prev-led-button-color (mklist 1 0))
    (setq target-led-button-color (mklist 1 0))
    (if (>= led-button-pin 0) {
        (setq led-button-buffer (rgbled-buffer 1 0))
    })

    (if (and (= led-front-strip-type 7) (>= led-front-highbeam-pin 0)) {
        (pwm-start 1000 0.0 0 led-front-highbeam-pin 10)
    })

    (if (and (= led-rear-strip-type 7) (>= led-rear-highbeam-pin 0)) {
        (pwm-start 1000 0.0 1 led-rear-highbeam-pin 10)
    })

    (var front-highbeam-leds 0)
    (var rear-highbeam-leds 0)
    (if (or (= led-front-strip-type 2) (= led-front-strip-type 3) (= led-front-strip-type 8) (= led-front-strip-type 9) (= led-front-strip-type 10)) {
         (setq front-highbeam-leds (+ front-highbeam-leds 1))
    })
    (if (or (= led-rear-strip-type 2) (= led-rear-strip-type 3) (= led-rear-strip-type 8) (= led-rear-strip-type 9) (= led-rear-strip-type 10)) {
         (setq rear-highbeam-leds (+ rear-highbeam-leds 1))
    })
    (if (or (= led-front-strip-type 4) (= led-front-strip-type 5) (= led-front-strip-type 6) (= led-front-strip-type 11)) {
         (setq front-highbeam-leds (+ front-highbeam-leds 4))
    })
    (if (or (= led-rear-strip-type 4) (= led-rear-strip-type 5) (= led-rear-strip-type 6) (= led-rear-strip-type 11)) {
         (setq rear-highbeam-leds (+ rear-highbeam-leds 4))
    })
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
    (var next-run-time (secs-since 0))
    (var loop-start-time 0)
    (var loop-end-time 0)
    (var led-loop-delay-sec (/ 1.0 led-loop-delay))
    (var prev-direction 1)
    (var direction-change-start-time 0)
    (var direction-change-window 0.5)
    (var anim-time 0)
    (var prev-run-state 0)
    (var led-run-start-time 0)
    (var mall-grab-press-start 0)
    (var mall-grab-press-active nil)
    (loopwhile t {
        (setq loop-start-time (secs-since 0))
        (setq anim-time (+ anim-time led-loop-delay-sec))
        (if (> anim-time 100.0) (setq anim-time 0.0)) ; prevent overflow
        (if led-exit-flag {
            (break)
        })

        (var idle-rpm-darkride 100)
        (if (= state 4) ; RUNNING_UPSIDEDOWN
            (setq idle-rpm-darkride (* idle-rpm-darkride -1))
        )
        (if (!= state 3){;Ignore direction changes during wheelslip.
            (var current-direction direction)
            (if (> rpm idle-rpm-darkride) {;deadzone
                (setq current-direction 1)
            })
            (if (< rpm (* idle-rpm-darkride -1)) {
                (setq current-direction -1)
            })
            (if (!= current-direction prev-direction) {
                (if (= direction-change-start-time 0) {
                    ; Start the timer for a new potential direction change
                    (setq direction-change-start-time (systime))
                }{
                    ; Check if the timer has expired
                    (if (>= (secs-since direction-change-start-time) direction-change-window) {
                        ; Timer expired, commit the direction change
                        (setq direction current-direction)
                        (setq prev-direction current-direction)
                        (setq direction-change-start-time 0)
                    })
                })
            })
        })

        (if (and (not (running-state)) (> pitch-angle 70)) {
            (setq led-mall-grab (if (= led-mall-grab-enabled 1) 1 0))
            (if (= switch-state 3) {

                (if (not mall-grab-press-active) {
                    (setq mall-grab-press-start (systime))
                    (setq mall-grab-press-active t)
                })
            }{
                (if mall-grab-press-active {
                    (var press-duration (secs-since mall-grab-press-start))
                    (if (< press-duration 1) {
                        ;; SHORT press → toggle LED ON/OFF
                        (setq led-on (if (= led-on 1) 0 1))
                    }{
                        ;; LONG press → toggle HIGHBEAM ON/OFF
                        (setq led-highbeam-on (if (= led-highbeam-on 1) 0 1))
                    })
            (setq mall-grab-press-active nil)
                })
            })
        }{
            (setq led-mall-grab 0)
            (setq mall-grab-press-active nil)
        })

        (if (or (running-state) (= led-mall-grab 1) (display-battery-charging)) {
            (setq led-last-activity-time (systime))
        }{
            (setq direction 1)
        })

        (if (running-state) {
            (if (= prev-run-state 0) {
                ;; Just transitioned from idle → running
                (setq led-run-start-time (systime))
            })
            (setq prev-run-state 1)
        }{
            (setq prev-run-state 0)
        })
        (var dont-freeze-update (not (and (running-state) (= led-update-not-running 1) (> (secs-since led-run-start-time) 1))))
        (update-leds (secs-since led-last-activity-time) anim-time)
        (trap (led-flush-buffers dont-freeze-update))

        (setq loop-end-time (secs-since 0))
        (var actual-loop-time (- loop-end-time loop-start-time))

        (var time-to-wait (- next-run-time (secs-since 0)))
        (if (> time-to-wait 0) {

            (yield (* time-to-wait 1000000))
        } {
            (setq next-run-time (secs-since 0))
        })

        (setq next-run-time (+ next-run-time led-loop-delay-sec))
    })
    (clear-leds)
    (set-led-strip-color led-button-color 0x00)
    (set-led-strip-color led-status-color 0x00)
    (led-flush-buffers t)
    (rgbled-deinit)
    (if (and (= led-front-strip-type 7) (>= led-front-highbeam-pin 0)) (pwm-stop 0))
    (if (and (= led-rear-strip-type 7) (>= led-rear-highbeam-pin 0)) (pwm-stop 1))
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
    (let ret (or bms-charger-just-plugged (and (= led-show-battery-charging 1) bms-is-charging (not (running-state) ))))
})

(defun led-flush-buffers (dont-freeze-update) {
    (reverse-led-strips)
    ;Enable/disable high beams and lets dim the rest of the leds if the high beams are on to help temps if on seperate pins

    (var led-current-brightness-rear led-current-brightness)
    (var led-current-brightness-front led-current-brightness)
    (var led-dim-on-highbeam-brightness (* led-current-brightness led-dim-on-highbeam-ratio))
    (var front-color-highbeam 0x00)
    (var rear-color-highbeam 0x00)
    (var led-current-front-color '())
    (var led-current-rear-color '())

    (var front-highbeam-on false)
    (var rear-highbeam-on false)
    (if (and (= led-on 1) (= led-highbeam-on 1) (running-state) (!= state 5)){
        (if (>= direction 0){
            (setq front-color-highbeam 0xFF)
            (if (> led-dim-on-highbeam-brightness 0.0) (setq led-current-brightness-front led-dim-on-highbeam-brightness))
            (setq front-highbeam-on t)
        })
        (if (< direction 0){
            (setq rear-color-highbeam 0xFF)
            (if (> led-dim-on-highbeam-brightness 0.0) (setq led-current-brightness-rear led-dim-on-highbeam-brightness))
            (setq rear-highbeam-on t)
        })
    })

    (cond
        ((or (= led-front-strip-type 2) (= led-front-strip-type 3) (= led-front-strip-type 8) (= led-front-strip-type 9) (= led-front-strip-type 10)) {
            (if (and (<= led-dim-on-highbeam-brightness 0.0) (>= direction 0) (= led-on 1) (= led-highbeam-on 1) (running-state) (!= state 5)){
                (setq led-current-front-color (append (list front-color-highbeam) (mklist led-front-num 0)))
            }{
                (setq led-current-front-color (append (list front-color-highbeam) (take led-front-color led-front-num)))
            })
        })
        ((or (= led-front-strip-type 4) (= led-front-strip-type 5) (= led-front-strip-type 6)) { ; JetFleet H4, JetFleet H4 (no limit), JetFleet GT
            (var led-tmp (take led-front-color (length led-front-color)))
            (setq led-current-front-color (mklist (+ (length led-front-color) 4) 0))
            (var led-tmp-index 0)
            (setq led-current-brightness-front (+ 0.6 (* (if (= led-front-strip-type 4) 0.2 0.4) led-current-brightness-front))); Maps 0-1 to 0.60-1.0
            (looprange k 0 (length led-current-front-color){
                (if (or (and (or (= led-front-strip-type 4) (= led-front-strip-type 5)) (or (= k 3) (= k 8) (= k 14) (= k 19))) (and (= led-front-strip-type 6) (or (= k 1) (= k 4) (= k 10) (= k 13)))) {
                    (setix led-current-front-color k (color-scale front-color-highbeam led-current-brightness-front)) ; We scale the color to apply the brightness here and not when the rgbled-color is called. We use the mapped brightness.
                }{
                    (if (and (<= led-dim-on-highbeam-brightness 0.0) (>= direction 0) (= led-on 1) (= led-highbeam-on 1) (running-state) (!= state 5)){
                        (setix led-current-front-color k 0)
                    }{
                        (setix led-current-front-color k (color-scale (ix led-tmp led-tmp-index) led-current-brightness)) ; We scale the color to apply the brightness here and not when the rgbled-color is called
                    })
                    (setq led-tmp-index (+ led-tmp-index 1))
                })
            })
        })
        ((= led-front-strip-type 11) { ; Fungineers GTFO
            (var led-tmp (take led-front-color (length led-front-color)))
            (setq led-current-front-color (mklist (+ (length led-front-color) 4) 0))
            (var led-tmp-index 0)
            (setq led-current-brightness-front (+ 0.4 (* 0.6 led-current-brightness-front))); Maps 0-1 to 0.40-1.0
            (looprange k 0 (length led-current-front-color){
                (if (or (= k 3) (= k 6) (= k 9) (= k 13)) {
                    (setix led-current-front-color k (color-scale front-color-highbeam led-current-brightness-front)) ; We scale the color to apply the brightness here and not when the rgbled-color is called. We use the mapped brightness.
                }{
                    (if (and (<= led-dim-on-highbeam-brightness 0.0) (>= direction 0) (= led-on 1) (= led-highbeam-on 1) (running-state) (!= state 5)){
                        (setix led-current-front-color k 0)
                    }{
                        (setix led-current-front-color k (color-scale (ix led-tmp led-tmp-index) led-current-brightness)) ; We scale the color to apply the brightness here and not when the rgbled-color is called
                    })
                    (setq led-tmp-index (+ led-tmp-index 1))
                })
            })
        })
        ((and (= led-front-strip-type 7) (>= led-front-highbeam-pin 0)) {
            (if front-highbeam-on {
                (pwm-set-duty (min led-brightness-highbeam led-max-brightness) 0)
                (setq led-current-brightness-front led-dim-on-highbeam-brightness)
            } {
                 (pwm-set-duty 0.0 0)
                 (setq led-current-brightness-front led-current-brightness)
            })
            (setq led-current-front-color led-front-color)
        })
        (_ {
            (setq led-current-front-color led-front-color)
            (setq led-current-brightness-front led-current-brightness)
        })
    )
    (cond
        ((or (= led-rear-strip-type 2) (= led-rear-strip-type 3) (= led-rear-strip-type 8) (= led-rear-strip-type 9) (= led-rear-strip-type 10)) {
            (if (and (<= led-dim-on-highbeam-brightness 0.0) (< direction 0) (= led-on 1) (= led-highbeam-on 1) (running-state) (!= state 5)){
                (setq led-current-rear-color (append (list rear-color-highbeam) (mklist led-rear-num 0)))
            }{
                (setq led-current-rear-color (append (list rear-color-highbeam) (take led-rear-color led-rear-num)))
            })
        })
        ((or (= led-rear-strip-type 4) (= led-rear-strip-type 5) (= led-rear-strip-type 6)) { ; JetFleet H4, JetFleet H4 (no limit), JetFleet GT
            (var led-tmp (take led-rear-color (length led-rear-color)))
            (setq led-current-rear-color (mklist (+ (length led-rear-color) 4) 0))
            (var led-tmp-index 0)
            (setq led-current-brightness-rear (+ 0.6 (* (if (= led-rear-strip-type 4) 0.2 0.4) led-current-brightness-rear))) ; Maps 0-1 to 0.60-1.0
            (looprange k 0 (length led-current-rear-color){
                (if (or (and (or (= led-rear-strip-type 4) (= led-rear-strip-type 5)) (or (= k 3) (= k 8) (= k 14) (= k 19))) (and (= led-rear-strip-type 6) (or (= k 1) (= k 4) (= k 10) (= k 13) ))) {
                    (setix led-current-rear-color k (color-scale rear-color-highbeam led-current-brightness-rear)) ; We scale the color to apply the brightness here and not when the rgbled-color is called. We use the mapped brightness.
                }{
                    (if (and (<= led-dim-on-highbeam-brightness 0.0) (< direction 0) (= led-on 1) (= led-highbeam-on 1) (running-state) (!= state 5)){
                        (setix led-current-rear-color k 0)
                    }{
                        (setix led-current-rear-color k (color-scale (ix led-tmp led-tmp-index) led-current-brightness)) ; We scale the color to apply the brightness here and not when the rgbled-color is called
                    })
                    (setq led-tmp-index (+ led-tmp-index 1))
                })
            })
        })
        ((= led-rear-strip-type 11) { ; Fungineers GTFO
            (var led-tmp (take led-rear-color (length led-rear-color)))
            (setq led-current-rear-color (mklist (+ (length led-rear-color) 4) 0))
            (var led-tmp-index 0)
            (setq led-current-brightness-rear (+ 0.4 (* 0.6 led-current-brightness-rear))); Maps 0-1 to 0.40-1.0
            (looprange k 0 (length led-current-rear-color){
                (if (or (= k 3) (= k 6) (= k 9) (= k 13)) {
                    (setix led-current-rear-color k (color-scale rear-color-highbeam led-current-brightness-rear)) ; We scale the color to apply the brightness here and not when the rgbled-color is called. We use the mapped brightness.
                }{
                    (if (and (<= led-dim-on-highbeam-brightness 0.0) (< direction 0) (= led-on 1) (= led-highbeam-on 1) (running-state) (!= state 5)){
                        (setix led-current-rear-color k 0)
                    }{
                        (setix led-current-rear-color k (color-scale (ix led-tmp led-tmp-index) led-current-brightness)) ; We scale the color to apply the brightness here and not when the rgbled-color is called
                    })
                    (setq led-tmp-index (+ led-tmp-index 1))
                })
            })
        })
        ((and (= led-rear-strip-type 7) (>= led-rear-highbeam-pin 0)) {
            (if rear-highbeam-on {
                (pwm-set-duty (min led-brightness-highbeam led-max-brightness) 1)
                (setq led-current-brightness-rear led-dim-on-highbeam-brightness)
            } {
                 (pwm-set-duty 0.0 1)
                 (setq led-current-brightness-rear led-current-brightness)
            })
            (setq led-current-rear-color led-rear-color)
        })
        (_ {
            (setq led-current-rear-color led-rear-color)
            (setq led-current-brightness-rear led-current-brightness)
        })
    )
    (if (and (> led-button-strip-type 0) (>= led-button-pin 0)) {
        (rgbled-color led-button-buffer 0 led-button-color led-current-brightness)
        (rgbled-init led-button-pin)
        (yield led-fix)
        (rgbled-update led-button-buffer)
    })

    (if (and (> led-footpad-strip-type 0) (>= led-footpad-pin 0)) {
        (rgbled-color led-footpad-buffer 0 led-footpad-color led-current-brightness)
        (rgbled-init led-footpad-pin)
        (yield led-fix)
        (rgbled-update led-footpad-buffer)
    })

    (if (and (>= led-status-strip-type 0) (>= led-front-strip-type 0) (>= led-rear-strip-type 0) (>= led-front-pin 0) (= led-status-pin led-front-pin) (= led-front-pin led-rear-pin)) {
        ; All LED strips are chained on the same pin
        (var led-combined-color (append led-status-color led-current-front-color led-current-rear-color))
        (var total-leds (length led-combined-color))
        (rgbled-color led-combined-buffer 0 led-combined-color led-current-brightness)
        (rgbled-init led-front-pin)
        (yield led-fix)
        (rgbled-update led-combined-buffer)
    }{
        ;LED front/back are on same pin
        (if (and (> led-front-strip-type 0) (> led-rear-strip-type 0) (>= led-front-pin 0) (= led-front-pin led-rear-pin)) {
            (var led-combined-color (append led-current-front-color led-current-rear-color))
            (var total-leds (length led-combined-color))
            (rgbled-color led-combined-buffer 0 led-combined-color led-current-brightness)
            (rgbled-init led-front-pin)
            (yield led-fix)
            (rgbled-update led-combined-buffer)
        }{
            (if (and (> led-status-strip-type 0) (> led-rear-strip-type 0) (>= led-status-pin 0) (= led-status-pin led-rear-pin)) {
                (if (!= led-status-type led-rear-type)
                    (swap-rg led-status-color); Fix for avaspark rgb when there's different type like rgb and grb chained together.
                )
                (var led-combined-color (append led-status-color led-current-rear-color))
                (var total-leds (length led-combined-color))
                (rgbled-color led-combined-buffer 0 led-combined-color led-current-brightness)
                (rgbled-init led-status-pin)
                (yield led-fix)
                (rgbled-update led-combined-buffer)
            }{
                ; LED strips are on separate pins
                (if (and (> led-status-strip-type 0) (>= led-status-pin 0)) {
                    (rgbled-color led-status-buffer 0 led-status-color (min led-brightness-status led-max-brightness))
                    (rgbled-init led-status-pin)
                    (yield led-fix)
                    (rgbled-update led-status-buffer)
                })
                (if (and (> led-rear-strip-type 0) (>= led-rear-pin 0) dont-freeze-update) {
                    ; If it's a JetFleet H4, JetFleet H4 (no limit), JetFleet GT or Fungineers GTFO we do not pass brighness as the buffer already has brighness applied to each color to account for the special mapping of the high beams.
                    (if (or (= led-rear-strip-type 4) (= led-rear-strip-type 5) (= led-rear-strip-type 6) (= led-rear-strip-type 11))
                        (rgbled-color led-rear-buffer 0 led-current-rear-color)
                        (rgbled-color led-rear-buffer 0 led-current-rear-color led-current-brightness-rear)
                    )
                    (rgbled-init led-rear-pin)
                    (yield led-fix)
                    (rgbled-update led-rear-buffer)
                })
            })
            (if (and (> led-front-strip-type 0) (>= led-front-pin 0) dont-freeze-update) {
                ; If it's a JetFleet H4, JetFleet H4 (no limit), JetFleet GT or Fungineers GTFO we do not pass brighness as the buffer already has brighness applied to each color to account for the special mapping of the high beams.
                (if (or (= led-front-strip-type 4) (= led-front-strip-type 5) (= led-front-strip-type 6) (= led-front-strip-type 11))
                    (rgbled-color led-front-buffer 0 led-current-front-color)
                    (rgbled-color led-front-buffer 0 led-current-front-color led-current-brightness-front)
                )
                (rgbled-init led-front-pin)
                (yield led-fix)
                (rgbled-update led-front-buffer)
            })
        })
    })
})

(defun update-status-leds (can-last-activity-time-sec anim-time) {
    (if (or (= state 15) handtest-mode (>= can-last-activity-time-sec 1) (< can-id 0)) {
        (cond
            (handtest-mode {
                (led-handtest led-status-color switch-state 1 anim-time led-mode-status)
            })
            ((= state 15) {
                (led-float-disabled led-status-color)
            })
            ((>= can-last-activity-time-sec 1) {
                (led-connecting led-status-color anim-time)
            })
        )
    }{
        (if (> rpm 250.0){
            (if (> sat-t 2) {
                (strobe-pattern led-status-color 0x00FF0000 anim-time)
            }{
                (duty-cycle-pattern led-status-color)
            })
        }{
            (if (and (!= switch-state 1) (!= switch-state 2) (!= switch-state 3)){
                ;(if (display-battery-charging) { TODO
                ;    ;Do something
                ;}{
                    (battery-pattern led-status-color bms-is-charging anim-time)
                ;})
            }{
                (footpad-pattern led-status-color switch-state led-mode-status)
            })
        })
    })
})

(defun update-button-led (anim-time) {
    (cond
        ((= led-mode-button 0) {
            (rainbow-pattern led-button-color anim-time)
        })
        ((= led-mode-button 1) {
            (battery-pattern-button led-button-color bms-is-charging anim-time)
        })
    )
})

(defun update-leds (last-activity-sec anim-time) {
    (var can-last-activity-time-sec (secs-since can-last-activity-time))
    (if (> (length led-status-color) 0){
        (if (or (= led-mode-status 0) (= led-mode-status 1)) (update-status-leds can-last-activity-time-sec anim-time))
    })
    (var current-led-mode led-mode)
    (setq led-current-brightness (min led-brightness led-brightness led-max-brightness))
    (if (= led-mall-grab 1) {
        (setq led-current-brightness (min led-brightness-status led-max-brightness))
    })
    (if (or (and (>= last-activity-sec idle-timeout) (<= can-last-activity-time-sec 1)) (= state 5)) {
        (setq current-led-mode led-mode-idle)
        (setq led-current-brightness (min led-brightness-idle led-max-brightness))
    })

    (if (and (<= (secs-since 0) led-startup-timeout) (not (running-state) )) { (setq current-led-mode led-mode-startup)})
    (var blend-ratio (/ blend-count led-max-blend-count))
    (looprange i 0 (length led-front-color) {
        (setix led-front-color i (color-mix (ix prev-led-front-color i) (ix target-led-front-color i)  blend-ratio))
    })
    (looprange i 0 (length led-rear-color) {
        (setix led-rear-color i (color-mix (ix prev-led-rear-color i) (ix target-led-rear-color i)  blend-ratio))
    })
    (looprange i 0 (length led-footpad-color) {
        (setix led-footpad-color i (color-mix (ix prev-led-footpad-color i) (ix target-led-footpad-color i)  blend-ratio))
    })
    (setix led-button-color 0 (color-mix (ix prev-led-button-color 0) (ix target-led-button-color 0)  blend-ratio))
    (setq blend-count (+ blend-count 1.0))
    ; Reset blend count and update colors when max count is reached
    (if (> blend-count led-max-blend-count) {
        (setq prev-led-front-color (take target-led-front-color (length target-led-front-color)))
        (setq prev-led-rear-color (take target-led-rear-color (length target-led-rear-color)))
        (setq prev-led-footpad-color (take target-led-footpad-color (length target-led-footpad-color)))
        (setq prev-led-button-color (take target-led-button-color (length target-led-button-color)))
        ;TODO Put mall grab stuff here, since should happen even if led is off. also make sure it works if it's timeout
        (if (= led-on 1) {
            (if (> (length led-footpad-color) 0){
                (cond
                    ((= led-mode-footpad 0) {
                        (rainbow-pattern led-footpad-color anim-time)
                    })
                )
            })
            (if (and (> (length led-front-color) 0) (> (length led-rear-color) 0)){
                (cond
                    ((= state 15) {
                        (clear-leds)
                        (led-float-disabled led-rear-color)
                        (led-float-disabled led-front-color)
                    })
                    (handtest-mode {
                        (led-handtest led-front-color switch-state 2 anim-time led-mode-status)
                        (led-handtest led-rear-color switch-state 2 anim-time led-mode-status)
                    })
                    ((and (> last-activity-sec idle-timeout-shutoff) (< can-last-activity-time-sec 1) (!= state 5)){;make sure we dont' clear if we loose can bus
                        (clear-leds)
                    })
                    ((and (or (= current-led-mode 1) (= led-mall-grab 1)) (< can-last-activity-time-sec 1)) {
                        (battery-pattern led-front-color bms-is-charging anim-time)
                        (battery-pattern led-rear-color bms-is-charging anim-time)
                    })
                    ((or (= current-led-mode 0) (and (> can-last-activity-time-sec 1) (> (secs-since 0) led-startup-timeout)) (and (running-state) (= led-update-not-running 1))) {
                        (set-led-strip-color (if (> direction 0) led-front-color led-rear-color) 0xFFFFFFFFu32);todo add led-front-rgb-val
                        (set-led-strip-color (if (< direction 0) led-front-color led-rear-color) 0x00FF0000u32)
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
                        (rainbow-pattern led-front-color anim-time)
                        (rainbow-pattern led-rear-color anim-time)
                    })
                    ((= current-led-mode 6) {
                        (strobe-pattern led-front-color 0xFFFFFFFF anim-time)
                        (strobe-pattern led-rear-color 0xFFFFFFFF anim-time)
                    })
                    ((= current-led-mode 7) {
                        (rave-pattern led-front-color anim-time)
                        (rave-pattern led-rear-color anim-time)
                    })
                    ((= current-led-mode 8) {
                        (if (>= direction 0) {
                            (set-led-strip-color led-front-color 0xFFFFFFFF)
                            (rave-pattern led-rear-color anim-time)
                        }{
                            (set-led-strip-color led-rear-color 0xFFFFFFFF)
                            (rave-pattern led-front-color anim-time)
                        })
                    })
                    ((= current-led-mode 9) {
                        (knight-rider-pattern led-front-color anim-time)
                        (knight-rider-pattern led-rear-color anim-time)
                    })
                    ((= current-led-mode 10) {
                        (felony-pattern led-front-color anim-time)
                        (felony-pattern led-rear-color anim-time)
                    })
                    ((= current-led-mode 11) {
                        (trans-pattern led-front-color anim-time)
                        (trans-pattern led-rear-color anim-time)
                    })
                )
                (if (and (= led-brake-light-enabled 1) (running-state) (!= state 5) (<= tot-current led-brake-light-min-amps) (= led-update-not-running 0) ){
                    (strobe-pattern (if (>= direction 0) led-rear-color led-front-color) 0x00FF0000 anim-time)
                })

                (if (display-battery-charging) {
                    (battery-pattern led-front-color bms-is-charging anim-time)
                    (battery-pattern led-rear-color bms-is-charging anim-time)
                })
            })
        }{
            (clear-leds)
        })
        (update-button-led anim-time)
        (setq target-led-front-color (take led-front-color (length led-front-color)))
        (setq target-led-rear-color (take led-rear-color (length led-rear-color)))
        (setq target-led-button-color (take led-button-color (length led-button-color)))
        (setq blend-count 1.0)  ; Reset blend count for new transition
        ; Blend colors
        (var blend-ratio (if (> blend-count 0) (/ blend-count led-max-blend-count) 0.0))
        (looprange i 0 (length led-front-color) {
            (setix led-front-color i (color-mix (ix prev-led-front-color i) (ix target-led-front-color i)  blend-ratio))
        })
        (looprange i 0 (length led-rear-color) {
            (setix led-rear-color i (color-mix (ix prev-led-rear-color i) (ix target-led-rear-color i)  blend-ratio))
        })
        (looprange i 0 (length led-footpad-color) {
            (setix led-footpad-color i (color-mix (ix prev-led-footpad-color i) (ix target-led-footpad-color i)  blend-ratio))
        })
        (setix led-button-color 0 (color-mix (ix prev-led-button-color 0) (ix target-led-button-color 0)  blend-ratio))
    })

    (if (!= blinker-state 0) {
        (var blink-on (= (to-i (floor (mod (* anim-time 3.0) 2.0))) 0))
        (if (and blinker-prev-on (not blink-on)) {
            (setq blinker-flash-count (+ blinker-flash-count 1))
            (if (>= blinker-flash-count 4) (set-blinker 0))
        })
        (setq blinker-prev-on blink-on)
        (if (!= blinker-state 0) {
            (var blink-color (if blink-on 0x00FF9900 0x00000000))
            (if (> (length led-front-color) 1) {
                (var flen (length led-front-color))
                (var fhalf (/ flen 2))
                (var fstart (if (= blinker-state 1) fhalf 0))
                (var fend   (if (= blinker-state 1) flen fhalf))
                (looprange i fstart fend { (setix led-front-color i blink-color) })
            })
            (if (> (length led-rear-color) 1) {
                (var rlen (length led-rear-color))
                (var rhalf (/ rlen 2))
                (var rstart (if (= blinker-state 1) rhalf 0))
                (var rend   (if (= blinker-state 1) rlen rhalf))
                (looprange i rstart rend { (setix led-rear-color i blink-color) })
            })
        })
    })
})

(defun clear-leds () {
    (set-led-strip-color led-front-color 0x00)
    (set-led-strip-color led-rear-color 0x00)
    (set-led-strip-color led-footpad-color 0x00)
    (if (and (= led-front-strip-type 7) (>= led-front-highbeam-pin 0)) (pwm-set-duty 0.0 0))
    (if (and (= led-rear-strip-type 7) (>= led-rear-highbeam-pin 0)) (pwm-set-duty 0.0 1))
})
@const-end
