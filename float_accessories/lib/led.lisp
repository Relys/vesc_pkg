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
(def led-max-brightness)

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
(def mall-grab-start t)
(def mall-grab-button-timer 0)
(def mall-grab-event t)
(def front-pattern-index)
(def rear-pattern-index)
(def button-pattern-index)
(def footpad-pattern-index)
(def status-pattern-index)

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
    (setq led-rear-highbeam-pin (get-config 'led-front-highbeam-pin))
    (setq led-max-brightness (get-config 'led-max-brightness))
})

(defun init-led-vars () {
    (setq front-pattern-index 0)
    (setq rear-pattern-index 0)
    (setq button-pattern-index 0)
    (setq footpad-pattern-index 0)
    (setq status-pattern-index 0)
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
        (pwm-start 2000 0.0 0 led-front-highbeam-pin 12)
    })

    (if (and (= led-rear-strip-type 7) (>= led-rear-highbeam-pin 0)) {
        (pwm-start 2000 0.0 1 led-rear-highbeam-pin 12)
    })

    (var front-highbeam-leds 0)
    (var rear-highbeam-leds 0)
    (cond
        ((or (= led-front-strip-type 2) (= led-front-strip-type 3)) {
             (setq front-highbeam-leds (+ front-highbeam-leds 1))
        })
        ((or (= led-rear-strip-type 2) (= led-rear-strip-type 3)) {
             (setq rear-highbeam-leds (+ rear-highbeam-leds 1))
        })
        ((or (= led-front-strip-type 4) (= led-front-strip-type 5) (= led-front-strip-type 6)) {
             (setq front-highbeam-leds (+ front-highbeam-leds 4))
        })
        ((or (= led-rear-strip-type 4) (= led-rear-strip-type 5) (= led-rear-strip-type 6)) {
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
    (var next-run-time (secs-since 0))
    (var loop-start-time 0)
    (var loop-end-time 0)
    (var led-loop-delay-sec (/ 1.0 led-loop-delay))
    (var prev-direction 1)
    (var direction-change-start-time 0)
    (var direction-change-window 0.5)
    (loopwhile t {
        (setq loop-start-time (secs-since 0))

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

        (if (and (not (running-state)) (> pitch-angle 70)){
            (if (= led-mall-grab-enabled 1) (setq led-mall-grab 1) (setq led-mall-grab 0))
            (if (= switch-state 3){
                (if mall-grab-start {
                    (setq mall-grab-button-timer (systime))
                    (setq mall-grab-start nil)
                    (setq mall-grab-event t)
                })
                (if (>= (secs-since mall-grab-button-timer) 1) {
                    (if mall-grab-event {
                        (setq led-on (if (= led-on 1) 0 1))
                        (setq mall-grab-event nil)
                    })
                })
            }{
                (setq mall-grab-start t)
            })
        }{
            (setq led-mall-grab 0)
        })

        (if (or (running-state) (= led-mall-grab 1) (display-battery-charging)) {
            (setq led-last-activity-time (systime))
        }{
            (setq direction 1)
        })

        (update-leds (secs-since led-last-activity-time))
        (led-flush-buffers)

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
    (led-flush-buffers)
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
        ((or (= led-front-strip-type 2) (= led-front-strip-type 3)) {
            (if (and (<= led-dim-on-highbeam-brightness 0.0) (>= direction 0) (= led-on 1) (= led-highbeam-on 1) (running-state) (!= state 5)){
                (setq led-current-front-color (append (list front-color-highbeam) (mklist led-front-num 0)))
            }{
                (setq led-current-front-color (append (list front-color-highbeam) (take led-front-color led-front-num)))
            })
        })
        ((or (= led-front-strip-type 4) (= led-front-strip-type 5) (= led-front-strip-type 6)) {
            (var led-tmp (take led-front-color (length led-front-color)))
            (setq led-current-front-color (mklist (+ (length led-front-color) 4) 0))
            (var led-tmp-index 0)
            (setq led-current-brightness-front (+ 0.6 (* (if (= led-front-strip-type 4) 0.2 0.4) led-current-brightness-front))); Maps 0-1 to 0.60-1.0
            (looprange k 0 (length led-current-front-color){
                (if (or (and (or (= led-front-strip-type 4) (= led-front-strip-type 5)) (or (= k 2) (= k 7) (= k 13) (= k 18))) (and (= led-front-strip-type 6) (or (= k 1) (= k 4) (= k 10) (= k 13)))) {
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
        ((and (= led-front-strip-type 7) (>= led-front-highbeam-pin 0)) {
            (if front-highbeam-on (pwm-set-duty (min led-brightness-highbeam led-max-brightness) 0) (pwm-set-duty 0.0 0))
            (setq led-current-front-color led-front-color)
            (setq led-current-brightness-front led-current-brightness)
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
        ((or (= led-rear-strip-type 4) (= led-rear-strip-type 5) (= led-rear-strip-type 6)) {
            (var led-tmp (take led-rear-color (length led-rear-color)))
            (setq led-current-rear-color (mklist (+ (length led-rear-color) 4) 0))
            (var led-tmp-index 0)
            (setq led-current-brightness-rear (+ 0.6 (* (if (= led-rear-strip-type 4) 0.2 0.4) led-current-brightness-rear))) ; Maps 0-1 to 0.60-1.0
            (looprange k 0 (length led-current-rear-color){
                (if (or (and (or (= led-rear-strip-type 4) (= led-rear-strip-type 5)) (or (= k 2) (= k 7) (= k 13) (= k 18))) (and (= led-rear-strip-type 6) (or (= k 1) (= k 4) (= k 10) (= k 13) ))) {
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
        ((and (= led-rear-strip-type 7) (>= led-rear-highbeam-pin 0)) {
            (if rear-highbeam-on (pwm-set-duty (min led-brightness-highbeam led-max-brightness) 1) (pwm-set-duty 0.0 1))
            (setq led-current-rear-color led-rear-color)
            (setq led-current-brightness-rear led-current-brightness)
        })
        (_ {
            (setq led-current-rear-color led-rear-color)
            (setq led-current-brightness-rear led-current-brightness)
        })
    )
    (if (and (> led-button-strip-type 0) (>= led-button-pin 0)) {
        (rgbled-color led-button-buffer 0 led-button-color led-current-brightness)
        (rgbled-init led-button-pin 0)
        (yield led-fix)
        (rgbled-update led-button-buffer)
    })

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
                    (swap-rg led-status-color); Fix for avaspark rgb when there's different type like rgb and grb chained together.
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
                    (rgbled-color led-status-buffer 0 led-status-color (min led-brightness-status led-max-brightness))
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

(defun update-status-leds (can-last-activity-time-sec) {
    (if (or (= state 15) handtest-mode (>= can-last-activity-time-sec 1) (< can-id 0)) {
        (led-float-disabled led-status-color)
    }{
        (if (> rpm 250.0){
            (if (> sat-t 2) {
                (setq status-pattern-index (strobe-pattern led-status-color status-pattern-index 0x00FF0000))
            }{
                (duty-cycle-pattern led-status-color)
            })
        }{
            (if (and (!= switch-state 1) (!= switch-state 2) (!= switch-state 3)){
                ;(if (display-battery-charging) { TODO
                ;    ;Do something
                ;}{
                    (battery-pattern led-status-color)
                ;})
            }{
                (footpad-pattern led-status-color switch-state)
            })
        })
    })
})

(defun update-button-led () {
    (cond
        ((= led-mode-button 0) {
            (setq button-pattern-index (rainbow-pattern led-button-color button-pattern-index))
        })
    )
})

(defun update-leds (last-activity-sec) {
    (var can-last-activity-time-sec (secs-since can-last-activity-time))
    (if (> (length led-status-color) 0){
        (if (= led-mode-status 0) (update-status-leds can-last-activity-time-sec))
    })
    (var current-led-mode led-mode)
    (setq led-current-brightness (min led-brightness led-brightness led-max-brightness))
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
                        (setq footpad-pattern-index (rainbow-pattern led-foodpad-color foodpad-pattern-index))
                    })
                )
            })
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
                        (setq rear-pattern-index front-pattern-index)
                        (setq front-pattern-index (rainbow-pattern led-front-color front-pattern-index))
                        (setq rear-pattern-index (rainbow-pattern led-rear-color rear-pattern-index))
                    })
                    ((= current-led-mode 6) {
                        (setq rear-pattern-index front-pattern-index)
                        (setq front-pattern-index (strobe-pattern led-front-color front-pattern-index 0xFFFFFFFF))
                        (setq rear-pattern-index (strobe-pattern led-rear-color rear-pattern-index 0xFFFFFFFF))
                    })
                    ((= current-led-mode 7) {
                        (setq rear-pattern-index front-pattern-index)
                        (setq front-pattern-index (rave-pattern led-front-color front-pattern-index))
                        (setq rear-pattern-index (rave-pattern led-rear-color rear-pattern-index))
                    })
                    ((= current-led-mode 8) {
                        (set-led-strip-color led-front-color 0xFFFFFFFF)
                        (setq rear-pattern-index (rave-pattern led-rear-color rear-pattern-index))
                    })
                    ((= current-led-mode 9) {
                        (knight-rider-pattern)
                    })
                    ((= current-led-mode 10) {
                        (setq rear-pattern-index front-pattern-index)
                        (setq front-pattern-index (felony-pattern led-front-color front-pattern-index))
                        (setq rear-pattern-index (felony-pattern led-rear-color rear-pattern-index))
                    })
                )
                (if (and (= led-brake-light-enabled 1) (running-state) (!= state 5) (<= tot-current led-brake-light-min-amps)){
                    (if (>= direction 0){
                        (setq rear-pattern-index (strobe-pattern led-rear-color rear-pattern-index 0x00FF0000))
                    }{
                        (setq front-pattern-index (strobe-pattern led-front-color front-pattern-index 0x00FF0000))
                    })
                })

                (if (display-battery-charging) {
                    (battery-pattern led-front-color)
                    (battery-pattern led-rear-color)
                })
            })
        }{
            (clear-leds)
        })
        (update-button-led)
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
})

(defun clear-leds () {
    (set-led-strip-color led-front-color 0x00)
    (set-led-strip-color led-rear-color 0x00)
    (set-led-strip-color led-footpad-color 0x00)
    (if (and (= led-front-strip-type 7) (>= led-front-highbeam-pin 0)) (pwm-set-duty 0.0 0))
    (if (and (= led-rear-strip-type 7) (>= led-rear-highbeam-pin 0)) (pwm-set-duty 0.0 1))
})
@const-end
