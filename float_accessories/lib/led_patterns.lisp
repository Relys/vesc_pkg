;@const-symbol-strings
@const-start
(def rainbow-colors '(0xFF0000u32 0xFFFF00u32 0x00FF00u32 0x00FFFFu32 0x0000FFu32 0xFF00FFu32))
(def rave-colors '(0x00FFFF00 0x0000FF00 0x0000FFFF 0x000000FF 0x00FF00FF 0x00FF0000))
(def strobe-index 0)
(def brake-index 0)
(def rave-index 0)
(def knight-rider-position 0)
(def knight-rider-direction 1)
(def rainbow-index 0)
(def rainbow-button-index 0)
(def rainbow-footpad-index 0)
(def felony-index 0)

(defun led-float-disabled (led-color) {
    (var led-num (length led-color))
    (var start (floor (/ led-num 4.0)))
    (var end (floor (* led-num 3 (/ 1 4.0))))
    ; Single loop for LEDs
    (looprange i 0 led-num {
        (if (and (>= i start) (< i end)) {
            (if (or (= i start) (= i (- end 1))) {
                (setix led-color i 0x007F0000)  ; Dimmed red for first and last
            }{
                (setix led-color i 0x00FF0000)  ; Full red for center
            })
        }{
            (setix led-color i 0x00000000)  ; Black for outer LEDs
        })
    })
})

(defun strobe-pattern () {
    (setq strobe-index (mod (+ strobe-index 1) 2))
    (var color (if (= strobe-index 0) 0xFFFFFFFF 0x00000000))
    (set-led-strip-color led-front-color color)
    (set-led-strip-color led-rear-color color)
})
(defun brake-pattern (color-list) {
    (setq brake-index (mod (+ brake-index 1) 2))
    (set-led-strip-color color-list (if (= brake-index 0) 0x00FF0000 0x00000000))
})

(defun rave-pattern (type){
    (var current-color (ix rave-colors rave-index))
    (set-led-strip-color led-front-color (if (= type 0) current-color 0xFFFFFFFF))
    (set-led-strip-color led-rear-color current-color)
    (setq rave-index (mod (+ rave-index 1) 6))
})

(defun knight-rider-pattern (){
    (var total-leds (+ (length led-front-color) (length led-rear-color)))
    (looprange i 0 total-leds {
        (var distance (abs (- i knight-rider-position)))
        (var intensity (max 0 (- 255 (* distance 51))))
        (var color (color-make intensity 0 0))
        (if (< i (length led-front-color))
            (setix led-front-color i color)
            (setix led-rear-color (- i (length led-front-color)) color))
        })
    (setq knight-rider-position (+ knight-rider-position knight-rider-direction))
    (if (or (>= knight-rider-position total-leds) (< knight-rider-position 0))
    (setq knight-rider-direction (* knight-rider-direction -1)))
})

(defun battery-pattern (color-list) {
    (var led-num (length color-list))
    (var num-lit-leds (floor (* led-num battery-percent-remaining)))

    (looprange led-index 0 led-num {
        (var color
            (if (or (< led-index num-lit-leds)
                   (and (= led-index 0) (<= num-lit-leds 1))) {
                ; LED should be lit
                (if (or (< battery-percent-remaining 0.2)
                       (and (= led-index 0) (<= num-lit-leds 1))) {
                    ; Low battery - red color
                    (color-make 255 0 0)
                } {
                    ; Normal battery - gradient from green to yellow to red
                    (let ((red-ratio (- 1 (/ battery-percent-remaining 0.8)))
                          (green-ratio (/ battery-percent-remaining 0.8))) {
                        (color-make
                            (* 255 red-ratio)
                            (* 255 green-ratio)
                            0)
                    })
                })
            } {
                ; LED should be off
                (color-make 0 0 0)
            }))
        (setix color-list led-index color)
    })
})

(defun rainbow-pattern () {
    (var num-colors (length rainbow-colors))
    (looprange led-index 0 (length led-front-color) {
        (var color-index (mod (+ rainbow-index led-index (length led-front-color)) num-colors))
        (var color (ix rainbow-colors color-index))
        (setix led-front-color led-index color)
    })
    (looprange led-index 0 (length led-rear-color) {
        (var color-index (mod (+ rainbow-index led-index (length led-rear-color)) num-colors))
        (var color (ix rainbow-colors color-index))
        (setix led-rear-color led-index color)
    })
    (setq rainbow-index (mod (+ rainbow-index 1) num-colors))
})

(defun rainbow-button (){
    (var num-colors (length rainbow-colors))
    (var color-index (mod (+ rainbow-button-index 1) num-colors))
    (var color (ix rainbow-colors color-index))
    (setix led-button-color 0 (ix rainbow-colors color-index))
    (setq rainbow-button-index (mod (+ rainbow-button-index 1) num-colors))
})

(defun rainbow-footpad (){
    (var num-colors (length rainbow-colors))
    (looprange led-index 0 (length led-footpad-color) {
        (var color-index (mod (+ rainbow-index led-index (length led-footpad-color)) num-colors))
        (var color (ix rainbow-colors color-index))
        (setix led-footpad-color led-index color)
    })
    (setq rainbow-footpad-index (mod (+ rainbow-index 1) num-colors))
})

(defun felony-pattern () {
    (var felony-state (mod felony-index 3))
    (var led-front-num (length led-front-color))
    (var led-rear-num (length led-rear-color))
    (var forward-half (floor (/ led-front-num 2)))
    (var rear-half (floor (/ led-rear-num 2)))

    (cond
      ((= felony-state 0) {
       (looprange i 0 forward-half
         (setix led-front-color i 0x00000000)) ; BLACK
       (looprange i forward-half led-front-num
         (setix led-front-color i 0x00FF0000)) ; RED
       (looprange i 0 rear-half
         (setix led-rear-color i 0x00000000)) ; BLACK
       (looprange i rear-half led-rear-num
         (setix led-rear-color i 0x00FF0000)) ; RED
      })

      ((= felony-state 1) {
       (looprange i 0 forward-half
         (setix led-front-color i 0x00FF0000)) ; RED
       (looprange i forward-half led-front-num
         (setix led-front-color i 0x000000FF)) ; BLUE
       (looprange i 0 rear-half
         (setix led-rear-color i 0x00FF0000)) ; RED
       (looprange i rear-half led-rear-num
         (setix led-rear-color i 0x000000FF)) ; BLUE
      })

      ((= felony-state 2) {
       (looprange i 0 forward-half
         (setix led-front-color i 0x000000FF)) ; BLUE
       (looprange i forward-half led-front-num
         (setix led-front-color i 0x00000000)) ; BLACK
       (looprange i 0 rear-half
         (setix led-rear-color i 0x000000FF)) ; BLUE
       (looprange i rear-half led-rear-num
         (setix led-rear-color i 0x00000000)) ; BLACK
      }))

    (setq felony-index (mod (+ felony-index 1) 3))
})

(defun duty-cycle-pattern (color-list) {
    (var scaled-duty-cycle (* (abs duty-cycle-now) 1.1112))
    (var clamped-duty-cycle 0.0)

    (if (< scaled-duty-cycle 1.0) {
        (setq clamped-duty-cycle scaled-duty-cycle)
    } {
        (setq clamped-duty-cycle 1.0)
    })
    (var led-num (length color-list))
    (var duty-leds (floor (* clamped-duty-cycle led-num)))

    (var duty-color 0x00FFFF00u32)

    (if (> (abs duty-cycle-now) 0.85) {
        (setq duty-color 0x00FF0000u32)
    } {
        (if (> (abs duty-cycle-now) 0.7) {
            (setq duty-color 0x00FF8800u32)
        })
    })

    (looprange led-index 0 led-num {
        (setix color-list led-index (if (< led-index duty-leds) duty-color 0x00000000u32))
    })
})

(defun footpad-pattern (switch-state){
    (var color-status-half1 (if (or (= switch-state 1) (= switch-state 3)) 0xFF 0x00))
    (var color-status-half2 (if (or (= switch-state 2) (= switch-state 3)) 0xFF 0x00))
    (looprange led-index 0 led-status-num {
        (setix led-status-color led-index (if (< led-index (/ led-status-num 2)) color-status-half1 color-status-half2))
    })
})

(defun set-led-strip-color (led-color color) {
    (looprange led-index 0 (length led-color) {
        (setix led-color led-index color)
    })
})
@const-end