;@const-symbol-strings

@const-start
(def rainbow-colors '(0x00FF0000 0x00FFFF00 0x0000FF00 0x0000FFFF 0x000000FF 0x00FF00FF))
(def knight-rider-position 0)
(def knight-rider-direction 1)

(defun led-float-disabled (color-list) {
    (var led-num (length color-list))
    (var start (floor (/ led-num 4.0)))
    (var end (floor (* led-num 3 (/ 1 4.0))))
    ; Single loop for LEDs
    (looprange i 0 led-num {
        (if (and (>= i start) (< i end)) {
            (if (or (= i start) (= i (- end 1))) {
                (setix color-list i 0x007F0000)  ; Dimmed red for first and last
            }{
                (setix color-list i 0x00FF0000)  ; Full red for center
            })
        }{
            (setix color-list i 0x00000000)  ; Black for outer LEDs
        })
    })
})

(defun strobe-pattern (color-list strobe-index color) {
    (set-led-strip-color color-list (if (= strobe-index 0) color 0x00000000))
    (mod (+ strobe-index 1) 2)
})

(defun rave-pattern (color-list rave-index){
    (set-led-strip-color color-list (ix rainbow-colors rave-index))
    (mod (+ rave-index 1) (length rainbow-colors))
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

(defun rainbow-pattern (color-list rainbow-index) {
    (var num-colors (length rainbow-colors))
    (looprange led-index 0 (length color-list) {
        (var color-index (mod (+ rainbow-index led-index (length color-list)) num-colors))
        (var color (ix rainbow-colors color-index))
        (setix color-list led-index color)
    })
    (mod (+ rainbow-index 1) num-colors)
})

(defun felony-pattern (color-list felony-index) {
    (var felony-state (mod felony-index 3))
    (var led-num (length color-list))
    (var led-half (floor (/ led-num 2)))

    (cond
      ((= felony-state 0) {
       (looprange i 0 led-half
         (setix color-list i 0x00000000)) ; BLACK
       (looprange i led-half led-num
         (setix color-list i 0x00FF0000)) ; RED
      })

      ((= felony-state 1) {
       (looprange i 0 led-half
         (setix color-list i 0x00FF0000)) ; RED
       (looprange i led-half led-num
         (setix color-list i 0x000000FF)) ; BLUE
      })

      ((= felony-state 2) {
       (looprange i 0 led-half
         (setix color-list i 0x000000FF)) ; BLUE
       (looprange i led-half led-num
         (setix color-list i 0x00000000)) ; BLACK
      }))

    (mod (+ felony-index 1) 3)
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

(defun footpad-pattern (color-list switch-state){
    (var color-status-half1 (if (or (= switch-state 1) (= switch-state 3)) 0xFF 0x00))
    (var color-status-half2 (if (or (= switch-state 2) (= switch-state 3)) 0xFF 0x00))
    (looprange led-index 0 (length color-list) {
        (setix color-list led-index (if (<= led-index (/ (length color-list) 2)) color-status-half1 color-status-half2))
    })
})

(defun set-led-strip-color (color-list color) {
    (looprange led-index 0 (length color-list) {
        (setix color-list led-index color)
    })
})
@const-end