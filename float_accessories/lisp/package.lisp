
(defun main () {
    ;(sleep 30)
    (var fw-ver (sysinfo 'fw-ver))
    (var fw-num (+ (first fw-ver) (* (second fw-ver) 0.01)))
    (if (!= (str-cmp (to-str (sysinfo 'hw-type)) "hw-express") 0) {
        (exit-error "Not running on hw-express")
    })

    (if (< fw-num 6.06) (exit-error "hw-express needs to be running 6.06"))
    (print "Importing package lib...")
    (import "src/package_lib.bin" 'package-lib)
    (print "Imported package lib")
    (print "Loading native library...")
    (load-native-lib package-lib)
    (print "Loaded native library")
    ;(loopwhile (not (main-init-done)) {(sleep 0.1)})
    (ext-set-fw-version fw-ver)
    (print "Scanning for can-id")
    ;TODO We can try the last can-id used and make a function to help save it for the next time.
    (var cfg-can-id (ext-can-id))
    (var can-id (init-can fw-num cfg-can-id))
    ;(var can-id nil)
    (if can-id {
        (print (str-merge "Found can-id: " (str-from-n can-id)))
        (ext-set-can-ids can-id (get-bms-val 'bms-can-id))
        ;(event-register-handler (spawn event-handler))
        ;(event-enable 'event-data-rx)
        ;(spawn can-loop can-id)
        (ext-set-bms-info (fetch-series-cells can-id))
    } {
        (print "No ESC found on CAN bus")
    })
})

(def pitch-angle 0.0)
(def state-byte 0)
(def switch-state-byte 0)
(def footpad-adc1-t 0.0)
(def footpad-adc2-t 0.0)
(def rpm 0.0)
(def duty-cycle-now 0.0)
(def distance-abs 0.0)
(def battery-percent-remaining 0.0)
(defun can-loop (can-id) {

    (loopwhile t {
        (float-cmd can-id (list (assoc float-cmds 'COMMAND_GET_ALLDATA) 3))
        (print pitch-angle)
        (ext-update-data state-byte switch-state-byte footpad-adc1-t footpad-adc2-t rpm duty-cycle-now distance-abs battery-percent-remaining pitch-angle)
        (yield 1000000)
    })
})

(def FLOAT_MAGIC 101)
(def float-cmds '(
    (COMMAND_GET_INFO . 0)
    (COMMAND_GET_ALLDATA . 10)
    (COMMAND_HUMIDITY . 51)
))
(defun float-cmd (can-id cmd) {
    (send-data (append (list FLOAT_MAGIC) cmd) 2 can-id)
})

(defun event-handler ()
    (loopwhile t
        (recv
            ((event-data-rx . (? data)) (float-command-rx data))
            (_ nil)
        )
    )
)
(defun print-hex (data)
    (print
        (map (fn (x) (bufget-u8 data x)) (range (buflen data)))
    )
)
(defun float-command-rx (data) {
    ;(print-hex data)
    ;(print "hi")
    ;(ext-float-rx data)
    ; Only process data if data is long enough and magic number is correct
    (if (and (> (buflen data) 1) (= (bufget-u8 data 0) FLOAT_MAGIC)) {
        (match (cossa float-cmds (bufget-u8 data 1))
                (COMMAND_GET_ALLDATA {
                    (if  (> (buflen data) 3){
                            (var mode (bufget-u8 data 2))
                            (if (and (!= mode 69) (>= (buflen data) 32)) {
                                (setq state-byte (bufget-u8 data 9))
                                (setq switch-state-byte (bufget-u8 data 10))
                                (setq footpad-adc1-t (/ (to-float (bufget-u8 data 11)) 50))
                                (setq footpad-adc2-t (/ (to-float (bufget-u8 data 12)) 50))
                                (setq pitch-angle (/ (to-float (bufget-i16 data 19)) 10))
                                (setq rpm (to-float  (bufget-i16 data 24)))
                                (setq duty-cycle-now (/ (to-float (- (bufget-u8 data 32) 128)) 100))
                                (if (>= mode 2) {
                                    (setq distance-abs (bufget-f32 data 34))
                                })
                                (if (>= mode 3) {
                                    (setq battery-percent-remaining (/ (to-float (bufget-u8 data 53)) 200))
                                })
                            })

                    })
                })
                (_ nil)
            )
    })
})

(defunret init-can (fw-num cfg-can-id) {
    (var init-time (systime))
    (var can-devices (list))
    (loopwhile (<= (secs-since init-time) 20) {
        (cond
            ((<= (secs-since init-time) 10) { ;try saved can id first
                (if (>= cfg-can-id 0) {
                    (setq can-devices (list cfg-can-id))
                }{
                    (setq can-devices (can-list-devs))
                })
            })
            ((<= (secs-since init-time) 15) { ; then see if ESC is broadcasting status
                (setq can-devices (can-list-devs))
            })
            (_ (setq can-devices (can-scan))) ; finally resort to the congestive task of scanning the can bus.
        )
        (loopforeach can-id can-devices {
            ;check if can-ping is here and if so use that instead
            (var found-can-id nil)
            (var res (can-ping can-id))
            (setq found-can-id (and (not-eq res nil) (= res 0)))

            (if (eq found-can-id t) {
                (if (not (= can-id cfg-can-id)) {
                    (ext-can-id can-id)
                })
                (return can-id)
            })
            (sleep 0.5)
        })
        (sleep 0.5)
    })
    (return nil)
})

(defun fetch-series-cells (can-id) {
    (var cells 0)
    (if (>= can-id 0) {
        (if (> (get-bms-val 'bms-can-id) -1) (setq cells (get-bms-val 'bms-cell-num)))
        (if (= cells 0) {
            (print "No BMS info; querying ESC for series cells...")
            (var resp)

            ; Spawn thread to receive CAN response
            (loopwhile-thd 35 (eq resp nil) {
                (setq resp (canmsg-recv 0 5)) ; Blocks until message or 'timeout
            })

            ; Send ESC query
            (can-cmd can-id (str-merge
                "(progn "
                "(var resp (array-create 4)) "
                "(bufset-i32 resp 0 (conf-get 'si-battery-cells)) "
                "(canmsg-send " (str-from-n (can-local-id)) " 0 resp) "
                "(free resp))"
            ))

            ; Busy wait until thread updates response
            (loopwhile (eq resp nil) {
                (sleep 0.01) ; Light spin, prevent CPU hammering
            })

            ; Evaluate the response
            (if (not (eq resp 'timeout)) {
                (setq cells (bufget-i32 resp 0))
                (print (str-merge "Battery cells (from ESC): " (str-from-n cells)))
            } {
                (print "ESC query for series cells timed out")
            })
        } {
            (print (str-merge "Battery cells (from BMS): " (str-from-n cells)))
        })
    })
    cells
})

(main)