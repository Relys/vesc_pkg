@const-start
(import "lib/utils.lisp" 'utils)
(read-eval-program utils)
(import "lib/pubmote.lisp" 'pubmote)
(read-eval-program pubmote)

(def wifi-enabled-on-boot true)
(def pubmote-context-id -1)
(def pubmote-enabled false)
(def can-id 0)

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
    (setq can-id (init-can fw-num cfg-can-id))
    ;(var can-id nil)
    (if can-id {
        (print (str-merge "Found can-id: " (str-from-n can-id)))
        (ext-set-can-id can-id)

        ;(spawn can-loop can-id)
        (setq pubmote-enabled (= (ext-cfg 4) 1))
        (if (= (conf-get 'wifi-mode) 0) {
            (setq wifi-enabled-on-boot false)
        })
        (event-register-handler (spawn event-handler))
        (event-enable 'event-esp-now-rx)
        (event-enable 'event-data-rx)
        (setq pubmote-context-id (spawn pubmote-loop))
    } {
        (print "No ESC found on CAN bus")
    })
})

(image-save)
(main)
@const-end