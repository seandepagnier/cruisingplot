;; Copyright (C) 2011 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

(declare (unit wifiaimer))
(declare (uses computation utilities))

(use srfi-1 srfi-13 srfi-18 posix)

; given and update and calibration we solve for command
; the motor rotates the antenna 1 degree every 30 milliseconds,
; but it takes half milliseconds
(define (track-speed-calculate desired-update)
  (min 5000 (max -5000
                  (* desired-update 50))))

(define (create-wifi-aimer arg)
  (let ((scan-speed 200)
        (scan-direction -1)
        (statistics-timer (start-timer))
        (motor-command-total 0)
        (misalignment-total 0)
        (misalignment-count 0))

    (define (make-state-verifier)
      (make-discrete-verifier 'state "initial state" 'tracking '(scanning tracking)))
    
    (define options
      (create-options
       `(,(make-string-verifier 'wifidevice "wlan device to use" "wlan20")
         ,(make-number-verifier 'unwrap "if ccables get twisted do this to unwrap n counts" 0 -2 2)
         ,(make-number-verifier 'heading "heading to hold" 0 -180 180)
         ,(make-number-verifier 'beamwidth "tolerated heading mismatch" 8 0 180)
         ,(make-state-verifier))
       "-wifiaimer wifidevice=wlan0"
      (create-motor-options)))

    (define mag-calibration (matrix-transpose (matrix '((5551
                                                         -12832.0247508799
                                                         4.00245833922293e-05
                                                         0.963022408978104
                                                         -0.116278162422756)))))

    (define (scan-heading-min-max-from-headings headings)
      (let ((mins (map (lambda (heading) (if heading heading 180)) headings))
            (maxs (map (lambda (heading) (if heading heading -180)) headings)))
        (let ((min-heading (apply min mins))
              (max-heading (apply max maxs)))
          (cond ((> (- max-heading min-heading) 180)
                 (list max-heading (+ min-heading 360)))
                ((> min-heading max-heading) #f)
                (else (list min-heading max-heading))))))

                                        ; return association status
    (define (read-wifi-info)
      (let ((wifidevice (options 'wifidevice))
            (tempfilename "/tmp/cruisingplotiwconfig"))
        (system (string-append "bash -c \"iwconfig " wifidevice " &> " tempfilename "\""))
        (let ((winfo (read-all tempfilename)))
          (cond ((string-contains winfo "No such device")
                 (warning-once "wifi device: " wifidevice " does not exist")
                 #f)
                ((string-contains winfo "Not-Associated") #f)
                (else #t)))))

    (define (apply-heading mag-x mag-y)
      (let ((cal (flat-mag-apply mag-calibration mag-x mag-y)))
        (rad2deg (atan (second cal) (first cal)))))

    (if (equal? arg "help") (wifiaimer-help))
    (parse-basic-options-string options arg)

    (let ((motor (motor-open options))
          (motor-position 0))
      (define (get-heading)
        (let ((heading (computation-calculate 'heading)))
          (cond (heading heading)
                (else (warning-once "heading not calculated, assuming stationary")
                      0))))
      (start-periodic-task
       "wifi aimer task" 1
       (let ((scan-start #f)
             (min-associated-mag-x #f)
             (max-associated-mag-y #f)
             (min-associated-mag-x #f)
               (max-associated-mag-y #f)
               (mag-log '())
               (state (case (options 'state)
                        ((scanning) 'start-scan)
                        ((tracking) 'tracking)))
               (antenna-heading 0)
               (last-antenna-heading 0)
               (antenna-heading-wraps 0)
               (scan-heading (options 'heading))
               )
           (lambda ()
             (let ((associated (read-wifi-info))
                   (mag-x (sensor-query '(magnetometer 0)))
                   (mag-y (sensor-query '(magnetometer 1))))
               (verbose "Wifiaimer associated: " associated)
               (very-verbose "maglog " mag-log)
                                        ; statistics
               (cond ((>= (statistics-timer) 60)
                      (print "\nwifiaimerstats: movement "
                             (round-to-places (/ motor-command-total 1000) 2)
                             " seconds or "
                             (round-to-places (/ motor-command-total (statistics-timer) 10) 2)
                             "% of time")
                      (print "average misalignment: "
                             (if (zero? misalignment-count) "N/A"
                                 (round-to-places (/ misalignment-total misalignment-count) 2))
                             " degrees")
                      (set! misalignment-total 0)
                      (set! misalignment-count 0)
                      (set! motor-command-total 0)
                      (set! statistics-timer (start-timer))))

               (case state
                 ((start-scan) ; move motor to start of scan, and wait until it is there
                  (set! scan-start (start-timer))
                  (set! min-associated-mag-x #f)
                  (set! min-associated-mag-y #f)
                  (set! max-associated-mag-x #f)
                  (set! max-associated-mag-y #f)
                  (set! mag-log '())
                  (set! scan-direction (- scan-direction))
                  (set! state 'scanning))
                 ((scanning) 
                  (if (>= (scan-start) 120)
                      (set! state 'scan-complete))
                  (set! motor-command-total (+ motor-command-total (abs command)))
                  (motor-command motor (* scan-speed scan-direction))
                  (cond ((and associated mag-x mag-y)
                         (cond ((or (not min-associated-mag-x) (< mag-x min-associated-mag-x))
                                (set! min-associated-mag-x (list mag-x mag-y)))
                               ((< mag-y min-associated-mag-y)
                                (set! min-associated-mag-y (list mag-x mag-y)))
                               ((> mag-x max-associated-mag-x)
                                (set! max-associated-mag-x (list mag-x mag-y)))
                               ((> mag-y max-associated-mag-y)
                                (set! max-associated-mag-y (list mag-x mag-y))))))

                  (if (and mag-x mag-y)
                      (set! mag-log (cons (list mag-x mag-y) mag-log))))
                 ((scan-complete)
                  (print "Wifiaimer scan complete")
                  (set! mag-calibration (flat-mag-calibrate mag-log))
                  (print "mag-calibration: " mag-calibration)
                  (let ((headings
                         (map (lambda (mag-xy) (if mag-xy
                                                   (map (lambda (mag-xy)
                                                          (apply apply-heading mag-xy))
                                                        (apply flat-mag-apply mag-calibration mag-xy))
                                                   #f))
                              `(,min-associated-mag-x
                                ,min-associated-mag-y
                                ,max-associated-mag-x
                                ,max-associated-mag-y))))
                    (verbose "headings " headings)
                    (let ((heading-min-max (scan-heading-min-max-from-headings headings)))
                      (cond (heading-min-max
                             (verbose "Detected wifi heading range: " heading-min-max)
                             (set! scan-heading (apply average heading-min-max)))))
                    (cond (scan-heading
                           (set! state 'tracking))
                          (else
                           (print "Wifiaimer scan failed, repeating")
                           (set! state 'start-scan)))))
                 ((tracking)
                  (if (and mag-x mag-y)
                      (let ((antenna-heading (apply-heading mag-x mag-y)))
                        (verbose "wifiaimer antenna-heading " antenna-heading " wraps " antenna-heading-wraps)

                                        ; calculate error, and update motor to correct it
                        (let ((antenna-total-heading
                               (+ antenna-heading (* antenna-heading-wraps 360))))
                          (let ((antenna-misalignment (- antenna-total-heading scan-heading)))
                            (show "Wifiaimer antenna " (round-to-places antenna-total-heading 2)
                                  " misalignment " (round-to-places antenna-misalignment 2))
                            (set! misalignment-total (+ misalignment-total (abs antenna-misalignment)))
                            (set! misalignment-count (+ misalignment-count 1))

                            (if (> (abs antenna-misalignment) (options 'beamwidth))
                                (let ((command
                                       (track-speed-calculate antenna-misalignment)))
                                  (motor-command command)))))
                                        ; did we wrap?  record it
                        (cond ((and (> antenna-heading 90) (< last-antenna-heading -90))
                               (set! antenna-heading-wraps (+ antenna-heading-wraps 1)))
                              ((and (< antenna-heading -90) (> last-antenna-heading 90))
                               (set! antenna-heading-wraps (- antenna-heading-wraps 1))))
                        (set! last-antenna-heading antenna-heading)))
                  )))))
       'wifi-aimer))))
