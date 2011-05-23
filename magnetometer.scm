;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This file handles reading from a magnetometer (and optional accelerometer)
;; of the digitalsurveyinstruments design, perform calibration on the unit

(declare (unit magnetometer))
(declare (uses sensor))

(define (dsi-reader i o rate)
  (let ((accel-sensor-indexes (map sensor-new-index '(accelerometer accelerometer accelerometer)))
        (mag-sensor-indexes (map sensor-new-index '(magnetometer magnetometer magnetometer))))
;  (display "set /sensors/accel/outputrate " o)
;  (display rate o) (newline o)
;  (display "set /sensors/mag/outputrate " o)
;  (display rate o) (newline o)
  (let read-loop ()
    (make-line-reader
     (lambda () i)
     (lambda (line)
         (if (eof-object? line)
             (begin (verbose "magnetometer end of file: we will try to reconnect")
                    (let-values (((ni no) (try-opening-serial-devices
                                           '("/dev/ttyACM0" "/dev/ttyACM1" "/dev/ttyACM2" "/dev/ttyACM3")
                                           38400)))
                      (cond ((and ni no)
                             (set! i ni) (set! o no))
                            (else
                             (for-each (lambda (sensor sensor-indexes)
                                         (for-each (lambda (sensor-index)
                                                     (sensor-update (list sensor
                                                                          sensor-index) #f))
                                                   sensor-indexes))
                                       '(accelerometer magnetometer)
                                       (list accel-sensor-indexes mag-sensor-indexes))
                             (task-sleep 1)))))

             (let ((words (string-split line)))
               (if (= (length words) 4)
                   (let ((sensor (cond ((equal? (car words) "accel:")
                                        `(accelerometer ,accel-sensor-indexes))
                                       ((equal? (car words) "mag:")
                                        `(magnetometer ,mag-sensor-indexes))
                                       (else #f)))
                         (sensor-values (map string->number (cdr words))))
                     (if sensor
                         (for-each (lambda (sensor-value sensor-index)
                                     (sensor-update (list (first sensor) sensor-index) sensor-value))
                                   sensor-values (second sensor))
                         (warning-once "unrecognized sensor: " sensor)))))))))))


(define (magnetometer-setup device)
  (let-values (((i o) (open-serial-device device 38400)))
    (create-periodic-task "magnetomter task" .1
                          (lambda () (dsi-reader i o 8)))))

(computation-register 'magnetic-heading "The heading derived from the magnetometer" '(magnetometer)
                      (lambda () 0))

(computation-register
 'heading "The true heading derived from the magnetometer and declination, if no magnetometer is specified, gps heading is used" '(gps)
 (lambda ()
    (if (sensor-contains? 'magnetometer)
        (- (computation-calculate 'magnetic-heading)
           (computation-calculate 'declination))
        (begin (warning-once "using gps heading for heading, "
                             "this may be very inaccurate.")
               (computation-calculate 'gps-heading)))))

(computation-register 'magnetic-magnitude "The magnitude of the field" '(magnetometer)
                      (lambda ()
                        (magnitude (sensor-query 'magnetometer))))


; take pairs of x and y data for flat mag calibration
; (c*(x-a))^2 + (c*(d*(y-b) + e*(x-a)))^2 = 1
;
; a and b are bias for x and y axis   (initially set to average of data)
; c is overall scale   (initially 1/25000)
; d is relative scale difference between x and y   (initially 1)
; e is cross coupling term   (initially 0)
; all terms should be constant except e changes with local magnetic field strength

(define (flat-mag-apply calibration x y)
  (let-values (((a b c d e) (apply values (first (matrix->list (matrix-transpose calibration))))))
    (list
     (* c (- x a))
     (* c (+ (* d (- y b)) (* e (- x a)))))))

(define (build-flat-mag-jacobian-residual-row state measurement)
  (very-verbose "build-flat-mag-jacobian-residual-row: " state " " measurement)
  (let-values (((a b c d e) (apply values (first (matrix->list (matrix-transpose state)))))
               ((x y) (apply values measurement)))
    (let ((x-a (- x a))
          (y-b (- y b))
          (c^2 (* c c)))
    (list (list (+ (* -2 c^2 e (+ (* d y-b) (* e x-a)))
                   (* -2 c^2 x-a))
                (* -2 c^2 d (+ (* d y-b) (* e x-a)))
                (* 2 c (+ (square (+ (* d y-b) (* e x-a)))
                          (square x-a)))
                (* 2 c^2 y-b (+ (* d y-b) (* e x-a)))
                (* 2 c^2 x-a (+ (* d y-b) (* e x-a))))
          (- (+ (square (* c x-a))
                (square (* c (+ (* d y-b) (* e x-a))))
                -1))))))

(define (flat-mag-complete? state residuals)
  (let ((dev (apply rms (first (matrix->list (matrix-transpose residuals))))))
    (verbose "flat-mag-complete? dev: " dev)
    (if (> dev .1) #f #t)))

(define (flat-mag-calibrate mag-log)
  (let ((avg-x (apply average (map first mag-log)))
        (avg-y (apply average (map second mag-log))))
    (least-squares-iterate (matrix-transpose (matrix `((,avg-x ,avg-y ,(/ 25000) 1 0))))
                           build-flat-mag-jacobian-residual-row
                           mag-log flat-mag-complete? 10)))
