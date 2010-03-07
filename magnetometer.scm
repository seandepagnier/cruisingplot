;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This file handles reading from a magnetometer (and optional accelerometer)
;; of the digitalsurveyinstruments design, perform calibration on the unit

(type-register 'accelerometer '(x y z))
(type-register 'magnetometer '(x y z))

(define (dsi-reader i o rate)
  (display "set /sensors/accel/outputrate " o)
  (display rate o) (newline o)
  (display "set /sensors/mag/outputrate " o)
  (display rate o) (newline o)
  (let read-loop ()
    (make-line-reader
     i (lambda (line)
         (let ((words (string-split line)))
           (if (= (length words) 4)
               (case (car words)
                 (("accel:") 
                  (sensor-update 'accelerometer (map string->number (cdr words))))
                 (("mag:") 
                  (sensor-update 'magnetometer (map string->number (cdr words)))))))))))

(define (magnetometer-setup device)
  (let-values (((i o) (open-serial-device device 38400)))
    (thread-start! (lambda () (dsi-reader i o 2)))))


(computation-register 'magnetic-heading "The heading derived from the magnetometer"
                      (lambda () 0))

(computation-register
 'heading "The true heading derived from the magnetometer and declination, if no magnetometer is specified, gps heading is used, with a warning when below 5knots"
 (if (sensor-contains? 'magnetometer)
     (let ((c (computations-revaluate 'magnetic-heading 'declination)))
       (lambda () (apply - (c))))
     (let ((c (computations-revaluate 'gps-heading 'gps-speed)))
       (lambda () (let ((d (c)))
                    (if (< (second d) 5) (warning-once "using gps heading for heading, "
                                                       "but gps speed is low so it may "
                                                       "be very inaccurate."))
                    (first d))))))

(computation-register 'magnetic-magnitude "The magtitude of the field"
                      (lambda ()
                        (magnitude (sensor-query 'magnetometer))))

(computation-register 'force "The magtitude of the accelerometers combined force, non-moving yields 1 (for 1 gravity)"
                      (lambda ()
                        (magnitude (sensor-query 'accelerometer))))

