;; Copyright (C) 2009 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This file converts units from weather sensors and calculates
;; True and apparent wind speed in knots and direction in radians

(declare (unit wind))

(use srfi-1)

; with some simple filters it is easier to read plots
; because it is clustered near a value (close but not identical)
(define (make-low-pass-filter gamma)
  (let ((last #f))
    (lambda (update)
      (set! last (if last
                     (+ (* gamma update) (* (- 1 gamma) last))
                     update))
      last)))

(define (make-directional-low-pass-filter)
  (let ((last #f))
    (lambda (update)
      (let ((next (phase-resolve-degrees update)))
        (set! last (if last
                       (rad2deg (car (wind-direction-Yamartino-method
                                      (list (deg2rad last)
                                            (deg2rad next)))))
                       next)))
      last)))


;; calculate the true wind direction and speed
;; apparent = course + true
(computation-register 'wind-speed "speed in knots of apparant wind"
                      '(weather)
                      (lambda ()
                        (sensor-field-get 'weather 'wind-speed)))

(computation-register 'wind-direction "direction in degrees of apparant wind, relative to heading" '(weather)
                      (lambda ()
                        (sensor-field-get 'weather 'wind-direction)))

; find the course wind is taking over ground
(define (calculate-true-wind heading wind-speed wind-direction gps-speed gps-heading)
  (course- `(,wind-speed ,(+ heading wind-direction))
           `(,gps-speed ,gps-heading)))

(define (current-true-wind)
  (calculate-true-wind (computation-calculate 'heading)
             (computation-calculate 'wind-speed)
             (computation-calculate 'wind-direction)
             (computation-calculate 'gps-speed)
             (computation-calculate 'gps-heading)))

(computation-register 'true-wind-speed "The true wind speed in knots." '(gps weather)
                      (lambda ()
                        (first (true-wind))))

(computation-register 'true-wind-heading "The true wind heading in degrees." '(gps weather)
                      (lambda ()
                        (second (true-wind))))

(computation-register 'true-wind-direction "The true wind direction relative to heading in degrees."
                      '(gps weather)
                      (lambda ()
                        (- (computation-calculate 'true-wind-heading) (computation-calculate 'heading))))

; calculate average wind direction and deviation from a list of
; wind directions (in radians) using the Yamartino method
(define (wind-direction-Yamartino-method directions)
  (let ((sa (/ (fold + 0 (map sin directions)) (length directions)))
        (ca (/ (fold + 0 (map cos directions)) (length directions))))
    (list
     (atan sa ca)
     (let ((e (sqrt (- 1 (* sa sa) (* ca ca)))))
       (* (asin e) (+ 1 (* (- (/ 2 (sqrt 3)) 1) (cube e))))))))


; we can smooth wind direction to get more useful measurement using the magnetic
; heading to improve bandwidth
;
;  we need proper filter for heading with resolver
; truewinddirectionsmoothed = tc*(magneticheading+winddirection) + (1-tc)*truewinddirectionsmoothed
;
; then we can achieve wind direction bandwidth as fast as magnetometer heading
; winddirectionmagneticbandwidthsmoothed = truewinddirectionsmoothed - magneticheading



