;; Copyright (C) 2009 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This file converts units from weather sensors and calculates
;; True and apparent wind speed in knots and direction in radians

(use srfi-1)

; with some simple filters it is easier to read plots
; because it is clustered near a value (close but not identical)
(define (make-low-pass-filter gamma)
  (let ((last #f))
    (lambda (update)
      (set! last (if last
                     (+ (* gamma speed) (* (- 1 gamma) last))
                     speed))
      last)))

;; calculate the true wind direction and speed
;; apparent = course + true
(computation-register 'wind-speed "speed in knots of apparant wind"
                      (let ((filter (make-low-pass-filter .5)))
                        (lambda ()
                          (filter (sensor-field-get 'weather 'wind-speed)))))

(computation-register 'relative-wind-direction "relative direction in degrees on vessel"
                      (lambda ()
                        (sensor-field-get 'weather 'wind-direction)))

(computation-register 'wind-direction "direction in degrees of apparant wind,
after heading is subtracted"
                      (let ((c (computations-revaluate 'relative-wind-direction 'heading)))
                        (lambda ()
                          (sensor-field-get 'weather 'wind-direction))))

; compute true wind speed and direction
(define (true-wind)
  (let ((wind-speed (sensor-field-get 'weather 'wind-speed))
        (wind-direction ((computation-revaluate 'wind-direction)))
        (gps-speed (sensor-field-get 'gps 'speed))
        (gps-direction (deg2rad (sensor-field-get 'gps 'heading))))
    (let ((wx (* wind-speed (cos wind-direction)))
          (wy (* wind-speed (sin wind-direction)))
          (gx (* gps-speed (cos gps-direction)))
          (gy (* gps-speed (sin gps-direction))))
      (let ((sx (- wx gx))
            (sy (- wy gy)))
        (list (magnitude (list sx sy)) (rad2deg (atan sy sx)))))))

(computation-register 'true-wind-speed "The true wind speed in knots."
                      (lambda ()
                        (first (true-wind))))

(computation-register 'true-wind-direction "The true wind direction in degrees."
                      (lambda ()
                        (second (true-wind))))

; calculate average wind direction and deviation from a list of
; wind directions (in radians) using the Yamartino method
(define (wind-direction-Yamartino-method directions)
  (let ((sa (/ (fold + 0 (map sin directions)) (length directions)))
        (ca (/ (fold + 0 (map cos directions)) (length directions))))
    (list
     (atan sa ca)
     (let ((e (sqrt (- 1 (* sa sa) (* ca ca)))))
       (* (asin e) (+ 1 (* (- (/ 2 (sqrt 3)) 1) (* e e e))))))))
