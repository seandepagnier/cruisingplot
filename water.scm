;; Copyright (C) 2011 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This file converts units from weather sensors and calculates
;; True and apparent wind speed in knots and direction in radians

(declare (unit water))

(use srfi-1)

(define (water-speed)
  (warning-once "no water speed data supported, assuming no current")
  (computation-calculate 'gps-speed))

(define (water-direction)
  (warning-once "no water speed data supported, assuming no current")
  (let ((heading (computation-calculate 'heading))
        (gps-heading (computation-calculate 'gps-heading)))
    (phase-resolve-degrees (- gps-heading heading))))

(define (water-heading)
  (+ (computation-calculate 'heading)
     (water-direction)))

;; calculate the true win heading and speed
;; apparent = course + true
(computation-register 'water-speed "speed in knots thru water"
                      '(water)
                      (lambda () (water-speed)))

(computation-register 'water-direction "relative direction water is flowing past us, indicates side flow (slip)"
                      '(water)
                      (lambda () (water-direction)))

(computation-register 'water-heading "heading we are actually moving through water"
                      '(water)
                      (lambda () (water-heading)))

; find the course of water over ground
(define (water-current)
  (let ((heading (computation-calculate 'heading))
        (water-speed (computation-calculate 'water-speed))
        (water-heading (computation-calculate 'water-heading))
        (gps-speed (computation-calculate 'gps 'speed))
        (gps-direction (computation-calculate 'gps 'heading)))
    (course- `(,water-speed ,water-heading)
             `(,gps-speed ,gps-heading))))

(computation-register 'water-current-speed "speed water is moving relative to ground"
                      '(gps)
                      (first (water-current)))

(computation-register 'water-current-direction "direction water is moving relative to ground"
                      '(gps)
                      (second (water-current)))

; the course we are moving through the water
(define (water-course)
  `(,(water-speed) ,(water-heading)))

; find the course of wind over water
(define (wind-course-over-water)
  (course- (true-wind) (true-water)))

(computation-register 'angle-of-attack
                      "The angle between the true wind and the movement through the water"
                      (lambda ()
                         (let ((wow (wind-course-over-water))
                               (wc (water-course)))
                           (phase-resolve-degrees
                            (- (second wow) (second wc)))))))

