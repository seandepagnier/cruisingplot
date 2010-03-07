;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.


;; This file contains routines for computations in spherical coordinates

;; give the great circle distance
(define (spherical-distance lat1 lon1 lat2 lon2)
  (acos (+ (* (sin lat1) (sin lat2)) (* (cos lat1) (cos lat2) (cos (- lon1 lon2))))))

;; the initial heading for a great circle course from point 1 to point 2
(define (spherical-heading lat1 lon1 lat2 lon2)
  (remainder (atan (* (sin (- lon1 lon2)) (cos lat2))
                    (- (* (cos lat1) (sin lat2)) (* (sin lat1) (cos lat2) (cos (- lon1 lon2)))))
             (* 2 pi)))

;; return the latitude and longitude of the normal axis of the great circle
(define (spherical-great-circle lat1 lon1 lat2 lon2)
  1)

;; latitude on a great circle given longitude
(define (spherical-gc-latitude lat1 lon1 lat2 lon2 lon)
  (atan (/ (- (* (sin lat1) (cos lat2) (sin (- lon lon2)))
              (* (sin lat2) (cos lat1) (sin (- lon lon1))))
           (* (cos lat1) (cos lat2) (sin (- lon2 lon1))))))

;; give latitude and longitude if traveling
;; at a certain heading and distance from another position
(define (spherical-position-from-course lat lon tc d)
  (list
   (asin (+ (* (sin lat) (cos d)) (* (cos lat) (sin d) (cos tc))))
   (remainder (- lon (atan (* (sin tc) (sin d) (cos lat)
                              (- (cos d) (* (sin lat1) (sin lat))))))
              (* 2 pi))))

;; give the maximum latitude of a great circle given a latitude and course
;; derived from clairaut's formula:
;;  sin(course)*cos(lat) = constant for great circles
(define (spherical-great-circle-max-latitude-from-radial lat course)
  (acos (abs (* (sin course) (cos lat)))))

(define (spherical-coordinates-to-rectangular lat lon radius)
  1)

; Looking at the unit sphere
(define (spherical-draw-latitude-line lat steps)
  (glBegin GL_LINE_LOOP
           (let ((z (sin lat))
                 (r (cos lat))
                 (astep (/ (* 2 pi) steps)))
             (let each-angle ((a 0))
               (cond ((< a (* 2 pi))
                      (gl:Vertex3f (* r (sin a)) (* r (cos a)) z)
                      (each-angle (+ a astep))))))))

(define (spherical-draw-longitude-line lon steps)
  (gl:PushMatrix)
  (gl:Rotated lon 0 0 1)
  (glBegin GL_LINE_LOOP
           (let ((astep (/ (* 2 pi) steps)))
             (let each-angle ((a 0))
               (cond ((< a (* 2 pi))
                      (gl:Vertex3f 0 (* r (sin a)) (* r (cos a)))
                      (each-angle (+ a astep)))))))
  (gl:PopMatrix))
