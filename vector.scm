;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

; Not scheme vectors, these are used in calculations for movement
; a "course" is the polar version with heading and speed

(define (vector-magnitude vec)
  (sqrt (apply + (map square vec))))

(define (vector-normalize vec)
  (let ((mag (magnitude vec)))
    (map (lambda (x) (/ x mag)) vec)))

(define (vector-dot . vecs)
  (apply + (apply map * vecs)))

;  uh, yeah we support higher order cross products
(define  (vector-cross . vecs)
  (if ()))

;  These vector operations only apply to 2D vectors
(define (course->vector course)
  (list (* (car course) (cos (cadr course)))
        (* (car course) (sin (cadr course)))))

(define (vector->course vector)
  (list (vector-magnitude vector) (vector-heading vector)))

(define (vector-heading vector)
  (atan (cadr vector) (car vector)))
