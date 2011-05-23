;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

; Not scheme vectors, these are used in calculations for movement
; a "course" is the polar version with heading and speed

(define (vector-magnitude^2 vec)
  (apply + (map square vec)))

(define (vector-magnitude vec)
  (sqrt (vector-magnitude^2 vec)))

(define (vector-normalize vec)
  (let ((mag (vector-magnitude vec)))
    (map (lambda (x) (/ x mag)) vec)))

(define (vector+ . vecs)
  (apply map + vecs))

(define (vector- . vecs)
  (apply map - vecs))

(define (vector-scale factor v)
  (map (lambda (w) (* factor w)) v))

(define (vector-dot . vecs)
  (apply + (apply map * vecs)))

; we support all dimensions of cross products
; 1 2d vector given will yield an orthogonal vector
; two 3d vectors can be supplied to gain a third orthogonal, or
; three 4d vectors etc...
(define  (vector-cross . vecs)
  (if (not (apply = (+ (length vecs) 1) (map length vecs)))
      (error "invalid dimensions given to vector-cross, requires n vectors of n+1 dimension" vecs))
  (let ((m (matrix vecs)))
    (map (lambda (col)
           (* (if (even? col) 1 -1)
              (matrix-determinant (matrix-remove-col m col))))
         (sequence 0 (length vecs)))))

;  These vector operations only apply to 2D vectors
(define (course->vector course)
  (list (* (car course) (cos (deg2rad (cadr course))))
        (* (car course) (sin (deg2rad (cadr course))))))

(define (vector->course vector)
  (list (vector-magnitude vector) (vector-heading vector)))

(define (vector-heading vector)
  (rad2deg (atan (cadr vector) (car vector))))

(define (course+ . courses)
  (vector->course (apply vector+ (map course->vector courses))))

(define (course- . courses)
  (vector->course (apply vector- (map course->vector courses))))

(define (vector->row-matrix v)
  (matrix (map list v)))

(define (row-matrix->vector m)
  (map first (matrix->list m)))
