;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

(declare (unit units))

(define (mph->knots x) (* x .868976242))
(define (m/s->knots x) (* x 1.94384449))

(define (C->F x) (+ 32 (* 9 (/ x 5))))
(define (F->C x) (* (/ (- x 32) 9) 5))

(define (in->mm x) (* x 25.4))

(define Pi (* 2 (asin 1)))
(define (deg2rad x) (* Pi (/ x 180)))
(define (rad2deg x) (* 180 (/ x pi)))
