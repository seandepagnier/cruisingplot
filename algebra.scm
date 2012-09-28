;; Copyright (C) 2009, 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

(declare (unit algebra))

(declare (uses srfi-1))

; some linear algebra functions
(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (magnitude vec)
  (sqrt (apply + (map square vec))))

(define (distance vec1 vec2)
  (magnitude (map - vec1 vec2)))

(define (normalize vec)
  (let ((mag (magnitude vec)))
    (map (lambda (x) (/ x mag)) vec)))

(define (dot . vecs)
  (apply + (apply map * vecs)))

(define average (lambda args (/ (fold + 0 args) (length args))))
(define average-list (lambda (arg) (/ (fold + 0 arg) (length arg))))

(define rms (lambda args (sqrt (apply average (map square args)))))

(define Pi (* 2 (asin 1)))
(define (r2d x) (* (/ 180 Pi) x))
(define (d2r x) (* (/ Pi 180) x))

; from +=- Pi
(define (phase-resolve phase)
  (let ((phase (remainder phase (* 2 Pi))))
    (cond ((< phase (- Pi)) (+ phase (* 2 Pi)))
          ((> phase Pi) (- phase (* 2 Pi)))
          (else phase))))

; from 0 to 2Pi
(define (phase-resolve-positive phase)
  (let ((phase (remainder phase (* 2 Pi))))
    (+ phase (if (negative? phase) (* 2 Pi) 0))))

; from +- 180
(define (phase-resolve-degrees phase)
  (r2d (phase-resolve (d2r phase))))

; from 0 to 360
(define (phase-resolve-positive-degrees phase)
  (r2d (phase-resolve-positive (d2r phase))))

; calculate heading difference, if p2 is false
; there is no difference (for initial conditions)
(define (phase-difference p1 p2 period)
  (if p2
      (let eachd ((d (- p1 p2)))
        (cond ((> d (/ period 2)) (eachd (- d period)))
              ((< d (- (/ period 2))) (eachd (+ d period)))
              (else d)))
      0))

(define (phase-difference-degrees p1 p2)
  (phase-difference p1 p2 360))

(define (newtons-method state build-jacobian-residual-row iterations)
  (let each-iteration ((state state)
                       (iteration 0))
    (if (= iteration iterations) state
    (let ((HZ (build-jacobian-residual-row state)))
      (let ((H (first HZ))
            (Z (second HZ)))
        (each-iteration (map (lambda (state diff)
                               (- state (if (= 0 diff)
                                            (begin (print "reached local minima at "
                                                          state " nudge .01") -.01)
                                            (/ Z (abs diff)))))
                             state H)
                        (+ iteration 1)))))))

(define (solve-quadratic-method a b c)
  (/ (- (sqrt (- (* b b) (* 4 a c))) b) (* 2 a)))

; 0 = a*x^2 + b*x + c
(define (solve-quadratic initial-value a b c)
  (first (newtons-method `(,initial-value) (lambda (state) (let ((x (first state)))
                                                             (list (list (+ (* 2 a x) b))
                                                                   (+ (* a x x) (* b x) c))))
                         100)))

; 0 = a*x^3 + b*x^2 + c*x + d
(define (solve-cubic initial-value a b c d)
  (first (newtons-method `(,initial-value) (lambda (state) (let ((x (first state)))
                                                             (list (list (+ (* 3 a x) (* 2 b x) c))
                                                                   (+ (* a x x x) (* b x x) (* c x) d))))
                         100)))

(define (integrator pos step derivative)
  (* step (derivative pos)))

(define (runge-kutta-2 pos step derivative)
  (* step (/ (+ (derivative pos)
                (derivative (+ pos step))) 2)))

;(define (runge-kutta-4 pos step derivative)

