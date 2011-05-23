;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version. 

(declare (unit kalman))
; In the kalman filter, the following matrices are used
; with these dimensions:
;  n = number of states
;  m = number of measurements
;
; A: nxn BK: nxm  P: nxn  H: mxn  R: mxm  Z: mx1  X: nx1

(define (kalman-state name initial-value initial-covarience
                      time-update-calculation measurement-update-calculation)
  (list value covarience
        (list name initial-value initial-covarience
              time-update-calculation measurement-update-calculation)))

; reset a stat to initial values
(define (kalman-state-reset state)
  (let ((s (third state)))
    (list (second s) (third s) s)))

; return the name of a state
(define (kalman-state-name state)
  (first (third state)))

; Create a kalman measurement where calculation is a thunk
; returning the measurement

; kalman calculations take the input value as well as the states
(define (kalman-measurement name calculation

; create a kalman filter from these states and measurements
(define (kalman-filter states measurements)
  (let ((n (length states))
        (m (length measurements)))
    (append `(,states measurements)
    (if (null? prev)
        

    (define (kalman-time-update filter Q)
; X = AX + BU
; P = APA' + Q
      (let* ((X (matrix+ (matrix* A X) (matrix* B U)))
             (P (matrix+ (matrix* A P (matrix-transpose A)) Q)))

    (define (kalman-measurement-update filter H V R Z)
 ; K = PH'(HPH' + VRV')^-1
 ; X = X + K*Z
 ; P = (I - KH)P(I - KH)' + KRK'
      (let* ((K (matrix* P (matrix-transpose H)
                         (matrix-invert (matrix+ (matrix* H P (matrix-transpose H))
                                                 (matrix* V R (matrix-transpose V)))))))
             (X (matrix+ X (matrix* K Z)))
             (d (matrix- (matrix-identity n) (matrix* K H)))
             (P (matrix+ (matrix* d P (matrix-transpose d)) (matrix* K R (matrix-transpose K)))))

  
(define (make-kalman-state initial-value process-noise time-update)
  (list initial-value process-noise time-update))

; update
(define (make-kalman-measurement measurement-update)


(define (unscented-scale alpha kappa L)
  (- (* alpha alpha (+ L kappa)) L))

; W(0) = scale / (scale + L)
; W(i) = 1 / (2 * (scale + L))   i = 1..2L
(define (unscented-mean-weights alpha beta kappa L)
  (cons (/ scale (+ L scale))
        (make-list (* 2 L) (/ (* 2 (+ L scale))))))

; W(0) = scale / (scale + L) + (beta - alpha^2 + 1)
; W(i) = 1 / (2 * (scale + L))   i = 1..2L
(define (unscented-covariance-weights alpha beta kappa L)
  (cons (+ (/ scale (+ L scale)) (- beta (square alpha)) 1)
        (make-list (* 2 L) (/ (* 2 (+ L scale))))))

(define (unscented-mean sigma-points)
  (let*((L (length sigma-points))
        (weights (unscented-mean-weights alpha beta kappa L)))
    (fold x
; scale matrix square root by (alpha^2*(L + kappa) + L) to give a good spread
(define (unscented-sigma-points mean covariance)
  (let*((L (length mean))
        (scale )
        (covariance_root (matrix-cholesky-decomposition covariance))
        

; x contains initial state
; Px contains covarience of x
; y contains 2L+1 sigma points
; Py contains covarience of y
; l = a^2(L + k) - L, scaling parameter
; a determines spread of sigma points usually from 1 to 1e-4
; k is a secondary parameter usually 0 or 3 - L
; X = avg(x), and for i from 0 to L, avg(x) +- sqrt((L+l)Px)i 

(define (unscented-transformation x Px


(define (unscented-sigma-points->mean-covarience 

(define (unscented-kalman-filter states measurements)

; calculate test using states for position and velocity

(define (kalman-test
