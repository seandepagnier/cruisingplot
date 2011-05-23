;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version. 

(declare (unit leastsquares))
(declare (uses matrix))

; return (HH')^-1*H'Z'
(define (least-squares-apply H Z)
  (matrix* (let ((i (matrix* (matrix-transpose H) H)))
            (let ((ii (matrix-inverse i)))
              ii))
           (matrix-transpose H) Z))

; the functions build build-jacobian-row takes state and measurements
(define (compute-least-squares state build-jacobian-residual-row measurements)
  (let ((HZ (map (lambda (measurement)
                   (build-jacobian-residual-row state measurement))
                 measurements)))
    (list (matrix (map car HZ)) (matrix (map cdr HZ)))))

; complete? is a procedure which takes the state and may determine to stop iteration
;              and/or modify the state
(define (least-squares-iterate initial-state build-jacobian-residual-row measurements
                               complete? max-iterations)
  (let each-iteration ((state initial-state) (iteration 0))
    (let ((HZ (compute-least-squares state build-jacobian-residual-row measurements)))    
      (let* ((nstate (matrix+ state (apply least-squares-apply HZ))))
        (print "nstate " nstate)
        (cond ((complete? nstate (second HZ)) nstate)
              ((> iteration max-iterations) (verbose "max iters reached") #f)
              (else (each-iteration nstate (+ iteration 1))))))))

;(define (regression data . partials)


; least squares to fit data
; use y = a*x + b
;(define (linear-regression data)
;  (let ((state '(1 1)))
;    (let ((update
;           (compute-least-squares (lambda (state measurement)
;                                    (list (list (first measurement) 1)
;                                          (-  (second measurement)
;                                              (* (first state) (first measurement))
;                                              (second state)
;                                              )))
;                                  state
;                                  data)))
;      (matrix->list (matrix+ (matrix (list state))  update)))))
;)

; use y = a*x^2 + b*x + c
;(define (quadratic-regression data)

;y = a*x^3 + b*x^2 + c*x + d
;(define (cubic-regression data)
