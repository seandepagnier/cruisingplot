;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version. 

(declare (unit leastsquares))
(declare (uses matrix vector))

; return (HH')^-1*H'Z'
(define (least-squares-apply H Z)
  (let ((i (matrix* (matrix-transpose H) H)))
    (let ((ii (matrix-inverse i)))
      (if ii
          (matrix* ii (matrix-transpose H) Z)
          (zeroed-matrix (matrix-rows i) (matrix-cols i))))))

; the functions build build-jacobian-row takes state and measurements
(define (compute-least-squares state build-jacobian-residual-row measurements)
  (let ((HZ (map (lambda (measurement)
                   (build-jacobian-residual-row state measurement))
                 measurements)))    
    (list (matrix (map car HZ)) (matrix (map cdr HZ)))))

(define (least-squares-iterate initial-state build-jacobian-residual-row measurements
                               complete? max-iterations)
  (let each-iteration ((state initial-state) (iteration 0))
    (let*((HZ (compute-least-squares state build-jacobian-residual-row measurements))
          (updates (first (matrix->list (matrix-transpose (apply least-squares-apply HZ)))))
          (nstate (map + state updates))          
;          (update (vector-magnitude (first (matrix->list (matrix-transpose (second HZ)))))))
          (update (vector-magnitude updates)))
;      (print "Z " (first (matrix->list (matrix-transpose (second HZ)))))
      (cond ((or (> iteration max-iterations)
                 (and complete? (complete? nstate update)))
             (list nstate (vector-magnitude
                           (first (matrix->list (matrix-transpose (second HZ)))))))
            (else (each-iteration nstate (+ iteration 1)))))))

; least squares to fit data
; use y = a
(define (constant-regression data)
    (least-squares-iterate '(1) (lambda (state measurement)
                             (list (list 1) ; derivative
                                   (-  (second measurement) (first state)))) ;residual
                                  data 1))

; use y = a*x + b
(define (linear-regression data)
    (least-squares-iterate '(1 1) (lambda (state measurement)
                                    (list (list (first measurement) 1)
                                          (-  (second measurement)
                                              (* (first state) (first measurement))
                                              (second state))))
                           data 1))

; use y = a*x^2 + b*x + c
(define (quadratic-regression data)
  (least-squares-iterate
   (cons 1 (first (linear-regression data)))
   (lambda (state measurement)
     (let ((j (list (square (first measurement)) (first measurement) 1)))
              (list j (- (second measurement)
                         (vector-dot j state)))))
   data 10))

;y = a*x^3 + b*x^2 + c*x + d
(define (cubic-regression data)
  (least-squares-iterate
   (cons 1 (first (quadratic-regression data)))
   (lambda (state measurement)
     (let ((j (list (cube (first measurement)) (square (first measurement)) (first measurement) 1)))
              (list j (-  (second measurement)
                          (vector-dot j state)))))
   data 10))

; y = a + b*x + c*x^2 + ...
(define (polynomial-regression data max-order)
  (let each-order ((order 0) (last-state '()))
    (cond ((<= order max-order)
           (let ((state
                  (least-squares-iterate
                   (cons 1 last-state)
                   (lambda (state measurement)
                     (let ((fm (first measurement)))
                       (let ((j (cons 1 (let build-j ((o 1) (m fm))
                                          (cond ((<= o order)
                                                 (cons m (build-j (+ o 1) (* m fm))))
                                                (else '()))))))
                         (list j (- (second measurement)
                                    (vector-dot j state))))))
                     data 10)))
             (cond (state
                    (print "order " order " state " (map (lambda (n) (round-to-places n 4)) (first state))
                           " residual " (round-to-places (second state) 4))
                    (each-order (+ 1 order) (first state)))
                   (else
                    (print "failed to compute least squares after order " order))))))))


; calibrate x and y offsets as well as radius
; best fit circle to data
;
; measurements: x y
; states: yb zb (bias) s (scale)
;
; truth equation:
; (x-xb)^2 + (y-yb)^2 = s^2
;
(define (calibrate-biases-and-scale-2d measurements)
  (least-squares-iterate
   `(,(apply average (map first measurements))
     ,(apply average (map second measurements))
     1)
   (lambda (state measurement)
     (let-values (((xb yb s) (apply values state))
                  ((x y) (apply values measurement)))
       (let ((xs (- x xb)) (ys (- y yb)))
       (list (list (* 2 xs)
                   (* 2 ys)
                   (* 2 s))
             (-  (+ (square xs) (square ys)) (square s))))))
   measurements 10))

; measurements: x y
; states: xb yb (biase) s (scale) yrs (y relative scale)
; (x-xb)^2 + (yrs*(y-yb))^2 = s^2
;
; fit non-rotated ellipse to data
;
; convert yrs to sqrt on return
(define (calibrate-biases-scale-and-relative-scale-2d measurements)
  (least-squares-iterate
   `(,(apply average (map first measurements))
     ,(apply average (map second measurements))
     1 1)
   (lambda (state measurement)
     (let-values (((xb yb s yrs) (apply values state))
                  ((x y) (apply values measurement)))
       (let ((xs (- x xb)) (ys (- y yb)))
       (list (list (* 2 xs)
                   (* 2 (* (square yrs) ys))
                   (* 2 s)
                   (- (* 2 yrs (square ys))))
             (-  (+ (square xs) (square (* yrs ys))) (square s))))))
   measurements 10))

; this equation calibrates biases and relative scales
; calibrate x, y and z offsets as well as radius
;
; measurements: x y z
; states: xb yb zb (biase) s (scale)
;
; truth equation:
; (x-xb)^2 + (y-yb)^2 + (z-zb)^2 = s^2
;
(define (calibrate-biases-and-scale-3d measurements)
  (least-squares-iterate
   `(,(apply average (map first measurements))
     ,(apply average (map second measurements))
     ,(apply average (map third measurements))
     1)
   (lambda (state measurement)
     (let-values (((xb yb zb s) (apply values state))
                  ((x y z) (apply values measurement)))
       (let ((xs (- x xb)) (ys (- y yb)) (zs (- z zb)))
       (list (list (* 2 xs)
                   (* 2 ys)
                   (* 2 zs)
                   (* 2 s))
             (-  (+ (square xs) (square ys) (square zs)) (square s))))))
   measurements #f 20))

; measurements: x y z
; states: xb yb zb (biase) s (scale) yrs zrs (relative scales)
; (x-xb)^2 + (yrs*(y-yb))^2 + (zrs*(z-zb))^2 = s^2
;
; fit non-rotated ellipse to data
;
(define (calibrate-biases-scale-and-relative-scales-3d measurements)
  (least-squares-iterate
   `(,(apply average (map first measurements))
     ,(apply average (map second measurements))
     ,(apply average (map third measurements))
     1 1 1)
   (lambda (state measurement)
     (let-values (((xb yb zb s yrs zrs) (apply values state))
                  ((x y z) (apply values measurement)))
       (let ((xs (- x xb)) (ys (- y yb)) (zs (- z zb)))
       (list (list (* 2 xs)
                   (* 2 (* (square yrs) ys))
                   (* 2 (* (square zrs) zs))
                   (* 2 s)
                   (- (* 2 yrs (square ys)))
                   (- (* 2 zrs (square zs))))
             (-  (+ (square xs) (square (* yrs ys)) (square (* zrs zs))) (square s))))))
   measurements #f 20))
