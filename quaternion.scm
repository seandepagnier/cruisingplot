;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version. 

(declare (unit quaternion))

(define (quaternion+ . args)
  (apply map + args))

(define quaternion-identity '(1 0 0 0))

(define (quaternion* . args)
  (case (length args)
    ((0) quaternion-identity)
    ((1) (first args))
    ((2) (let ((q0 (first args)) (q1 (second args)))
           (let ((q00 (first q0)) (q01 (second q0)) (q02 (third q0)) (q03 (fourth q0))
                 (q10 (first q1)) (q11 (second q1)) (q12 (third q1)) (q13 (fourth q1)))
             `(,(- (* q00 q10)    (* q01 q11)    (* q02 q12)    (* q03 q13))
               ,(+ (* q00 q11)    (* q01 q10)    (* q02 q13) (- (* q03 q12)))
               ,(+ (* q00 q12) (- (* q01 q13))   (* q02 q10)    (* q03 q11))
               ,(+ (* q00 q13)    (* q01 q12) (- (* q02 q11))   (* q03 q10))))))
    (else (apply quaternion* (quaternion* (first args) (second args)) (cddr args)))))

(define (quaternion-conjugate q)
  (cons (car q) (vector- (cdr q))))

(define (apply-quaternion-to-vector q v)
  (let ((w (cons 0 v)))
    (cdr (quaternion* q w (quaternion-conjugate q)))))

; create a quaternion from an angle and vector it rotates about
(define (angle-vector->quaternion angle v)
  (let ((n (vector-magnitude v)))
    (let ((fac (if (zero? n) 0 (/ (sin (/ angle 2)) n))))
      (cons (cos (/ angle 2))
            (vector-scale fac v)))))

; give the quaternion rotation from 3 vector a to 3 vector b
(define (vector-vector->quaternion a b)
  (angle-vector->quaternion (acos (min 1 (/ (vector-dot a b) (sqrt (* (vector-magnitude^2 a)
                                                                      (vector-magnitude^2 b))))))
                            (vector-cross a b)))

(define (quaternion a b c d) (list a b c d))    

; same as 4 vector
(define quaternion-magnitude vector-magnitude)

; same as 4 vector
(define quaternion-normalize vector-normalize)

; angle of rotation of quaternion
(define (quaternion-angle q)
  (* 2 (acos (first q))))

; angle of rotation given just 3 imaginary parts
(define (quaternion-imaginary-vector-angle v)
  (* 2 (asin (vector-magnitude v))))

; convert quaternion rotation to 3x3 discrete cosine matrix rotation
(define (quaternion->discrete-cosine-matrix q)
  (let ((q0 (first q)) (q1 (second q)) (q2 (third q)) (q3 (fourth q)))
    (let ((q0^2 (square q0)) (q1^2 (square q1)) (q2^2 (square q2)) (q3^2 (square q3)))
      (matrix
       `((,(+ q0^2 q1^2 (- q2^2) (- q3^2)) ,(* 2 (- (* q1 q2) (* q0 q3))) ,(* 2 (+ (* q1 q3) (* q0 q2))))
         (,(* 2 (+ (* q1 q2) (* q0 q3))) ,(+ q0^2 (- q1^2) q2^2 (- q3^2)) ,(* 2 (- (* q2 q3) (* q0 q1))))
         (,(* 2 (- (* q1 q3) (* q0 q2))) ,(* 2 (+ (* q2 q3) (* q0 q1))) ,(+ q0^2 (- q1^2) (- q2^2) q3^2)))))))

; convert 3x3 discrete cosine matrix rotation to quaternion
(define (discrete-cosine-matrix->quaternion m)
  (let ((q0 (/ (sqrt (+ 1 (matrix-ref m 0 0) (matrix-ref m 1 1) (matrix-ref m 2 2))) 2)))
    `(,q0
      ,(/ (- (matrix-ref m 2 1) (matrix-ref m 1 2)) (* 4 q0))
      ,(/ (- (matrix-ref m 0 2) (matrix-ref m 2 0)) (* 4 q0))
      ,(/ (- (matrix-ref m 1 0) (matrix-ref m 0 1)) (* 4 q0)))))

; convert a quaternion rotation to pitch roll and yaw
(define (quaternion->pitch-roll-yaw q)
  (let ((q0 (first q)) (q1 (second q)) (q2 (third q)) (q3 (fourth q)))
    (let ((q0^2 (square q0)) (q1^2 (square q1)) (q2^2 (square q2)) (q3^2 (square q3)))
      `(,(atan (* 2 (+ (* q0 q1) (* q2 q3))) (+ q0^2 (- q1^2) (- q2^2) q3^2))
        ,(asin (* 2 (- (* q0 q2) (* q1 q3))))
        ,(atan (* 2 (+ (* q1 q2) (* q0 q3))) (+ q0^2 q1^2 (- q2^2) (- q3^2)))))))

; convert pitch roll and yaw angles to quaternion rotation
(define (pitch-roll-yaw->quaternion p r y)
  (let ((p2 (/ p 2)) (r2 (/ r 2)) (y2 (/ y 2)))
    (let ((sp2 (sin p2)) (sr2 (sin r2)) (sy2 (sin y2))
          (cp2 (cos p2)) (cr2 (cos r2)) (cy2 (cos y2)))
      `(,(+ (* cp2 cr2 cy2) (* sp2 sr2 sy2))
        ,(- (* sp2 cr2 cy2) (* cp2 sr2 sy2))
        ,(+ (* cp2 sr2 cy2) (* sp2 cr2 sy2))
        ,(- (* cp2 cr2 sy2) (* sp2 sr2 cy2))))))

; use rotate a vector around a quaternion using two different means of computation
; to test library correctness
(define (test-quaternion-on-vector q v)
  (print "q applied to v                       : " (apply-quaternion-to-vector q v))
  (print "q converted to matrix multiplied by v: "
         (row-matrix->vector
          (matrix* (quaternion->discrete-cosine-matrix q)
                   (vector->row-matrix v)))))

; given an orientation quaternion, and pitch roll, yaw rates relative to
; this orientation, calculate the quaternion rotation rate
(define (quaternion-rate quat p q r)
  (vector-scale (/ 2) (row-matrix->vector
                       (matrix*
                        (matrix `(( 0 ,(- p) ,(- q) ,(- r))
                                  (,p     0  ,   r  ,(- q))
                                  (,q ,(- r)     0  ,   p )
                                  (,r ,q     ,(- p)     0 )))
                        (vector->row-matrix quat)))))
