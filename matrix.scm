;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version. 

;  I define additional utility functions to matrix-utils
;  for matrices which sits on top of lapack blas etc..
;  makes it easier for me to read

(declare (unit matrix))

(use srfi-1 srfi-4-utils blas matrix-utils atlas-lapack)

(define-utility-matrices f64)

(define matrix-rows first)
(define matrix-cols second)
(define matrix-vector third)
(define matrix-trans fourth) ; is the matrix transposed?
                             ; this allows O(1) transposing

(define (vector->matrix n m vector)
  (list n m vector NoTrans))

(define (vector->square-matrix vector)
  (let ((n (sqrt (f64vector-length vector))))
    (if (not (integer? n)) (error "invalid vector size to vector->square-matrix" vector))
    (vector->matrix n n vector)))

(define (vector->matrix-trans n m vector trans)
  (list n m vector trans))

(define zero-matrix (vector->matrix 0 0 (f64vector)))

(define (zeroed-matrix r c)
  (vector->matrix r c (matrix-zeros r c)))

(define (matrix l)
  (let ((n (length l)))
    (if (= 0 n)
        zero-matrix
        (let ((m (length (car l))))
          (for-each (lambda (l) (if (not (= m (length l)))
                                    (error "non-rectangular lists in matrix")))
                    (cdr l))
          (vector->matrix n m (list->f64vector (fold append '() l)))))))

(define (matrix-identity r . c)
  (if (null? c)
      (vector->matrix r r (matrix-eye r r))
      (vector->matrix r (car c) (matrix-eye r (car c)))))

(define (matrix-ref m row col)
  (f64vector-ref (matrix-vector m)
                 (if (eq? NoTrans (matrix-trans m))
                     (+ (* row (matrix-cols m)) col)
                     (+ (* col (matrix-rows m)) row))))

(define (matrix->list m)
  (map (lambda (row)
         (map (lambda (col)
                (matrix-ref m row col))
              (sequence 0 (- (matrix-cols m) 1))))
       (sequence 0 (- (matrix-rows m) 1))))

(define (matrix-transpose m)
  (list (matrix-cols m) (matrix-rows m) (matrix-vector m)
        (if (eq? NoTrans (matrix-trans m))
            Trans
            NoTrans)))

(define (matrix+ . matrices)
  (define (matrix+sub a c)
    (let ((a (if (eq? (matrix-trans c) NoTrans) a (matrix-transpose a))))
      (if (not (and (= (matrix-rows a) (matrix-rows c))
                    (= (matrix-cols a) (matrix-cols c))))
          (error "dimension mis-match in matrices supplied to matrix+" a c))
      (let ((b (matrix-identity (matrix-cols a) (matrix-cols a))))
        (vector->matrix-trans (matrix-rows a) (matrix-cols b)
                                  (blas:dgemm blas:RowMajor (matrix-trans a) NoTrans
                                              (matrix-rows a) (matrix-cols b) (matrix-cols a)
                                              1 (matrix-vector a) (matrix-vector b) 1 (matrix-vector c))
                                              (matrix-trans c)))))
  (cond ((null? matrices) zero-matrix)
        ((null? (cdr matrices)) (car matrices))
        (else (apply matrix+ (cons (matrix+sub (car matrices) (cadr matrices))
                                           (cddr matrices))))))

(define (matrix-neg m)
  (vector->matrix-trans (matrix-rows m) (matrix-cols m)
                        (f64vector-map - (matrix-vector m)) (matrix-trans m)))

(define (matrix- . matrices)
  (cond ((null? matrices) zero-matrix)
        ((null? (cdr matrices)) (matrix-neg (car matrices)))
        (else (matrix+ (car matrices) (matrix-neg (apply matrix+ (cdr matrices)))))))

(define (matrix-map proc m)
  (vector->matrix-trans (matrix-rows m) (matrix-cols m)
                        (f64vector-map proc (matrix-vector m))
                        (matrix-trans m)))

(define (matrix* . matrices)
  (define (matrix*sub a b)
    (cond ((and (number? a) (number? b)) (* a b))
          ((number? a) (matrix-map (lambda (n) (* a n)) b))
          ((number? b) (matrix-map (lambda (n) (* b n)) a))
          ((not (= (matrix-cols a) (matrix-rows b)))
           (error "dimension mis-match in matrices supplied to matrix*" a b))
          (else
           (let ((c (matrix-zeros (matrix-rows a) (matrix-cols b))))
             (vector->matrix (matrix-rows a) (matrix-cols b)
                             (blas:dgemm blas:RowMajor (matrix-trans a) (matrix-trans b)
                                         (matrix-rows a) (matrix-cols b) (matrix-cols a)
                                         1 (matrix-vector a) (matrix-vector b) 0 c))))))
  (cond ((null? matrices) zero-matrix)
        ((null? (cdr matrices)) (car matrices))
        (else (apply matrix* (cons (matrix*sub (car matrices) (cadr matrices))
                                   (cddr matrices))))))

(define (matrix-square? m)
  (= (matrix-rows m) (matrix-cols m)))

(define (matrix-vector-index m row col)
  (if (eq? NoTrans (matrix-trans m))
      (+ (* row (matrix-cols m)) col)
      (+ (* col (matrix-rows m)) row)))

(define (matrix-ref m row col)
  (f64vector-ref (matrix-vector m) (matrix-vector-index m row col)))

(define (matrix-set! m row col value)
  (f64vector-set! (matrix-vector m) (matrix-vector-index m row col) value))

(define (matrix-remove-row m row)
  (matrix (let each-row ((rows (matrix->list m)) (r 0))
            (if (null? rows) '()
                (let ((rest (each-row (cdr rows) (+ r 1))))
                  (if (= row r) rest (cons (car rows) rest)))))))

(define (matrix-remove-col m col)
  (matrix-transpose (matrix-remove-row (matrix-transpose m) col)))

(define (matrix-determinant m)
  (cond ((not (= (matrix-rows m) (matrix-cols m)))
         (error "matrix-determinant failed on non-square matrix" m))
        ((= (matrix-rows m) 1) (matrix-ref m 0 0))
        (else
         (let ((n (matrix-remove-row m 0)))
           (fold + 0 (map (lambda (col)
                            (* (if (even? col) 1 -1) (matrix-ref m 0 col)
                               (matrix-determinant (matrix-remove-col n col))))
                          (sequence 0 (- (matrix-cols m) 1))))))))
                  
(define (matrix-inverse m)
  (if (not (matrix-square? m)) (error "cannot perform inverse on non-square matrix" m))
  (call/cc (lambda (cont)
             (with-exception-handler
              (lambda _ (cont #f))
              (lambda ()
              (let-values (((A pivot) (atlas-lapack:dgetrf blas:RowMajor (matrix-rows m)
                                                           (matrix-cols m) (matrix-vector m))))
                (vector->matrix (matrix-rows m) (matrix-cols m)
                                (atlas-lapack:dgetri blas:RowMajor (matrix-rows m) A pivot))))))))

; use cholesky decomposition on matrix A returns L
; A = LL'
; A must be square, symmetric, invertable, positive and definate
; Ljj = sqrt(Ajj - sum(Ljk^2)  k = 1 to j-1))
; Lij = (Aij - sum(LikLjk  k = 1 to j-1)) / Ljj
; return #f if no decomp can be found
(define (matrix-cholesky-decomposition A)
  (call/cc
   (lambda (ret)
     (let* ((n (matrix-rows A))
            (L (zeroed-matrix n n))
            (square (lambda (x) (* x x))))
       (for-each
        (lambda (j)
          (matrix-set!
           L j j (let ((val (- (matrix-ref A j j)
                               (fold + 0 (map (lambda (k)
                                               (square (matrix-ref L j k)))
                                             (sequence 0 (- j 1)))))))
                   (if (not (positive? val)) (ret #f) (sqrt val))))
          (for-each (lambda (i)
                      (matrix-set!
                       L i j (/ (- (matrix-ref A i j)
                                   (fold + 0 (map (lambda (k)
                                                   (* (matrix-ref L i k)
                                                      (matrix-ref L j k)))
                                                 (sequence 0 (- j 1)))))
                                (matrix-ref L j j))))
                    (sequence (+ j 1) (- n 1))))
        (sequence 0 (- n 1)))
       (ret L)))))
