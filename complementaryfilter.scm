;; Copyright (C) 2012 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; The complementary filter is simpler and easier to tune than kalman
;; filter for attitude estimation.  Also less computation

;  theta'(t) = a*gyro + b
;  theta(0) = 0
;  theta(t+1) = theta(t) + theta'(t)
;
;  we are trying to find
(define (gyroscope-bias)

(define (complementary-filter a b

(define (make-complementary-filter )
  (let ((attitude #f))
    (lambda ()
