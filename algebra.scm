;; Copyright (C) 2009, 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

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
