;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

(declare (unit frequency))

(use srfi-1)

; convert data into to the frequency domain
(define (frequency-from-data data min max step)
  (let* ((slots (/ (- max min) step))
         (frequencies (make-list slots 0)))
    (let each-data ((data data))
      (cond ((null? data) frequencies)
            (else
             (if (and (>= (car data) min)
                      (<= (car data) max))
                 (let ((pos (drop frequencies (/ (- (car data) min) step))))
                   (set-car! pos (+ (car pos) 1))))
             (each-data (cdr data)))))))
