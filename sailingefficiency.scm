;; Copyright (C) 2009 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.


;; This file calculates the efficiency of the boat based on windspeed
;; update with each gps message

(define (symbols-combine . symbols)
  (string->symbol (apply string-append (map symbol->string symbols))))

; updates fields by running-function which takes fields (C A N)
; C is the new data value
; A is the previous average value
; N is the count of updates
(define (install-running-calculator running-name running-function name field #!optional (index 0))
  (data-register-callback
   `(,name ,index)
   (lambda (data)
       (data-update `(,(symbols-combine name running-name) index)
                    (type-field-get name field data)))))

; initial average = C
; average update = (A + C) * N / (N+1)
(define (install-average-calculator name field #!optional (index 0))
  (install-running-calculator 'average
   (lambda (C A N)
     (if (= N 0) 0
         (/ (* (+ A C) N)
            (+ N 1))))))

; initial average = C
; average update = (A + C) * N / (N+1)
; final average is sum of all values
(define (install-deviation-calculator name field #!optional (index 0))
  (let ((average-name ,(symbols-combine 'average name)))
    (if (not (data-contains? `(average-name index)))
        (install-average-calculator name field index))
    (install-running-calculator
     'deviation
     (let ((values '()))
       (lambda (C A N)
         (let ((value (data-query `(,name ,index))))
           (set! values (cons value values))
           (sqrt (/ (fold (lambda (value total) (+ (square (- value C)) total))
                          0 values)
                    N)))))
     average-name field index)))


; store data and be able to plot
(define (new-wind-direction-speed-plot)
  (data-register-callback 

(define (calculate-hull-speed
