;; Copyright (C) 2009 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

; historys store a list of lists of numbers of data
;  are procedures that take a symbol for the operation:
; dump - return a list of history pairs ((T1 value1 .. valuem) ... (Tn value1 ... valuem))
; min  - return minimum value recorded
; max  - return maximum value recorded
; minmax-recalculate - reset and recalculate min and max from current storage
; update - store a new history value
; apply-clippings - remove items outside of the min or max list
(define (create-history)
   (let ((history '()) (minvalues '()) (maxvalues '()))
    (lambda (op . args)
      (case op
        ((dump) (map second history))
        ((min) minvalues)
        ((max) maxvalues)
        ((update)
         (set! minvalues (if (null? minvalues) (car args) (map min minvalues (car args))))
         (set! maxvalues (if (null? maxvalues) (car args) (map max maxvalues (car args))))
         (set! history (cons (list (/ (current-milliseconds) 1000) (car args)) history)))
        ((apply-timeout)
         (let ((oldest (- (/ (current-milliseconds) 1000) (first args))))
           (set! history (let each-history ((history history))
                           (if (or (null? history) (< (first (car history)) oldest))
                               '()
                               (cons (car history) (each-history (cdr history)))))))
         ; recalculate min and max
         (let ((history-values (map second history)))
           (set! minvalues (apply map min history-values))
           (set! maxvalues (apply map max history-values))))
       ((apply-clippings)
        (let ((mins (first args))
              (maxs (second args)))
          (set! history
                (let in-history ((history history))
                  (if (null? history) '()                      
                      (if (let in-range ((value (car history))
                                         (mins mins) (maxs maxs))
                            (cond ((null? value) #t)
                                  ((and (car mins) (< (car value) (car mins))) #f)
                                  ((and (car maxs) (> (car value) (car maxs))) #f)
                                  (else (in-range (cdr value) (cdr mins) (cdr maxs)))))
                          (cons (car history) (in-history (cdr history)))
                          (in-history (cdr history))))))))))))
