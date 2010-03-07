;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

; various values can be computed from the raw sensors
; these values may not be computed all the time

(use srfi-1 srfi-69 environments)

(define computations (make-hash-table))

; Register a computation with given name, and info string
; sensors are a list of the sensors needed for this calculation
; to succeed
; calculation is a thunk that returns the computed value
(define (computation-register name info calculation)
  (hash-table-set! computations name
                   (list info calculation)))

; print the computation info for all the registered computations
(define (computation-info)
  (hash-table-walk computations
                   (lambda (name data)
                           (print name " -- " (first data)))))

; shorthand way of reaching computations
; certain computations may be buffered within a certain period to
(define (computation-calculate name)
  (if (hash-table-exists? computations name)
      (let ((data (hash-table-ref computations name)))
        ((second data)))
      (error "Cannot perform unregistered computation" name)))

; return a thunk which evaluates a given expression which may use
; symbols which are registered computations which are evaluated and substituted
(define (computations-revaluate . exps)
  (let ((symbols (delete-duplicates
                  (let each-expr ((exps exps))
                    (cond ((and (symbol? exps)
                                (hash-table-exists? computations exps))
                           (list exps))
                          ((and (not (null? exps)) (list? exps))
                           (append (each-expr (car exps)) (each-expr (cdr exps))))
                          (else '()))))))
      (lambda ()
        (let ((env (scheme-report-environment 5)))
          (for-each (lambda (symbol)
                      (environment-extend! env symbol (computation-calculate symbol)))
                    symbols)
          (map (lambda (exp) (eval exp env)) exps)))))

; basic computations
(computation-register 'time "time in seconds since start" current-time)
