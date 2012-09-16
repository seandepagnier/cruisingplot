;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

; various values can be computed from the raw sensors
; these values may not be computed all the time

(declare (unit computation))
(declare (uses utilities))

(use srfi-1 srfi-69 environments)

(define computations (make-hash-table))

; Register a computation with given name, and info string
; sensors are a list of the sensors needed for this calculation
; to succeed
; calculation is a thunk that returns the computed value
(define (computation-register name info sensors calculation)
  (if (hash-table-exists? computations name)
      (warning "re-registering computation: " name)
      (verbose "registered computation: " name " (" info ")"))
  (hash-table-set! computations name
                   (list info sensors calculation)))

; give the name which should return the computation for name.0
(define (computation-register-first-alias name sensors)
  (let*((zsname (string-append (symbol->string name) ".0"))
        (zname (string->symbol zsname)))
    (cond ((hash-table-exists? computations name) ; already without index? move it to first index
           (verbose "moving computation: " name " -> " zsname)
           (hash-table-set! computations zname (hash-table-ref computations name))
           (hash-table-delete! computations name)))
    (computation-register name (string-append "alias for " zsname) sensors
                          (lambda () (computation-calculate zname)))))

; print the computation info for all the registered computations
(define (computation-info)
  (hash-table-walk computations
                   (lambda (name data)
                     (if (apply sensor-contains? (second data))
                           (print name " -- " (first data))))))

; shorthand way of reaching computations
(define (computation-calculate name)
  (if (hash-table-exists? computations name)
      (let ((data (hash-table-ref computations name)))
        (if (apply sensor-contains? (second data))
            ((third data)) #f))
      (error "Cannot perform unregistered computation " name)))

; return a thunk which evaluates a given expression which may use
; symbols which are registered computations which are evaluated and substituted
; if the return is false, then the expression could not be computed
(define (computations-revaluate . exps)
  (let ((symbols '()))
    (define (symbols-recompute)
      (set! symbols
            (delete-duplicates
             (let each-expr ((exps exps))
               (cond ((and (symbol? exps)
                           (hash-table-exists? computations exps))
                      (list exps))
                     ((and (not (null? exps)) (list? exps))
                      (append (each-expr (car exps)) (each-expr (cdr exps))))
                     (else '()))))))
    (define (evaluate)
      (let ((env (scheme-report-environment 5)))
        (call/cc (lambda (cont)
                   (for-each (lambda (symbol)
                               (let ((value (computation-calculate symbol)))
                                 (if (not value) (cont #f))
                                 (environment-extend! env symbol value)))
                             symbols)
                   (map (lambda (exp) (eval exp env)) exps)))))
    (lambda ()
      (call/cc (lambda (bail)
                 (with-exception-handler
                  (lambda _
                    (symbols-recompute)
                    (bail
                     (call/cc (lambda (bail)
                                (with-exception-handler
                                 (lambda _ (bail #f))
                                 evaluate)))))
                  evaluate))))))

(define (computation-unique-index name)
    (if (not (hash-table-exists? computations name))
        0
        (let each-index ((index 1))
          (let ((sym (string->symbol (string-append (symbol->string name) "." (number->string index)))))
            (if (hash-table-exists? computations sym)
                (each-index (+ index 1))
                index)))))

(define (computation-indexed-name name index)
  (string->symbol (string-append (symbol->string name) "." (number->string index))))

(define (computation-register-unique-name name info sensors calculation)
  (let ((index (computation-unique-index name)))
    (cond ((zero? index) (computation-register name info sensors calculation))
          (else
           (if (= 1 index) (computation-register-first-alias name sensors))
           (computation-register (computation-indexed-name name index) info sensors calculation)))))

; basic computations
(computation-register 'time "time in seconds since start" '() elapsed-seconds)
