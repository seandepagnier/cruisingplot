;; Copyright (C) 2011 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version. 

;;; types for fields within a list
(declare (unit types))

(define types (make-hash-table))

; provide a means to extract elements from a list by name
; handler should take the subfield and a value
(define (type-register name fields)
  (hash-table-set! types name fields))

; generic means to operate on the type
(define (type-field-apply name field values func)
  (let each-def ((definition (hash-table-ref types name))
                 (values values))
    (cond 
     ((null? definition) (error "Field " field " does not exist in " name))
     ((eq? field (car definition)) (func values))
     (else (each-def (cdr definition) (cdr values))))))

; get the right field from a list
(define (type-field-get name field values)
  (type-field-apply name field values car))

; set a field in a list
(define (type-field-set! name field values value)
  (type-field-apply name field values (lambda (values) (set-car! values value))))

(define (type-field-print name values)
  (for-each (lambda (field value)
              (display field) (display ": ") (display value) (display "   "))
            (hash-table-ref types name) values)
  (newline))
