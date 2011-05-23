;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version. 

(declare (unit options))

;  Verifiers for options
;  these store the name, default value, and a method to verify a new value
;  and update it
(define (make-verifier name help default verify)
  (lambda (op)
    (case op
      ((name) name)
      ((help) (print name " -- " help " (default " default ")"))
      ((default) (verify default))
      ((verify) verify))))

(define (make-number-verifier name help default min max)
  (make-verifier
   name help default
   (lambda (num)
     (cond ((< num min) (print "Value given for option '" name "' is below minimum of " min) min)
           ((> num max) (print "Value given for option '" name "' is above maximum of " max) max)
           (else num)))))

(define (make-integer-verifier name help default min max)
  (make-verifier name help default
                 (lambda (num)
                   (if (not (integer? num))
                       (die "non-integer value: " num " given for " name)
                       (((make-number-verifier name help default min max) 'verify) num)))))

(define (make-discrete-verifier name help default validvalues)
  (make-verifier
   name (apply string-append help " Values: " (map symbol->string validvalues))
   default
   (lambda (value)
     (if (member value validvalues)
         value
         (die "value " value " for option " name
              " invalid.  Valid values:\n" validvalues)))))

(define (make-baud-verifier name help default)
  (make-discrete-verifier
   name help default
   '(300 600 1200 2400 4800 9600 19200 38400 57600)))

(define (make-boolean-verifier name help default)
  (make-verifier
   name (string-append help " (true or false)") default
   (lambda (value)
     (cond ((boolean? value) value)
           ((eq? value 'true) #t)
           ((eq? value 'false) #f)
           (else (error "boolean option " name " must be either true or false\n"))))))

(define (make-string-verifier name help default)
  (make-verifier
   name help default
   (lambda (value)
     (cond ((string? value) value)
           ((symbol? value) (symbol->string value))
           (else (error "string option " name " requires string value\n"))))))

(define (make-color-verifier default)
  (make-verifier 'color 
                 (string-append
                  "can be by name, eg:\n"
                  "            red green blue cyan magenta\n"
                  "            yellow gray black white\n"
                  "            or can be a normalized triple, for red:\n"
                  "            (1 0 0)\n")
                 default parse-color))

(define (make-bounds-verifier)
  (make-verifier 'bounds
                 (string-append
                  "minimum and maximum values for each axis (rectangular), eg:\n"
                  "                 ((-10 10) (-5 5))\n")
                 '()
                 (lambda (value)
                   (cond ((eq? value 'auto) '())
                         (else value)))))

(define (make-sensor-indicies-verifier sensors-name sensor-name)
  (make-verifier sensors-name
                 "list of indicies of sensors to use or all default all"
                 (lambda (value)
                   (letrec ((and (lambda args (cond ((null? args) #t)
                                                    ((car args) (apply and (cdr args)))
                                                    (else #f)))))
                     (cond ((eq? value 'all) (sensor-indicies sensor-name))
                           ((not (and (list? value) (apply and (map number? value))))
                            (error "value must be list of indicies for" sensors-name))
                           (else value))))))

(define (create-options verifiers examples parent-options)
  (let ((values (alist->hash-table
                 (map (lambda (verifier) (cons (verifier 'name) (verifier 'default))) verifiers)))
        (verifiers (alist->hash-table
                    (map (lambda (verifier) (cons (verifier 'name) (verifier 'verify))) verifiers))))
    (lambda (op . args)
      (cond
        ((eq? op 'help)
         (hash-table-for-each
          values
          (lambda (name value) (print name ": " value)))
         (print "Examples: " examples)
         (if parent-options (parent-options 'help)))
        ((eq? op 'update)
         (let ((value (if (null? (cdr args)) 'true (second args))))
           (cond ((hash-table-exists? values (first args))
                  (hash-table-set! values (first args)
                                   ((hash-table-ref verifiers (first args)) value)))
                 (parent-options (apply parent-options op args))
                 (else (error "cannot update unrecognized option" (car args)
                              (hash-table-ref values op))))))
        ((hash-table-exists? values op)
         (hash-table-ref values op))
        (parent-options (parent-options op))
        (else (error "unrecognized option" op))))))

(define (options-help options arg)
  (cond ((equal? arg "help")
         (print "options include:")
         (options 'help)
         (exit))))

;  parse basic option splitting and parsing where options are separated by
; , and values assignned with =
(define (parse-basic-split-options-string options args)
  (if (not (null? args)) (options-help options (first args)))
  (for-each (lambda (option)
              (apply options 'update
                     (map read-from-string (string-split option "="))))
            args))

(define (parse-basic-options-string options arg)
  (parse-basic-split-options-string options (string-split arg ",")))

(define (parse-basic-arg-options-string options arg)
  (options-help options arg)
  (let ((options-string (string-split arg ",")))
    (parse-basic-split-options-string options (cdr options-string))
    (car options-string)))
