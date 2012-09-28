;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

(declare (unit utilities))
(declare (uses plot srfi-18))

(use srfi-18)

(define (number-in-bounds x min max)
  (cond ((< x min) min)
        ((> x max) max)
        (else x)))

(define (sequence from to . step)
  (if (> from to) '() (cons from (apply sequence (+ from (if (null? step) 1 (car step))) to step))))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (complex-abs c)
  (sqrt (+ (square (real-part c)) (square (imag-part c)))))

(define (complex-phase c)
  (atan (imag-part c) (real-part c)))

(define (nan? x)
  (not (= x x)))

; this works for all real and imaginary infinities
(define (infinity? x)
  (let ((y (complex-abs x)))
    (and (= y y (+ y 1)))))

(define (read-from-string string) (with-input-from-string string read))

(define last-show #t)
(define (nice-print . args)
  (if last-show (display "\n"))
  (apply print args)
  (set! last-show #f))

(define (show . args)
  (if last-show (display "\r"))
  (for-each (lambda (arg) (display arg)) args)
  (if (not last-show) (display "\n"))
  (set! last-show #t))

(define (die . args)
  (apply nice-print "Error: " args)
  (exit))

(define (warning . args)
  (apply nice-print "Warning: " args))

(define warning-once
  (let ((table (make-hash-table)))
    (lambda args
      (cond ((not (hash-table-exists? table args))
             (hash-table-set! table args #t)
             (apply warning args))))))

(define (start-timer . replayrate)
  (let ((start (current-milliseconds)))
    (lambda () (* (if (null? replayrate) 1 (car replayrate))
                  (/ (- (current-milliseconds) start) 1000)))))

(define elapsed-seconds (start-timer))

(define (noop . _) #t)
(define verbose noop)
(define very-verbose noop)

(define sleep thread-sleep!)

(define (push-exit-handler handler)
 (exit-handler (let ((old-handler (exit-handler)))
                (lambda _ (handler) (apply old-handler _)))))

(define (port->string port)
  (with-output-to-string (lambda () (write port))))

(define (debug arg)        
  (create-periodic-task
   (string-append "debug '" arg "' task") 1
   (let ((exps (map read-from-string (string-split arg ","))))
     (let ((comp (apply computations-revaluate exps)))
       (lambda ()
         (cond ((equal? arg "help")
                (nice-print "possible values which can be used in a debugging expression:")
                (task-sleep 1) ; give tasks time to create computations for the info
                (computation-info)
                (exit)))

           (let ((values (comp)))
             (cond (values
                    (for-each (lambda (exp value)
                                (if (not (eq? exp (car exps))) (display "\t"))
                                (display exp)
                                (display ": ")
                                (display value))
                              exps values)
                    (newline)))))))))

(define (make-run-once proc)
  (let ((run #f))
    (lambda ()
      (if run
          (error "proc run more than once" proc)
          (let ()
            (set! run #t)
            (proc))))))

(define (scientific-notation x)
  (if (negative? x)
      (display "-"))
  (let helper ((x (abs x)) (exp 0))
    (cond ((>= x 10)
           (helper (/ x 10) (+ exp 1)))
          ((< x 1)
           (helper (* x 10) (- exp 1)))
          (else (list x exp)))))

(define (display-scientific-notation-places x places)
  (let ((s (scientific-notation x)))
    (display (round-to-places (first s) places))
    (cond ((not (zero? (second s)))
           (display "e")
           (display (second s))))))

(define (round-to-places n places)
  (if n (let ((factor (expt 10 places)))
          (/ (round (* n factor)) factor))
      #f))

(define (map-round-to-places l places)
  (cond ((not l) #f)
        ((null? l) '())
        ((string? l) l)
        ((number? l) (round-to-places l places))
        ((list? l) (cons (map-round-to-places (car l) places)
                         (map-round-to-places (cdr l) places)))
        (else (error "unhandled type to map-round-to-places" l))))

(define (saturate value minimum maximum)
  (min minimum (max maximum value)))

(define (round-toward-zero n)
  (if (positive? n) (floor n) (ceiling n)))

(define (integer->name n)
  (if (not (integer? n)) (error "integer->name recieved non-integer" n))
  (define (integer->name-append n)
    (if (zero? n) "" (string-append " " (integer->name n))))
  (if (negative? n)
      (string-append "negative " (integer->name (abs n)))
      (let each-div ((divs '((1e15 "quadrillion") (1e12 "trillion") (1e9 "billion")
                             (1e6 "million") (1e3 "thousand") (1e2 "hundred"))))
        (cond ((null? divs) ; under 100 now
           (if (< n 20)
               (case (inexact->exact n)
                 ((0) "zero") ((1) "one") ((2) "two") ((3) "three") ((4) "four")
                 ((5) "five") ((6) "six") ((7) "seven") ((8) "eight") ((9) "nine")
                 ((10) "ten") ((11) "eleven") ((12) "twelve") ((13) "thirteen") ((14) "fourteen")
                 ((15) "fifthteen") ((16) "sixteen") ((17) "seventeen") ((18) "eighteen") ((19) "nineteen")
                 (else (error "unsuported number" n)))
               (string-append (case (inexact->exact (floor (/ n 10)))
                                ((2) "twenty") ((3) "thirty") ((4) "fourty") ((5) "fifty")
                                ((6) "sixty") ((7) "seventy") ((8) "eighty") ((9) "ninety")
                                (error "failed decoding in integer->number" (floor (/ n 10))))
                              (integer->name-append (remainder n 10)))))
              ((>= n (caar divs))
               (string-append (integer->name (floor (/ n (caar divs))))
                              " " (cadar divs)
                              (integer->name-append (remainder n (caar divs)))))
              (else (each-div (cdr divs)))))))

(define (number->name n)
  (cond ((not (number? n)) (error "number->name requires number, given" n))
        ((not (= n n)) "NaN")
        ((not (zero? (imag-part n)))
         (string-append (number->name (real-part n)) " and "
                        (number->name (imag-part n)) " imaginary"))
        (else
         (let* ((r (round-toward-zero n))
                (f (- n r)))
           (string-append (integer->name r)
                          (if (zero? f) ""
                              (let start ((s (string->list (number->string f))))
                                (cond ((null? s) (error "number->name failed decoding fraction"))
                                      ((equal? (car s) #\.)
                                       (apply string-append " point"
                                              (map (lambda (c)
                                                     (string-append
                                                      " "
                                                      (integer->name
                                                       (string->number
                                                        (string c)))))
                                                   (cdr s))))
                                      (else (start (cdr s)))))))))))

(define (base-integer->primary-name n)
  (case n ((1) "first") ((2) "second") ((3) "third") ((5) "fifth")
    (else (let ((sn (integer->name n)))
            (string-append
             (case (string-ref sn (- (string-length sn) 1))
               ((#\e #\t) (substring sn 0 (- (string-length sn) 1)))
               (else sn))
             "th")))))
         
; convert 1 to "first", 2 to "second", 3 to "third" etc..
(define (integer->primary-name n)
  (let* ((sn (remainder n 10)) (bn (- n sn)))
    (if (or (< (remainder n 100) 20) (zero? (remainder n 10)))
        (base-integer->primary-name n)
        (string-append (integer->name bn) " " (base-integer->primary-name sn)))))

; remove last character from a string
(define (remove-last-character s)
  (list->string (reverse (cdr (reverse (string->list s))))))

(define (zero-if-false value)
  (if value value 0))

(define (first-if-true value)
  (if value (first value) value))

(define (random-in-range start end)
  (+ start (/ (* (- end start) (random 100001)) 100000)))
