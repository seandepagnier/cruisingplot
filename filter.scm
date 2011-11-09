;; Copyright (C) 2011 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

(declare (unit filter))
(declare (uses utilities options))

(define (create-filter arg)
  (let* ((options (create-options
                   (list (make-number-verifier 'update-period "how often to apply" .1 0 1000)
                         (make-number-verifier 'frequency "filter frequency" 1 0 1000)
                         (make-number-verifier 'order "filter order" 1 0 10)
                         (make-discrete-verifier 'type "type of filter" 'lowpass
                                                 '(lowpass highpass derivative integral))
                         (make-string-verifier 'name "name of filter" "filter"))
                   (string-append " lowpass gps speed: '-f gps-speed,frequency=.1'\n")
                   #f))
        (filter-string (parse-basic-arg-options-string options arg))
        (filter (read-from-string filter-string))
        (computation (computations-revaluate filter))
        (value (make-list (options 'order) #f)))

    (computation-register-unique-name (string->symbol (options 'name))
                          (string-append "filtered value of: " filter-string)
                          '() (lambda () 
                                (case  (options 'type)
                                  ((lowpass) (last value))
                                  ((highpass) (let ((c (computation))) (if c (- (first c) (last value)) #f)))
                                  (else (error "unknown filter type" (options 'type))))))


    (verbose "created " (integer->primary-name (options 'order)) " order " (options 'type) " filter '"
             (options 'name) "' filtering '" filter-string "' @ "
             (/ (options 'update-period)) "hz")
    (create-periodic-task
     `(filter ,filter)
     (options 'update-period)
     (lambda ()
       (let ((c (computation)))
         (set! value
               (if c (let ((fc (first c))
                           (lp (*
                                (current-task-period) (options 'frequency))))
                       (let each-order ((value value)
                                        (fc fc))
                         (if (null? value) '()
                             (let ((filtered-value
                                    (if (not (first value)) fc
                                        (case (options 'type)
                                          ((lowpass highpass) (+ (* lp fc) (* (- 1 lp) (first value))))
                                          (else (error "unknown filter type" (options 'type)))))))
                               (cons filtered-value
                                     (each-order (cdr value) filtered-value))))))
                   (cons #f (cdr value)))))))))
