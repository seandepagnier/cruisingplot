;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

(declare (unit weather))

;; This file handles reading wind speed and direction data from
;; a davis vantage pro 2 weather station

(type-register 'weather '(barometer
                          inside-temperature inside-humidity temperature
                          wind-speed wind-speed-10-min-avg wind-direction
                          humidity rain-rate))

(define (davis-weather-reader i o index)
  (create-periodic-task
   (string-append "davis-weather-reader " (number->string index)) 1.5
   (lambda () 
     (display "LOOP 1\n" o)
     (let ((timeout (+ (current-milliseconds) 2)))
       (let packet ((data '()))
         (cond ((> (current-milliseconds) timeout)
                (task-sleep))
               ((char-ready? i)
                (let ((count (length data))
                      (c (read-char i)))
                 (cond ((or (and (= count 0) (not (eq? c #\L)))
                            (and (= count 1) (not (eq? c #\O)))
                            (and (= count 2) (not (eq? c #\O))))
                        (packet data))
                       ((= count 98)
                        (parse-loop-packet (reverse data) index))
                       (else
                        (packet (cons c data))))))
              (else
               (task-sleep .1)
               (packet data))))))))

(define (weather-setup device)
  (let-values (((i o) (open-serial-device device 19200)))
    (davis-weather-reader i o (sensor-index-count 'weather))))

(define (parse-loop-packet line index)
  (very-verbose "parse-loop-packet " line index)
  (define (char-at ref)
    (char->integer (list-ref line ref)))
  (define (short-at ref)
    (+ (* 256 (char-at (+ ref 1))) (char-at ref)))

  (sensor-update `(weather ,index)
                 `(,(short-at 7)                ; barometer
                   ,(F->C (/ (short-at 9) 10))  ; inside-temperature
                   ,(/ (char-at 11) 100)        ; inside-humidity
                   ,(F->C (/ (short-at 12) 10)) ; temperature
                   ,(mph->knots (char-at 14))   ; wind-speed
                   ,(mph->knots (char-at 15))   ; wind-speed-10-min-avg
                   ,(short-at 16)               ; wind-direction
                   ,(/ (char-at 33) 100)        ; humidity
                   ,(/ (short-at 41) 100)       ; rain-rate
                   )))
