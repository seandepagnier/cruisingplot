;; Copyright (C) 2011 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version. 

; very simple motor control driver where ascii number is sent over a serial port
; command a motor for a specified number of milliseconds (+ or - for forward
; or backwards)

(declare (unit motor))

(define (motor-options)
  `(,(make-string-verifier "serial device of motor" 'motordevice "/dev/ttyUSB0")
    ,(make-baud-verifier "serial baud rate to use" 'serialbaud 19200)))

(define (motor-open motor-options)
  (let-values (((motor-input motor-output)
                (open-serial-device (motor-options 'motordevice) (motor-options 'serialbaud))))
    (list motor-input motor-output motor-options)))

(define motor-input first)
(define motor-output second)
(define motor-options third)

(define (motor-command motor command)
  (verbose "motor at " ((motor-options motor) 'motor-device) " command: " command)
  (write (inexact->exact (round command)) motor)
  (newline motor))
