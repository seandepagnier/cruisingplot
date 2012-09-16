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

(define (create-motor-options)
  (create-options
   `(,(make-string-verifier 'motordevice "serial device of motor" "none")
     ,(make-baud-verifier 'serialbaud "serial baud rate to use" 9600)
     ,(make-number-verifier 'min-motor-duty "minimum duty cycle" 0 0 1)
     ,(make-number-verifier 'max-motor-duty "maximum duty cycle" .2 0 1)
     )
  "no examples" #f))

(define (motor-open motor-options)
  (let-values (((motor-input motor-output)
                (if (equal? (motor-options 'motordevice) "none")
                    (values #f #f)
                    (open-serial-device (motor-options 'motordevice) (motor-options 'serialbaud)))))
    (let ((motor (list motor-input motor-output motor-options 'stopped default-motor-calibration)))
      (motor-command motor 0) ; stop motor and flush buffer
      (if motor-input
      (make-line-reader motor-input
                        (lambda (line)
                          (cond ((eof-object? line)
                                 (print "motor device closed unexpectedly")
                                 (task-sleep 1)
                                 (let ((new-motor (motor-open motor-options)))
                                   (set-car! motor (car new-motor))
                                   (set-cdr! motor (cdr new-motor))
                                   (task-exit)))
                                ((string= "o" line) (print "overflow in motor buffer"))
                                ((string= "f" line) (print "overflow in duty cycle"))
                                ((string= "u" line) (print "underflow in duty cycle"))
                                ((string= "k" line) 'ok)
                                ((string= "i" line) (error "invalid command to motor"))
                                ((string= "e" line)
                                 (motor-set-state! motor 'stopped))
                                (else
                                 (print "got from motor unhandled message: " line))))))
      motor)))

(define motor-input first)
(define motor-output second)
(define motor-options third)
(define motor-state fourth)
(define motor-calibration fifth)

; simrad hacked
;(define default-motor-calibration '(0 7.8824 0.8861 21.49))

(define default-motor-calibration '(0 1 0 0))

(define (motor-apply-calibration calibration velocity)
  (+ (first calibration)
     (* (second calibration) velocity)
     (* (third calibration) (square velocity))
     (* (fourth calibration) (cube velocity))))

(define (motor-set-state! motor state)
  (set-car! (cdddr motor) state))

; state can be:
; 'moving - moving at last command
; 'stopped - reached the end

; command motor to turn at a given rate (or duty cycle)
(define (motor-command motor duty)
  (let ((command (inexact->exact (round (* 100 duty)))))
    (verbose "autopilot motor command:  " command)
    (cond ((motor-output motor)
           (write command (motor-output motor))
           (newline (motor-output motor))
           (flush-output (motor-output motor))
           (motor-set-state! motor (if (zero? duty) 'stopped 'moving))))))

; calibrated motor can be commanded in velocity
(define (motor-command-velocity motor velocity)
  (motor-command motor
                 (let ((calduty (motor-apply-calibration (motor-calibration motor) velocity))
                       (minduty ((motor-options motor) 'min-motor-duty))
                       (maxduty ((motor-options motor) 'max-motor-duty)))
                   (cond ((> (abs calduty) maxduty) (* (/ calduty (abs calduty)) maxduty))
                         ((< (abs calduty) minduty) 0)
                         (else calduty)))))

; run motor at various speeds to full stops
; alternating direction to build a lookup table
; to calibrate out linearities.
; 
; distance / time = velocity
; normalized distance of 1
; velocity = 1 / time
; duty * cal[duty] = velocity

(define (calibrate-motor motor)
  (let ((raw-points
         (let ((duty-width (- ((motor-options motor) 'max-motor-duty)
                              ((motor-options motor) 'min-motor-duty)))
               (count 4))
           (let each-speed ((index -1) (sign -1))
             (cond ((< index count)
                    (let ((timer (start-timer)))
                      (let ((duty (if (negative? index) sign
                                      (* sign (+ ((motor-options motor) 'min-motor-duty)
                                             (* index (/ duty-width (- count 1))))))))
                      (motor-command motor duty)
                      (print "waiting until end for duty " duty)
                      (let wait-for-it ()
                        (cond ((eq? (motor-state motor) 'moving)
                               (task-sleep .01)
                               (wait-for-it))
                              (else
                               (cons (list (/ sign (timer)) duty)
                                     (each-speed (+ index (if (negative? sign) 1 0)) (- sign)))))))))
                   (else '()))))))

    ; remove first point (getting in position negative index) and add zero point
    (let ((points (cons '(0 0) (cdr raw-points))))
      (print "points " points)
      (polynomial-regression points 5))))
