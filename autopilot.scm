;; Copyright (C) 2011 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version. 

(declare (unit autopilot))

;  The goal is to keep the boat on course by measuring it's motion,
;  then estimating what the motion will be in the future with various
;  helm changes.


; h'' + 2 c w h' + w^2 h = 0
;
;              2
;     - h'' - w h
; c = -----------
;        2 w h'
;
; w = 1 / period
; 
; for critical dampening, c = 1
(define (calculate-dampening-factor heading heading-rate heading-rate-rate period)
  (let ((w (/ period)))
    (/ (+ (- heading-rate-rate) (- (* (square w) heading)))  (* 2 w heading-rate))))
;
; command autopilot based on h''
;
; cmd = -gain(2 c w h' + w^2 h)
(define (calculate-filter-cmd heading-error heading-rate period damping-factor gain)
  (let ((w (/ period))
        (c damping-factor))
    (- (* gain (+ (* 2 c w heading-rate) (* (square w) heading-error))))))
         

(define (create-autopilot arg)
  (define options
    (create-options
     `(,(make-number-verifier 'period "how fast to reach course in seconds" 3 1 10)
       ,(make-number-verifier 'heading-error-tolerance "acceptable heading error in degrees" 5 1 10)
       ,(make-number-verifier 'heading "compass heading to hold" 0 0 360)
       ,(make-number-verifier 'damping-factor "damping factor, 1 for critical" 1 0 10)
       ,(make-number-verifier 'wind-angle "apparent wind angle to hold" 0 -180 180))
     "-autopilot heading=90"
     (create-motor-options)))

(parse-basic-options-string options arg)
  (define gain .1)
  (define motor (motor-open options))

  (define heading-rate 0)
  (define heading-rate-rate 0)

;(create-task "motor cal task" (lambda () (calibrate-motor motor)))

  (create-periodic-task
   "autopilot" 1
   (lambda ()
     (let*((he (- (computation-calculate 'heading) (options 'heading)))
           (hr (- he heading-error)))
       (set! heading-rate-rate (- hr heading-rate))
       (set! heading-rate hr)
       (set! heading-error he)

       (print "heading-error " heading-error
              " heading-rate " heading-rate
              " heading-rate-rate " heading-rate-rate)

       ; update gain to reach critical dampening
       (let ((df (calculate-dampening-factor heading-error heading-rate heading-rate-rate (options 'period)))
             (lp .1))
         (let ((new-gain (+ (/ lp df) (* (- 1 lp) gain))))
           (print "gain " gain " new-gain " new-gain " df " df)
           (set! gain new-gain)))

       (motor-command-velocity motor
                      (let ((fc (calculate-filter-command heading-error heading-rate
                                           (options 'period) (options 'damping-factor) gain)))
                        (let ((mc (number-in-bounds fc (- max-motor-cmd) max-motor-cmd)))
                          (if (and (> (abs heading-error) (options 'heading-error-tolerance))
                                   (> mc (options 'min-motor-cmd)))
                              mc 0))))))))
