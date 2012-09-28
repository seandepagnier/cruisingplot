;; Copyright (C) 2011, 2012 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version. 

(declare (unit autopilot))

;  The goal is to keep the boat on course by measuring it's motion,
;  then estimating what the motion will be in the future with various
;  helm changes.

(define (create-autopilot arg)
  (define options
    (create-options
     `(,(make-number-verifier 'period "how fast to reach course in seconds" 10 1 20)
       ,(make-number-verifier 'dampening-factor "dampening factor, 1 for critical" 1 0 10)
       ,(make-unspecified-number-verifier 'heading "compass heading to hold, 0-360 or -1 for current" #f 0 360)
       )
     "-autopilot heading=90,period=5,dampening-factor=.8"
     (create-motor-options)))

  (parse-basic-options-string options arg)

  (let ((gain .1)
        (motor (motor-open options))
        (last-heading #f)
        (last-heading-rate #f)
        (desired-heading (options 'heading))
        (w-history '())
        (heading-history '())
        (gain-h 2)
        (gain-r 6))

    ; read from sensors at 10hz
  (create-periodic-task
   "autopilot-sensor-reader" .1
   (lambda ()
     (let ((accel (sensor-query-indexes 'accel '(0 1 2)))
           (mag (sensor-query-indexes 'mag '(0 1 2))))
       ; look for period of 1-10 seconds in accelerometer force
       ; by logging last 100 points
       (if (not (any not mag))
           (let ((f 
                  (list accel mag)
                  ))
             (set! w-history
                   (append (if (>= (length w-history) 30)
                               (cdr w-history)
                               w-history)
                           (list (list (elapsed-seconds) f))))
             )
           ))))

  (create-periodic-task
   "autopilot-update" 1
   (lambda ()
     (let ((heading (heading-from-history-1 w-history)))
       (cond ((not heading) (print "autopilot is waiting for magnetic heading update"))
             (else
              (if (not desired-heading)
                  (set! desired-heading heading))
              (if (not last-heading)
                (set! last-heading heading))
              (if (not last-heading-rate)
                (set! last-heading-rate 0))
              (let*((heading-error (phase-difference-degrees heading desired-heading))
                    (heading-rate (phase-difference-degrees heading last-heading))
                    (heading-rate-rate (phase-difference-degrees heading-rate last-heading-rate)))
                (print "heading " (round-to-places heading 2)
                       " h-1-2- " (round-to-places (- heading (heading-from-history-2 w-history)) 2)
                       " heading-error " (round-to-places heading-error 2)
                       " heading-rate " (round-to-places heading-rate 2)
                       " heading-rate-rate " (round-to-places heading-rate-rate 2))

                (set! last-heading heading)
                (set! last-heading-rate heading-rate)

                ; error and rate must not be zero for feedback..
                ; we are already perfect in this case
                (if (not (and (zero? heading-error) (zero? heading-rate)))
                    (set! heading-history
                          (append (if (>= (length heading-history) 30)
                                      (cdr heading-history)
                                      heading-history)
                                  (list (list heading-error heading-rate heading-rate-rate)))))
                                        ; h'' + 2 c w h' + w^2 h = 0
                                        ; m = a h' + b h

                ; update gains
                (let* ((d (calculate-dampening-factor heading heading-rate
                                                     heading-rate-rate (options 'period)))
                       (dd (/ (- (options 'dampening-factor) d) 100))
                       (dr (cond ((< dd -1e-1) -1e-1)
                                 ((> dd 1e-1) 1e-1)
                                 (else dd))))
                  (print " d " d " dd " dd " dr " dr)
                  (if (> (abs heading-error) (abs heading-rate))
                      (set! gain-h (+ gain-h dr))
                      (set! gain-r (+ gain-r dr)))
                  (print "new gain gain-h " gain-h " gain-r " gain-r)
                  (cond ((> gain-h 10) (set! gain-h 10))
                        ((< gain-h 1) set! gain-h 1))
                  (cond ((> gain-r 10) (set! gain-r 10))
                        ((< gain-r 1) (set! gain-r 1))))
             
                ; command motor
                (let ((command (+ (* gain-h heading-error)
                                  (* gain-r heading-rate))))
                  (print "command " command)
                (motor-command-velocity motor
                                        command))))))))))

(define (heading-from-history-1 w-history)
  (let ((history-sum
         (fold (lambda (a b) (list (map + (first a) (first b))
                                   (map + (second a) (second b))))
               '((0 0 0) (0 0 0)) (map second w-history)))
        (l (length w-history)))
    (if (zero? l) #f
    (let ((accel (map (lambda (h) (/ h l)) (first history-sum)))
          (mag (map (lambda (h) (/ h l)) (second history-sum))))
      (magnetometer-heading accel mag)))))

(define (heading-from-history-2 w-history)
  (let ((history (map second w-history))
        (l (length w-history)))
    (if (zero? l) #f)
    (/ (fold + 0 (map magnetometer-heading
                      (map first history)
                      (map second history))) l)))

;(define windvane-control-port #f)
;(define (set-windvane-control-port! port)
;  (set! windvane-control-port port))

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
    (let ((num (- (+ heading-rate-rate (* (square w) heading))))
          (dem (* 2 w heading-rate)))
      (cond ((zero? num) 0)
             ((zero? dem) (* num +inf))
             (/ num dem)))))
;
; command autopilot based on h''
;
; command = -gain(2 c w h' + w^2 h)
(define (calculate-filter-command heading-error heading-rate period dampening-factor gain)
;  (print "args " heading-error " " heading-rate " " period " " dampening-factor " " gain)
  (let ((w (/ period))
        (c dampening-factor))
    (- (* gain (+ (* 2 c w heading-rate) (* (square w) heading-error))))))

; update gain to reach critical dampening
(define (compute-autopilot-gain heading-error heading-rate heading-rate-rate period gain)
  (let ((df (calculate-dampening-factor heading-error heading-rate
                                        heading-rate-rate period))
        (lp .1))
    (let ((newfac (cond ((> df 1.1) 1.02)
                        ((< df .9)   .98)
                        (else 0))))
      (let ((new-gain (+ (* gain newfac lp) (* (- 1 lp) gain))))
            (print "gain " gain " new-gain " new-gain " df " df)
            new-gain))))

; adaptive autopilot uses gps data as feedback to self-calibrate
; 
; this autopilot uses pre-calibrated mag and accel data
; reducing errors in initial condiitons but not limiting modifying bias and scale
; with time (as this is needed for gyros anyway)
; redundant sensors are automatically supported as their weights can be calculated
; in the same feedback loop
; 

