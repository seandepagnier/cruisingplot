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

(define (create-autopilot arg)
  (define options
    (create-options
     `(,(make-number-verifier 'period "how fast to reach course in seconds" 5 1 10)
       ,(make-number-verifier 'damping-factor "damping factor, 1 for critical" 1 0 10)
       ,(make-unspecified-number-verifier 'heading "compass heading to hold, 0-360 or -1 for current" #f 0 360)
       )
     "-autopilot heading=90,period=5,damping-factor=.8"
     (create-motor-options)))

  (parse-basic-options-string options arg)

  (let ((gain .1)
        (motor (motor-open options))
        (last-heading #f)
        (last-heading-rate #f)
        (desired-heading (options 'heading)))

  (create-periodic-task
   "autopilot" 1
   (lambda ()
     (let ((heading (computation-calculate 'magnetic-heading)))
       (cond ((not heading) (print "autopilot is waiting for magnetic heading update"))
             (else
              (if (not desired-heading)
                  (set! desired-heading heading))
              (let*((heading-error (phase-difference-degrees heading desired-heading))
                    (heading-rate (phase-difference-degrees heading last-heading))
                    (heading-rate-rate (phase-difference-degrees heading-rate last-heading-rate)))
                
                (print "heading " heading "heading-error " heading-error
                       " heading-rate " heading-rate
                       " heading-rate-rate " heading-rate-rate)

                (set! last-heading heading)
                (set! last-heading-rate heading-rate)

                (if (and (> (abs heading-error) 3) (> (abs heading-rate) 3))
                    (set! gain
                          (compute-autopilot-gain heading-error heading-rate
                                                  heading-rate-rate (options 'period))))

                (motor-command-velocity motor
                                        (calculate-filter-command
                                         heading-error heading-rate
                                         (options 'period) (options 'damping-factor)
                                         .1 ;gain
                                         ))))))))))


(define windvane-control-port #f)
(define (set-windvane-control-port! port)
  (set! windvane-control-port port))

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
(define (calculate-filter-command heading-error heading-rate period damping-factor gain)
;  (print "args " heading-error " " heading-rate " " period " " damping-factor " " gain)
  (let ((w (/ period))
        (c damping-factor))
    (- (* gain (+ (* 2 c w heading-rate) (* (square w) heading-error))))))

; update gain to reach critical dampening
(define (compute-autopilot-gain heading-error heading-rate heading-rate-rate period)
  (let ((df (calculate-dampening-factor heading-error heading-rate
                                        heading-rate-rate ))
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

Using redundant sensors we can compute calibration coefficients.
Assuming there is an overall scale factor already taken out (you can
see how this would be impossible to calculate anyway because its
multiplied by all the sensors)

Each redundant sensor provides a truth equation which can be used
to determine one more than the dimensions additional unknowns.

sensor measurements are (A, B, C etc..)
sensor biases are (Ab, Bb, Cb... etc)
calibration coefficients (a, b, c etc...)
true value for dimension (X, Y, Z)

For 3 accelerometers all in 1 dimensions:

A = X + Ab
B = a*X + Bb
C = b*X + Cb

Each new sensor addes 2 unknowns and also can
be used to solve 2 unknowns.  With 3 sensors,
2 are redundant and allow us to solve for 4
unknowns, there are 5 unknowns, so one bias
cannot be calculated, always have 1 unknown


For 2 dimensions with 4 sensors:
A = X+Ab
B = a*X + b*Y + Bb
C = c*X + d*Y + Cb
D = e*X + f*Y + Db

Possible to calculate 6 unknowns, or a-f, but no biases
With an additional sensor:
E = g*X + h*Y + Eb
we now get a-h as well as 1 bias.

With more sensors we will always have 4 unknowns, one must
be a bias, but we can spread the others freely.

With 3 dimensions and 9 sensors:

A = X+Ab
B = a*X + b*Y + Bb
C = c*X + d*Y + e*Z + Cb
D = f*X + g*Y + h*Z + Db
E = i*X + j*Y + k*Z + Eb
F = l*X + m*Y + n*Z + Fb
G = o*X + p*Y + q*Z + Gb
H = r*X + s*Y + t*Z + Hb
I = u*X + v*Y + w*Z + Ib

can find all 24 unknowns (a-w) (4 per redundant axis) as well as overall error term (one bias)
There no matter how many sensors there will be 8 unknowns for 3 dimensions

If it is possible to determine bias from some other means, we can completely calibrate sensors in 3d as long as there are at least 8 linearly independant axes.  More axes may increase accuracy and/or give error feedback to bias estimation.

In the simpler case of only 4 axes, it is possible to compute 3 of the biases as well as 1 other term if all the other calibration terms are known.. since only bias terms change over time, once the calibration terms are well-estimated, it should be possible to use this method to calculate all biases except 1.

It also should be possible to find all non-linearities iteratively since each redundant axis would allow for much more than only 4 unknowns.
