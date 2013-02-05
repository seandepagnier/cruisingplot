;; Copyright (C) 2011, 2012 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version. 

(declare (unit autopilot))

; make && ./cruisingplot -v -swindvane-control  -ause-windvane  -d wind-vane

;  The goal is to keep the boat on course by measuring it's motion,
;  then estimating what the motion will be in the future with various
;  helm changes.

(define desired-heading #f)
(define sensor-history '())
(define pitch-misalignment #f)

(define gps-magnetic-declination #f)
(define gps-magnetic-declination-timeout 0)

(define (new-derivative-value initial-value)
  (list initial-value 0 0))

(define (update-derivative-value derivative value subtract-values)
  (if (first derivative)
      (let*((d (subtract-values value (first derivative)))
            (dd (subtract-values d (second derivative))))
        (list value d dd (fourth derivative)))
      (list value 0 0 subtract-values)))
                 
(define (create-autopilot arg)
  (define options
    (create-options
     `(,(make-number-verifier 'period "how fast to reach course in seconds" 10 1 20)
       ,(make-number-verifier 'dampening-factor "dampening factor, 1 for critical" 1 0 10)
       ,(make-boolean-verifier 'use-windvane "use wind vane for primary feedback" 'false)
       ,(make-boolean-verifier 'use-gps-heading "use gps instead of magnetic heading" 'false)
       ,(make-unspecified-number-verifier 'heading "compass heading to hold, 0-360 or -1 for current" #f 0 360)
       )
     "-autopilot heading=90,period=5,dampening-factor=.8"
     (create-motor-options)))
  (parse-basic-options-string options arg)
  (set! desired-heading (options 'heading))
    ; read from sensors at 10hz and average
  (create-periodic-task "autopilot-sensor-reader" .1 update-sensor-history)
    (let ((heading (new-derivative-value #f)) (windvane (new-derivative-value #f))
          (gain-h .03)                  (gain-r .02)
          (pitch-tilt-angle 5)          (roll-round-up-factor 0)
          (heading-history '())
          (motor (motor-open options motor-command)))
  (create-periodic-task
   "autopilot-update" 1

   (lambda ()
     (update-gps-declination)

     (let*((pitch-compensated-sensor-history
            (compensate-history-pitch sensor-history pitch-tilt-angle))
           (heading1 (heading-from-history-1 pitch-compensated-sensor-history))
           (heading2 (heading-from-history-2 pitch-compensated-sensor-history))
           (roll (roll-from-history-1 pitch-compensated-sensor-history))
           (gps-heading (computation-calculate-exists 'gps-heading))
           (gps-magnetic-heading (if (and gps-heading gps-magnetic-declination)
                                     (+ gps-heading gps-magnetic-declination) #f)))
       (set! heading (update-derivative-value heading
                                        (if (options 'use-gps-heading)
                                            gps-magnetic-heading heading1)
                                        phase-difference-degrees))
       (set! windvane (update-derivative-value windvane (sensor-query 'windvane) -))
     (cond ((options 'use-windvane)
            (cond ((first windvane)
                   (let ((data (map (lambda (s) (list (first s) (fourth s))) sensor-history)))
                     (if (not (any not (map (lambda (x) (not (any not x))) data)))
                         (let ((max-wave (find-wave data 2 17)))
                           (print "max-wave " max-wave))))
                   (let ((cmd (calc-windvane-command windvane heading gps-magnetic-heading)))
                     (cond (windvane-control-port
                            (write (* 100 cmd) windvane-control-port)
                            (newline windvane-control-port)))
                     (motor-command-velocity motor cmd)))
                  (else (print "waiting on windvane input"))))
           ((first heading)
            (if (not desired-heading)   (set! desired-heading (first heading)))
            (let*((heading-error (phase-difference-degrees
                                  (first heading) (- desired-heading
                                                     (if roll (* roll roll-round-up-factor) 0)))))
                ; error and rate must not be zero for feedback..
                ; we are already perfect in this case
                (if (not (and (zero? heading-error) (zero? (second heading))))
                    (set! heading-history
                          (append (if (>= (length heading-history) 30)
                                      (cdr heading-history)
                                      heading-history)
                                  (list (list heading-error (second heading) (third heading))))))
                ; PID command = a*heading-error + b*accumulated-error + c*heading-rate
                (let ((cmd (+ (* gain-h heading-error)
                                                 (* gain-r (second heading)))))
                  (cond (windvane-control-port
                         (write (* 100 cmd) windvane-control-port)
                         (newline windvane-control-port)))
                  (motor-command-velocity motor cmd))))
           (else (print "autopilot is waiting for heading update"))))))))

(define (update-gps-declination)
  (cond ((and (computation-exists? 'gps-heading) (or (not gps-magnetic-declination)
                                                     (>= gps-magnetic-declination-timeout 1000)))
         (set! gps-magnetic-declination (computation-calculate 'gps-magnetic-declination))
         (set! gps-magnetic-declination-timeout 0))
        (else
         (set! gps-magnetic-declination-timeout
               (+ gps-magnetic-declination-timeout 1)))))

(define (compensate-history-pitch sensor-history pitch-tilt-angle)
  (let ((current-pitch (pitch-from-history-1 sensor-history)))
    (set! pitch-misalignment
          (cond ((and current-pitch pitch-misalignment)
                 (+ (* .99 pitch-misalignment)
                    (* .01 current-pitch)))
                (current-pitch current-pitch)
                (else #f)))
    (history-cancel-pitch (+ pitch-tilt-angle
                             (if pitch-misalignment pitch-misalignment 0))
                          sensor-history)))

(define (update-sensor-history)
  (let ((accel (sensor-query-indexes 'accel '(0 1 2)))
        (mag (sensor-query-indexes 'mag '(0 1 2)))
        (wind-vane (sensor-query 'windvane)))
    (let ((measurement (list accel mag wind-vane)))
      (set! sensor-history
            (append (if (>= (length sensor-history) 120)
                        (cdr sensor-history) sensor-history)
                    (list (cons (elapsed-seconds) measurement)))))))

(define (motor-command command)
  (case command
    ((port) (set! desired-heading
                  (phase-difference-degrees desired-heading 3)))
    ((port-long) (set! desired-heading
                       (phase-difference-degrees desired-heading 15)))
    ((starboard) (set! desired-heading
                       (phase-difference-degrees desired-heading 3)))
    ((starboard-long) (set! desired-heading
                            (phase-difference-degrees desired-heading 15)))))


(define (+map a b)
  (if (or (not a) (not b)
          (any not a) (any not b))
      #f (map + a b)))

(define (pitch-from-history-1 history)
  (let ((history-accel-sum (fold +map
                                 '(0 0 0) (map cadr history)))
        (l (length history)))
    (if (or (zero? l) (not history-accel-sum)) #f
        (accelerometer-pitch
         (map (lambda (h) (/ h l)) history-accel-sum)))))

(define (history-cancel-pitch pitch history)
;  (print "history-cancel-pitch " pitch history)
  (let ((q (angle-vector->quaternion (deg2rad (if pitch pitch 0)) '(0 1 0))))
    (map (lambda (h)
           (list (first h) 
                 (if (any not (second h)) (second h) (apply-quaternion-to-vector q (second h)))
                 (if (any not (third h)) (third h) (apply-quaternion-to-vector q (third h)))
                 (fourth h)))
         history)))

(define (roll-from-history-1 history)
  (let ((history-accel-sum (fold +map '(0 0 0) (map cadr history)))
        (l (length history)))
    (if (or (zero? l) (not history-accel-sum)) #f
        (accelerometer-roll
         (map (lambda (h) (/ h l)) history-accel-sum)))))

(define (heading-from-history-1 history)
  (let ((history-sum
         (fold (lambda (a b) 
                 (if (or (not a) (not b)
                         (any not (map (lambda (x) (not (any not x))) a))
                         (any not (map (lambda (x) (not (any not x))) b)))
                     #f
                     (list (map + (first a) (first b))
                           (map + (second a) (second b)))))
               '((0 0 0) (0 0 0)) (map (lambda (h) (list (second h) (third h))) history)))
        (l (length history)))
    (if (or (not history-sum) (zero? l) (any not (first history-sum)) (any not (first history-sum))) #f
        (let ((accel (map (lambda (h) (/ h l)) (first history-sum)))
              (mag (map (lambda (h) (/ h l)) (second history-sum))))
          (magnetometer-heading accel mag)))))

(define (heading-from-history-2 history)
  (let ((measurement-history (map cdr history))
        (l (length history)))
    (let ((accels (map first measurement-history))
          (mags (map second measurement-history)))
      (if (or (zero? l)
              (any (lambda (x) (any not x)) accels)
              (any (lambda (x) (any not x)) mags))
          #f
          (/ (fold + 0 (map magnetometer-heading
                            accels mags)) l)))))

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

; steer to wind algorithm
(define (calc-windvane-command windvane heading gps-magnetic-heading)
  (let ((cmd  (+ (* 2 (first windvane))
                 (* .3 (second windvane)))))
    (print "cmd = " cmd " = 1*" (first windvane) " +  1*" (second windvane))
    cmd))
     
; Create a map based on wind direction vs heading rate of change
; vs rudder position

; balanced on both tacks
; find centerpoint
;
; rate of change of 5

; adaptive autopilot uses gps data as feedback to self-calibrate
; 
; this autopilot uses pre-calibrated mag and accel data
; 

