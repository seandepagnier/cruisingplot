;; Copyright (C) 2011 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version. 

(declare (unit ahrs))
(declare (uses sensor options computation infix2prefix))

(use srfi-1 environments)

(define (sensor-9dof-setup device)
  (let ((sensor-names '(accel accel accel
                        gyro gyro gyro
                        mag mag mag)))
    (let ((sensor-indexes (map sensor-new-index sensor-names)))
      (let-values (((i o) (open-serial-device device 38400)))
                                        ; set to output all sensors
        (create-task
         "9 dof reader setup"
         (lambda ()
           (let init ((index 0))
             (cond ((< index 8)
                    (display "4" o)
                    (flush-output o)
                    (task-sleep .05)
                    (init (+ index 1)))))
        
           (make-line-reader
            i
            (lambda (line)
              (let ((values (string-split line ",")))
                (cond ((< (length values) 2) #t) ; startup lines (no comma) and empty ok
                      ((and (= (length values) 11)
                            (equal? (list-ref values 0) "$")
                            (equal? (list-ref values 10) "#"))
                       (map (lambda (name index value)
                              (sensor-update `(,name ,index) (string->number value)))
                            sensor-names sensor-indexes (cdr values)))
                      (else (very-verbose "malformed line from 9dof: " line))))))))))))
    
; basic ahrs combines accels magnetometers and gyroscopes
; the parameters are lists of indices of which sensors to use, or
; 'all to use all available sensors

; If the list is null, then no sensors are used, this is allowed,
; but results in loss of features

(define (default-ahrs-options)
  (create-options
   (list
     (make-indicies-verifier 'accels 'accel)
     (make-indicies-verifier 'mags 'mag)
     (make-indicies-verifier 'gyros 'gyro)
     (make-discrete-verifier 'filter "which algorithm to use" 'complementary '(complementary ekf ukf))
     (make-number-verifier 'period "period of updates" .1 .001 1000))
   (string-append
    "  Create ahrs which updates twice a second, using only even axis accels "
    "and odd axis magnetometers,  (all available gyroscopes however)\n"
    "  '--ahrs=accels=(0 2 4 6 8),magnetometers=(1 3 5 7),period=.5'\n"
    "  Create an ahrs which only uses accels and magnetometers\n"
    "  '--ahrs=gyroscopes=()'\n")
   #f))

(define ahrses 0)
(type-register 'ahrs '(pitch roll yaw pitchrate rollrate yawrate vx vy vz ax ay az timestamp deviation))

(define (make-ahrs-from-string arg)
  (let ((options (default-ahrs-options))
        (lastahrsupdate #f))
    (define time (start-timer))
    (if arg (parse-basic-options-string options arg))

    ; make ahrs -> ahrs.0
    (if (zero? ahrses) (computation-register 'ahrs (lambda () (computation-calculate 'ahrs.0))))

    (computation-register (string->symbol (string-append "ahrs." (number->string ahrses)))
                          (lambda () lastahrsupdate))

    (verbose "starting ahrs task " ahrses)
    (set! ahrses (+ ahrses 1))

    (let ((last-time #f)
          (q (quaternion-identity)))
    (start-periodic-task
     "ahrs-debugging-task" 1
     (lambda ()
       (print "q " q " pry " (quaternion->pitch-roll-yaw q))
       ))

    (start-periodic-task
     "ahrs-computation-task" (options 'period)
       (lambda ()
        (let ((gps-speed (computation-calculate 'gps-speed))
              (accels (map computation-calculate  (options 'accels)))
              (gyros (map computation-calculate  (options 'gyros)))
              (mags (map computation-calculate  (options 'mags)))
              (time (elapsed-seconds)))
          (cond (last-time
                 (let ((period (- time last-time)))
                   (set! q (mahoney-algorithm q gyros accels mags .5 period)))))
          (set! last-time time))))
        )))

(define (mahoney-algorithm q g a m Kp period)
  (let ((norm-a (vector-normalize a)))
    (let-values (((q0 q1 q2 q3) (values q))
                 ((ax ay az) (values a)))
      ; half-v is estimated direction of gravity
    (let*((half-v (list (- (* q1 q3) (* q0 q2))
                        (+ (* q0 q1) (* q2 q3))
                        (+ (* q0 q0) (* q3 q3) -.5)))
      ; half-e is cross product between estimated and measured tilt, which is error
          (half-e (vector-cross norm-a half-v))
          (new-g (vector+ (vector-scale (* 2 Kp) half-e) g))
          (gyro-int (vector-scale (* .5 period) new-g)))
      (vector-normalize (quaternion* q gyro-int))))))

(define (madgwick-algorithm q g a m beta period)
  ; quaternion rate of change from gyros
  (let*((qdot (apply quaternion-rate q g))
        (norm-a (vector-normalize a)))
    (let-values (((q0 q1 q2 q3) (values q))
                 ((ax ay az) (values a)))
      ; gradient decent algorithm corrective step
      (let ((s0 (+ (* 4 q0 q2 q2) (*  2 q2 ax) (* 4 q0 q1 q1) (* -2 q1 ay)))
            (s1 (+ (* 4 q1 q3 q3) (* -2 q3 ax) (* 4 q0 q0 q1) (* -2 q0 ay)
                   (* -4 q1) (* 8 q1 q1 q1) (* 8 q1 q2 q2) (* 4 q1 az)))
            (s2 (+ (* 4 q0 q0 q2) (*  2 q0 ax) (* 4 q2 q3 q3) (* -2 q3 ay)
                   (* -4 q2) (* 8 q2 q1 q1) (* 8 q2 q2 q2) (* 4 q2 az)))
            (s3 (+ (* 4 q1 q1 q3) (* -2 q1 ax) (* 4 q2 q2 q3) (* -2 q2 ay))))
        (let*((norm-s (vector-normalize (list s0 s1 s2 s3)))
              (qdot-update (vector- qdot (vector-scale beta norm-s))))
          (vector-normalize (vector+ q (vector-scale period qdot-update))))))))

;;  for over-determined calibration:

; What I have is:

; X = x
; Y = a*x + b*y
; Z = c*x + d*y + e*z
; W = f*x + g*y + h*z

; X Y Z and W are the measurements from the sensors.  x, y and z are the
; truth values (unknown) and a-h are calibration coefficients.   To
; apply the calibration you need the matrix inverse of the above which
; is fairly obvious.


(define quad-axis-measurements '())
(define (quad-axis-record measurements)
  (set! quad-axis-measurements (cons measurements quad-axis-measurements)))

(define (quad-axis-recalculate)
  (print "quad axis state: "
         (call/cc (lambda (bail)
                    (with-exception-handler
                     (lambda _ (bail #f))
                     compute-quad-least-squares)))))


; grind(expand(eliminate(
; [X = x + Bx,
;  Y = a*x + b*y + By,
;  Z = c*x + d*y + e*z, + Bz
;  W = f*x + g*y + h*z, + Bw], [x, y, z])));

; With some substitution you can get the truth equation:
;
; b*h*Z-d*h*Y+e*g*Y+a*d*h*X-b*c*h*X-a*e*g*X+b*e*f*X-b*e*W+By*d*h-a*Bx*d*h
;      +b*Bx*c*h-b*Bz*h-By*e*g+a*Bx*e*g-b*Bx*e*f+b*Bw*e

; d/da = d*h*X - e*g*X - Bx*d*h + Bx*e*g
; d/db = h*Z - c*h*X + e*f*X - e*W + Bx*c*h - Bz*h - Bx*e*f + Bw*e
; d/dc = b*Bx*h - b*h*X
; d/dd = -h*Y + a*h*X + By*h - a*Bx*h
; d/de = g*Y - a*g*X + b*f*X - b*W - By*g + a*Bx*g - b*Bx*f + b*Bw
; d/df = b*e*X - b*Bx*e
; d/dg = e*Y - a*e*X - By*e + a*Bx*e
; d/dh = b*Z - d*Y + a*d*X - b*c*X + By*d - a*Bx*d + b*Bx*c - b*Bz
; d/dBx =  - a*d*h + b*c*h + a*e*g - b*e*f
; d/dBy = d*h - e*g
; d/dBz = -b*h
; d/dBw = b*e

(define quad-jacobian-partials
  (map string-infix->prefix
       '("d * h * X - e * g * X - Bx * d * h + Bx * e * g"
         "h * Z - c * h * X + e * f * X - e * W + Bx * c * h - Bz * h - Bx * e * f + Bw * e"
         "b * Bx * h - b * h * X"
         " - h * Y + a * h * X + By * h - a * Bx * h"
         "g * Y - a * g * X + b * f * X - b * W - By * g + a * Bx * g - b * Bx * f + b * Bw"
         "b * e * X - b * Bx * e"
         "e * Y - a * e * X - By * e + a * Bx * e"
         "b * Z - d * Y + a * d * X - b * c * X + By * d - a * Bx * d + b * Bx * c - b * Bz"
         " - a * d * h + b * c * h + a * e * g - b * e * f"
         "d * h - e * g"
         " - b * h"
         "b * e")))

(define quad-residual
  (string-infix->prefix "b * h * Z - d * h * Y + e * g * Y + a * d * h * X - b * c * h * X - a * e * g * X + b * e * f * X - b * e * W + By * d * h - a * Bx * d * h + b * Bx * c * h - b * Bz * h - By * e * g + a * Bx * e * g - b * Bx * e * f + b * Bw * e"))

(define (build-quad-jacobian-residual-row state measurements)
  (let ((env (scheme-report-environment 5)))
    (for-each (lambda (symbol value)
                (environment-extend! env symbol value))
                '(a b c d e f g h Bx By Bz Bw)
                (map first (matrix->list state)))
    (environment-extend! env 'X (first measurements))
    (environment-extend! env 'Y (second measurements))
    (environment-extend! env 'Z (third measurements))
    (environment-extend! env 'W (fourth measurements))
    (list
     (map (lambda (exp) (eval exp env)) quad-jacobian-partials)
     (eval quad-residual env))))

(define (quad-complete? state)
  (let ((d (matrix-ref (matrix* (matrix-transpose state) state) 0 0)))
    (verbose "d: " d)
    (< d 1e-1)))

(define (quad-least-squares)
  (least-squares-iterate (matrix-transpose (matrix '((0 1 -1 1 1 1 1 1 0 0 0 0))))
                         build-quad-jacobian-residual-row quad-complete? 10))

(computation-register 'accel-force 
                      (string-append "The magtitude of the accels combined force, "
                                     "non-moving yields 1 (for 1 gravity)")
                      '(accel)
                      (lambda ()
                        (magnitude (sensor-query-indexes 'accel '(0 1 2)))))

;Using redundant sensors we can compute calibration coefficients.
;Assuming there is an overall scale factor already taken out (you can
;see how this would be impossible to calculate anyway because its
;multiplied by all the sensors)
;
;Each redundant sensor provides a truth equation which can be used
;to determine one more than the dimensions additional unknowns.
;
;sensor measurements are (A, B, C etc..)
;sensor biases are (Ab, Bb, Cb... etc)
;calibration coefficients (a, b, c etc...)
;true value for dimension (X, Y, Z)
;
;For 3 accelerometers all in 1 dimensions:
;
;A = X + Ab
;B = a*X + Bb
;C = b*X + Cb
;
;Each new sensor addes 2 unknowns and also can
;be used to solve 2 unknowns.  With 3 sensors,
;2 are redundant and allow us to solve for 4
;unknowns, there are 5 unknowns, so one bias
;cannot be calculated, always have 1 unknown
;
;
;For 2 dimensions with 4 sensors:
;A = X+Ab
;B = a*X + b*Y + Bb
;C = c*X + d*Y + Cb
;D = e*X + f*Y + Db
;
;Possible to calculate 6 unknowns, or a-f, but no biases
;With an additional sensor:
;E = g*X + h*Y + Eb
;we now get a-h as well as 1 bias.
;
;With more sensors we will always have 4 unknowns, one must
;be a bias, but we can spread the others freely.
;
;With 3 dimensions and 9 sensors:
;
;A = X+Ab
;B = a*X + b*Y + Bb
;C = c*X + d*Y + e*Z + Cb
;D = f*X + g*Y + h*Z + Db
;E = i*X + j*Y + k*Z + Eb
;F = l*X + m*Y + n*Z + Fb
;G = o*X + p*Y + q*Z + Gb
;H = r*X + s*Y + t*Z + Hb
;I = u*X + v*Y + w*Z + Ib
;
;can find all 24 unknowns (a-w) (4 per redundant axis) as well as overall error term (one bias)
;There no matter how many sensors there will be 8 unknowns for 3 dimensions
;
;If it is possible to determine bias from some other means, we can completely calibrate sensors in 3d as long as are at least 8 linearly independant axes.  More axes may increase accuracy and/or give error feedback to bias estimation.
;
;In the simpler case of only 4 axes, it is possible to compute 3 of the biases as well as 1 other term if all the other calibration terms are known.. since only bias terms change over time, once the calibration terms are well-estimated, it should be possible to use this method to calculate all biases except 1.
;
;It also should be possible to find all non-linearities iteratively since each redundant axis would allow for much more than only 4 unknowns.
