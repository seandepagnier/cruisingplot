;; Copyright (C) 2010, 2012 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This file handles reading from a magnetometer (and optional additional accelerometer sensors)
;; raw data of the digitalsurveyinstruments design

(declare (unit magnetometer))
(declare (uses sensor leastsquares quaternion))

; (load "utilities.scm") (load "leastsquares.scm") (load "vector.scm") (load "matrix.scm") (load "algebra.scm")

(define accel-mag-3d-calibration #f)
(define calibration-measurements '())

(define (save-calibration filename)
  (cond ((not (equal? filename "none"))
         (verbose "saving magnetometer calibration to " filename)
         (with-output-to-file filename
           (lambda () (write calibration-measurements))))))

(define (load-calibration filename)
  (if (equal? filename "none")
      (verbose "not loading calibration for magnetometer")
      (call/cc (lambda (bail)
                (verbose "loading magnetometer calibration from " filename)
                (with-exception-handler
                 (lambda _ (verbose "failed to open file " filename
                                    " for reading, magnetometer calibration empty")
                         (bail #f))
                 (lambda ()
                   (with-input-from-file filename
                     (lambda () (set! calibration-measurements (read))))))))))

(define (add-measurement measurement)
  (set! calibration-measurements (cons measurement calibration-measurements)))

(define (calibrated-calibration-measurements)
  (map (lambda (measurement)
         (let ((time (first measurement))
               (accel (second measurement))
               (mag   (third measurement)))
           (cons time (apply-accel-mag-3d accel mag accel-mag-3d-calibration))))
       calibration-measurements))

; remove find how far a measurement is from norm (magnitude 1) and divide this
; by the distance when normalized to the nearest normalized calibration point.  The greatest value here
; is worst and should be removed.

(define (normalize-measurements calibrated-calibration-measurements)
  (map (lambda (measurement)
         (cons (car measurement) (map normalize (cdr measurement))))
       calibrated-calibration-measurements))

;c-calaccel: (1.005 -0.113 1.006 -0.036 0.989 0.016)
;c-calmag  : (0.955 0.009 -0.067 -0.089 -0.046 0.873 0.047 0.631 -0.044 -0.088 1.254 -0.397)
;c-caldip  : 68.188 err 0.433

;c-calaccel: (1.001 -0.111 0.995 -0.037 1.006 0.009)
;c-calmag  : (0.945 -0.046 -0.067 -0.147 -0.004 0.885 -0.025 0.653 0.006 -0.058 1.039 -0.303)
;c-caldip  : 65.338 err 0.29

;c-calaccel: (1.01 -0.091 0.99 -0.039 1.002 0.024)
;c-calmag  : (0.948 -0.014 0.003 -0.131 -0.069 0.901 -0.004 0.679 -0.076 -0.079 1.111 -0.326)
;c-caldip  : 66.111 err 0.379

;c-calaccel: (1.029 -0.084 0.005 1.005 -0.034 -0.03 -0.022 0.985 0.03)
;c-calmag  : (0.953 -0.057 0.012 -0.2 -0.013 0.901 -0.009 0.618 -0.026 -0.058 1.021 -0.311)
;c-caldip  : 64.207 err 0.39

;c-calaccel: (1.06 -0.079 -0.078 0.995 -0.069 -0.012 0.051 0.999 0.013)
;c-calmag  : (0.996 0.035 -0.013 -0.131 -0.146 0.94 0.009 0.681 -0.027 -0.088 1.075 -0.339)
;c-caldip  : 63.959 err 0.374



(define (compute-culling-metric calibrated-measurement
                                normalized-calibrated-measurement
                                normalized-calibrated-calibration-measurements)
  (let ((time (first calibrated-measurement))
        (cal-accel (second calibrated-measurement))
        (cal-mag (third calibrated-measurement))
        (norm-cal-accel (second normalized-calibrated-measurement))
        (norm-cal-mag (third normalized-calibrated-measurement)))
    (let each-minimum ((min-dist-accel #f)
                       (min-dist-mag #f)
                       (measurements normalized-calibrated-calibration-measurements))
      (cond ((null? measurements)
             (if (or (not min-dist-accel) (not min-dist-mag))
                 (error "min dist not achieved"))
             (if (< (- (elapsed-seconds) time) 10)
                 0
                 (let ((ane (- 1 (magnitude cal-accel)))
                       (mne (- 1 (magnitude cal-mag)))
                       (dipe (+ (last (first accel-mag-3d-calibration))
                               (dot norm-cal-accel norm-cal-mag))))
;                   (print "ane " ane " mne " mne " dipe " dipe " mda " min-dist-accel " mdm " min-dist-mag)
                   (if (or (> min-dist-accel .1) (> min-dist-mag .1))
                       1e-5
                       (+ (square ane) (square mne) (square dipe))))))
            (else
             (let ((cur-norm-cal-accel (second (car measurements)))
                   (cur-norm-cal-mag (third (car measurements))))
               (let ((dist-accel (distance norm-cal-accel cur-norm-cal-accel))
                     (dist-mag (distance norm-cal-mag cur-norm-cal-mag)))
                 (if (and (not (zero? dist-accel))
                          (not (zero? dist-mag))
                          (or (not min-dist-accel) (not min-dist-mag)
                              (< (+ (square dist-accel) (square dist-mag))
                                 (+ (square min-dist-accel) (square min-dist-mag)))))
                     (each-minimum dist-accel dist-mag
                                   (cdr measurements))
                     (each-minimum min-dist-accel min-dist-mag
                                   (cdr measurements))))))))))

(define (cull-calibration-measurements)
  (let*((calibrated-calibration-measurements (calibrated-calibration-measurements))
        (normalized-calibrated-calibration-measurements
         (normalize-measurements calibrated-calibration-measurements))
    ; compute magnitudes and nearest-distance for each measurement
        (measurements-with-metric
         (map (lambda (measurement calibrated-measurement normalized-calibrated-measurement)
                (list measurement
                      (compute-culling-metric
                       calibrated-measurement
                       normalized-calibrated-measurement
                       normalized-calibrated-calibration-measurements)))
                calibration-measurements
                calibrated-calibration-measurements
                normalized-calibrated-calibration-measurements))
; sort measurements based on metric
        (sorted-metric-measurements
         (sort measurements-with-metric
               (lambda (a b) (> (second a) (second b)))
               )))
    (verbose "culling measurement " (map-round-to-places (first (first sorted-metric-measurements)) 2)
             " metric " (second (first sorted-metric-measurements)) )
    (set! calibration-measurements (cdr (map first sorted-metric-measurements)))))

(define (update-calibration-measurements accel mag options)
  (let ((measurement (list (elapsed-seconds) accel mag)))
    (cond ((not (or (any not accel) (any not mag)))
           (add-measurement measurement)
           (if (>= (length calibration-measurements) (options 'max-calibration-points))
                   (cull-calibration-measurements))))))

(define (generate-calibration-measurements count v)
  (let ((sd 1.2) (abd .2) (acd .05) (mbd 1) (mcd .2))
    (let ((nsd (+ 2 (- sd))) (nabd (- abd))
          (nacd (- acd)) (nmbd (- mbd)) (nmcd (- mcd)))
  (let ((a (random-in-range nsd sd)) (b (random-in-range nabd abd))
        (c (random-in-range nacd acd)) (d (random-in-range nsd sd))
        (e (random-in-range nabd abd))
        (f (random-in-range nacd acd)) (g (random-in-range nacd acd))
        (h (random-in-range nsd sd)) (i (random-in-range nabd abd))
        (j (random-in-range nsd sd)) (k (random-in-range nmcd mcd))
        (l (random-in-range nmcd mcd)) (m (random-in-range nmbd mbd))
        (n (random-in-range nmcd mcd)) (o (random-in-range nsd sd))
        (p (random-in-range nmcd mcd)) (q (random-in-range nmbd mbd))
        (r (random-in-range nmcd mcd)) (s (random-in-range nmcd mcd))
        (t (random-in-range nsd sd)) (u (random-in-range nmbd mbd)))
    (print (map-round-to-places
            (list  "a: " a " b: " b " c: " c " d: " d " e: " e " f: " f " g: " g
                  " h: " h " i: " i " j: " j " k: " k " l: " l " m: " m " n: " n
                  " o: " o " p: " p " q: " q " r: " r " s: " s " t: " t " u: " u " v: " v) 3))
    (define (generate-calibration-measurement cax cay caz cmx cmy cmz)
; ax = (cax-b)/a
; ay = -(a*(e-cay)+c*cax-b*c)/(a*d)
; az = -(a*(d*(i-cz)-e*g+cy*g)+b*(c*g-d*f)+cx*(d*f-c*g))/(a*d*h)
;
; dem = (j*(p*s-o*t)+k*(n*t-p*r)+l*(o*r-n*s))
; mx = -(l*(o*(u-cmz)-q*s+cmy*s)+k*(p*(cmz-u)+q*t-cmy*t)+cmx*(o*t-p*s)+m*(p*s-o*t))
;      /dem
; my = (l*(n*(u-cmz)-q*r+cmy*r)+j*(p*(cmz-u)+q*t-cmy*t)+cmx*(n*t-p*r)+m*(p*r-n*t))
;      /dem      
; mz = -(k*(n*(u-cmz)-q*r+cmy*r)+j*(o*(cmz-u)+q*s-cmy*s)+cmx*(n*s-o*r)+m*(o*r-n*s))
;      /dem
      (list
         (list (/ (- cax b) a)
               (- (/ (- (+ (* a (- e cay)) (* c cax)) (* b c)) (* a d)))
               (- (/ (+ (* a (+ (- (* d (- i caz)) (* e g)) (* cay g)))
                        (* b (- (* c g) (* d f))) (* cax (- (* d f) (* c g)))) (* a d h))))
     (let ((dem (+ (* j (- (* p s) (* o t))) (* k (- (* n t) (* p r))) (* l (- (* o r) (* n s))))))
       (list (/ (- (+ (* l (+ (- (* o (- u cmz)) (* q s)) (* cmy s)))
                      (* k (- (+ (* p (- cmz u)) (* q t)) (* cmy t)))
                      (* cmx (- (* o t) (* p s))) (* m (- (* p s) (* o t)))))
                dem)
             (/ (+ (* l (+ (- (* n (- u cmz)) (* q r)) (* cmy r)))
                   (* j (- (+ (* p (- cmz u)) (* q t)) (* cmy t)))
                   (* cmx (- (* n t) (* p r))) (* m (- (* p r) (* n t))))
                dem)
             (/ (- (+ (* k (+ (- (* n (- u cmz)) (* q r)) (* cmy r)))
                      (* j (- (+ (* o (- cmz u)) (* q s)) (* cmy s)))
                      (* cmx (- (* n s) (* o r))) (* m (- (* o r) (* n s)))))
                dem)))))
    (map (lambda (index)
      (let*((accel (random-normalized-vector 3))
            (rand-orthogonal (vector-cross accel (random-normalized-vector 3)))
            (accel-to-mag-quaternion (angle-vector->quaternion (acos (- v)) rand-orthogonal))
            (mag (apply-quaternion-to-vector accel-to-mag-quaternion accel)))
        (cons index
              (let ((rn -.002) (rp .002))
                (map vector+
                     (list (list (random-in-range rn rp) (random-in-range rn rp) (random-in-range rn rp))
                           (list (random-in-range rn rp) (random-in-range rn rp) (random-in-range rn rp)))
                   (generate-calibration-measurement (first accel) (second accel) (third accel)
                                                     (first mag) (second mag) (third mag)))))))
         (sequence 1 count))))))
                      
(define (magnetometer-setup arg)

  (define options
    (create-options
     `(,(make-number-verifier 'max-calibration-points "number of calibration points to use" 48 0 1000)
       ,(make-string-verifier 'calibration-file
                              "file to use for saving and loading calibration between runs" "magcal"))
     "currently the magnetometer supports the first 3 axes of accelerometer and magnetometer sensors"
     #f))

  (parse-basic-options-string options arg)
  (load-calibration (options 'calibration-file))


                  (cond (#f
           (let ((meas (generate-calibration-measurements 64 .5)))
;             (print "meas " (map-round-to-places meas 3))
             (let ((cal (compute-accelerometer-calibration (map second meas))))
               (print "accel-cal "  (map-round-to-places cal 3)))
             (let ((cal (compute-magnetometer-calibration (map third meas))))
               (print "mag-cal "  (map-round-to-places cal 3)))
             (let ((cal (calibrate-sensor-3d (map second meas))))
               (print "accel-sensor-cal "  (map-round-to-places cal 4)))
             (let ((cal (calibrate-sensor-3d (map third meas))))
               (print "mag-sensor-cal "  (map-round-to-places cal 4)))
             (let ((cal (calibrate-basic-accel-mag-3d (map (lambda (m)
                                                       (append (second m) (third m))) meas))))
               (print "basic-accel-mag-cal "  (map-round-to-places cal 3)))
             (let ((cal (calibrate-accel-mag-3d (map (lambda (m)
                                                       (append (second m) (third m))) meas))))
               (print "accel-mag-cal "  (map-round-to-places cal 3)))
           (exit 1))
           ))

  (define accel-read '(0 0 0))
  (define default-accel-read-max '(-2 -2 -2))
  (define accel-read-max default-accel-read-max)
  (define default-accel-read-min '(2 2 2))
  (define accel-read-min default-accel-read-min)

  (define mag-read '(0 0 0))
  (define default-mag-read-max '(-2 -2 -2))
  (define mag-read-max default-mag-read-max)
  (define default-mag-read-min '(2 2 2))
  (define mag-read-min default-mag-read-min)

  (define sensor-read-count 0)

  (create-periodic-task
   "magnetometer-sensor-reader" .1
   (lambda ()
     (let ((accel (map sensor-query (map (lambda (n) (list 'accel n)) '(0 1 2))))
           (mag (map sensor-query (map (lambda (n) (list 'mag n)) '(0 1 2)))))
       (cond ((and accel mag (not (any not accel)) (not (any not mag)))
              (set! accel-read (vector+ accel-read accel))
              (set! accel-read-max (map max accel-read-max accel))
              (set! accel-read-min (map min accel-read-min accel))
              (set! mag-read (vector+ mag-read mag))
              (set! mag-read-max (map max mag-read-max mag))
              (set! mag-read-min (map min mag-read-min mag))
              (set! sensor-read-count (+ 1 sensor-read-count)))))))

  (let ((save-counter 0))
  (create-periodic-task
   "magnetometer-calibration" 3
   (lambda ()
     (if (> sensor-read-count 50)
         (let ((accel-avg (vector-scale (/ sensor-read-count) accel-read))
               (mag-avg (vector-scale (/ sensor-read-count) mag-read)))
           (let ((accel-dev (magnitude (vector- accel-read-max accel-read-min)))
                 (mag-dev (magnitude (vector- mag-read-max mag-read-min))))
             (print "accel-dev " accel-dev " mag-dev " mag-dev)
             (cond ((and (< accel-dev .02) (< mag-dev .01))
                    (print "update-cal " accel-avg " " mag-avg)
                    (update-calibration-measurements accel-avg mag-avg options)))
             (set! accel-read '(0 0 0))
             (set! accel-read-max default-accel-read-max)
             (set! accel-read-min default-accel-read-min)
             (set! mag-read '(0 0 0))
             (set! mag-read-max default-mag-read-max)
             (set! mag-read-min default-mag-read-min)
             (set! sensor-read-count 0))))

     (let ((accel (map sensor-query (map (lambda (n) (list 'accel n)) '(0 1 2))))
           (mag (map sensor-query (map (lambda (n) (list 'mag n)) '(0 1 2)))))

       (cond ((>= (length calibration-measurements) 10)
              
              (let ((accel-cal (calibrate-sensor-3d (map second
                                                         calibration-measurements))))
                (verbose "accel-sensor-cal "  (map-round-to-places accel-cal 4)))
              (let ((accel-cal (calibrate-sensor-3rd-order-3d (map second
                                                                   calibration-measurements))))
                (verbose "accel-sensor-3rd-order-cal "  (map-round-to-places accel-cal 4)))
              
              (set! accel-mag-3d-calibration (calibrate-accel-mag-3d (map (lambda (m)
                                                                            (append (second m) (third m)))
                                                                          calibration-measurements)))

              (if (and accel mag (not (any not accel)) (not (any not mag)))
                  (verbose "pitch " (round-to-places (computation-calculate 'pitch) 2)
                           " roll " (round-to-places (computation-calculate 'roll) 2)
                           " heading " (round-to-places (computation-calculate 'magnetic-heading) 2)
                           " dip " (round-to-places (computation-calculate 'magnetic-inclination) 2)))

           (let-values (((a b c d e f g h i j k l m n o p q r s t u v)
                         (apply values (first accel-mag-3d-calibration))))
             (verbose "c-calaccel: " (map-round-to-places `(,a ,b ,c ,d ,e ,f ,g ,h ,i) 3))
             (verbose "c-calmag  : " (map-round-to-places `(,j ,k ,l ,m ,n ,o ,p ,q ,r ,s ,t ,u) 3))
             (verbose "c-caldip  : " (round-to-places (rad2deg (asin v)) 3)
                    " err " (round-to-places (second accel-mag-3d-calibration) 3)))
           (if (and accel mag (not (any not accel)) (not (any not mag)))
               (let ((d (apply-accel-mag-3d accel mag accel-mag-3d-calibration)))
                 (verbose "caccel " (map-round-to-places (first d) 2)
                          " cm " (round-to-places (magnitude (first d)) 2)
                          " cmag " (map-round-to-places (second d) 2)
                          " cm " (round-to-places (magnitude (second d)) 2))
                 (let ((pitch (deg2rad (accelerometer-pitch (first d))))
                       (roll  (deg2rad (accelerometer-roll (first d)))))
                   (let ((mx (first (second d)))
                         (my (second (second d)))
                         (mz (third (second d))))
                        (let ((xh (+ (* mx (cos pitch))
                                     (* mz (sin pitch))))
                              (yh (+ (* mx (sin roll) (sin pitch))
                                     (* my (cos roll))
                                     (* -1 mz (sin roll) (cos pitch)))))
                          (let ((yaw (atan yh xh)))
                 (verbose "cpitch " (round-to-places (accelerometer-pitch (first d)) 2)
                        " croll " (round-to-places (accelerometer-roll (first d)) 2)
                        " cyaw "  (round-to-places (apply magnetometer-heading d) 2)
                        " new-yaw " (round-to-places (rad2deg yaw) 2)
                        " cinc "  (round-to-places (apply magnetometer-inclination d) 2))))) 
                  ))))))

       (cond ((>= save-counter 100)
              (save-calibration (options 'calibration-file))
              (set! save-counter 0))
             (else
              (set! save-counter (+ save-counter 1))))
       ))))

(computation-register 'pitch "The pitch derived from the accelerometer" '(accel)
                      (lambda ()
                        (if (not accel-mag-3d-calibration)
                            #f
                            (let*((accel (sensor-query-indexes 'accel '(0 1 2)))
                                  (cal-accel (first (apply-accel-mag-3d accel '(0 0 0)
                                                                        accel-mag-3d-calibration))))
                              (accelerometer-pitch cal-accel)))))

(computation-register 'roll "The roll derived from the accelerometer" '(accel)
                      (lambda ()
                        (if (not accel-mag-3d-calibration)
                            #f
                            (let*((accel (sensor-query-indexes 'accel '(0 1 2)))
                                  (cal-accel (first (apply-accel-mag-3d accel '(0 0 0)
                                                                        accel-mag-3d-calibration))))
                                (accelerometer-roll cal-accel)))))

(computation-register 'magnetic-heading "The heading derived from the magnetometer" '(mag accel)
                      (lambda ()
                          (let ((accel (sensor-query-indexes 'accel '(0 1 2)))
                                (mag (sensor-query-indexes 'mag '(0 1 2))))
                            (if (not accel-mag-3d-calibration)
                                #f
                                (let ((cal (apply-accel-mag-3d accel mag accel-mag-3d-calibration)))
                                  (magnetometer-heading (first cal) (second cal)))))))

(computation-register 'magnetic-inclination "The inclination derived from the magnetometer" '(mag accel)
                      (lambda ()
                          (let ((accel (sensor-query-indexes 'accel '(0 1 2)))
                                (mag (sensor-query-indexes 'mag '(0 1 2))))
                            (if (not accel-mag-3d-calibration)
                                #f
                                (let ((cal (apply-accel-mag-3d accel mag accel-mag-3d-calibration)))
                                  (magnetometer-inclination (first cal) (second cal)))))))

(computation-register
 'heading "The true heading derived from the magnetometer and declination, if no magnetometer is specified, gps heading is used" '(gps)
 (lambda ()
    (if (sensor-contains? 'mag)
        (- (computation-calculate 'magnetic-heading)
           (computation-calculate 'declination))
        (begin (warning-once "using gps heading for heading, "
                             "this may be very inaccurate.")
               (computation-calculate 'gps-heading)))))

(computation-register 'acceleration-magnitude "The magnitude of force" '(accel)
                      (lambda () (magnitude (sensor-query-indexes 'accel '(0 1 2)))))

(computation-register 'magnetic-magnitude "The magnitude of the field" '(mag)
                      (lambda () (magnitude (sensor-query-indexes 'mag '(0 1 2)))))

; rotate vec by whatever rotation is needed to make align axis
(define (align-axis vec align axis)
  (apply-quaternion-to-vector (vector-vector->quaternion align axis) vec))

; give mag coordinates so we can calculate yaw with atan on x and y
(define (rotate-out-mag accel mag)
    (let ((v1 (normalize accel))
          (v2 (normalize `(0 ,(second accel) ,(third accel)))))
      (align-axis (align-axis mag v1 v2) v2 '(0 0 1))))

(define (accelerometer-pitch accel)
  (rad2deg (- (atan (first accel)
                          (sqrt (+ (square (second accel))
                                   (square (third accel))))))))

; compute the roll angle from accel reading
(define (accelerometer-roll accel)
  (rad2deg (atan (second accel) (third accel))))

; given vectors for accel and mag, determine yaw direction of x axis
(define (magnetometer-heading accel mag)
  (let ((rmag (rotate-out-mag accel mag)))
    (rad2deg (phase-resolve-positive (atan (second rmag) (first rmag))))))

; given vectors for accel and mag, determine angle between them
(define (magnetometer-inclination accel mag)
  (rad2deg (- (/ pi 2) (phase-resolve (acos (dot (normalize accel)
                                                 (normalize mag)))))))

; we know boat moves with a time constant.  try to lock on to this for
; each sensor input so we can cancel it
; 
; x = bias + amplitude*sin(2*Pi*t*frequency + phase)
;
; states: bias amplitude period phase
; measurements: x t
(define (calibrate-sensor-boat-motion measurements)
  (let ((cal
         (least-squares-iterate
          `(,(apply average (map first measurements)) 1 .25 0)
          (lambda (state measurement)
            (let-values (((bias amplitude frequency phase) (apply values state))
                         ((x t) (apply values measurement)))
              (let ((a (+ (* (* 2 Pi t) frequency) phase)))
                (let ((val
                       (list (list 1
                                   (sin a)
                                   (* 2 Pi amplitude t (cos a))
                                   (* amplitude (cos a)))
                             (- x (+ bias (* amplitude (sin a)))))))
                  (print "val " val " state " state " measurement " measurement)
                  val))))
          measurements
          (lambda (state update)
            (< update 1e-3))
          2)))
    (let ((state (first cal)) (update (second cal)))
      (if (< update 1)
          (let-values (((bias amplitude frequency phase) (apply values state)))
            (list bias (abs amplitude) frequency
                  (phase-resolve (if (negative? amplitude) (+ phase Pi) phase))))
          #f))))


(define (test-calibration-constraints value constraints)
  (cond ((null? value) #t)
        ((not (first value)) #t)
        ((< (first value) (first (first constraints))) #f)
        ((> (first value) (second (first constraints))) #f)
        (else (test-calibration-constraints (cdr value) (cdr constraints)))))

; a completely calibrated 3 axis sensor has states for:
;
; xb yb zb (biases) s (scale to 1 earth field) yrs zrs (relative scales for y and z)
; xyc xzc yzc (cross coupling coefficients alignment of axes etc..)
;
; when states are set to #f then not enough data to determine them exists

(define (apply-accelerometer-calibration calibration measurement)
  (if (and calibration (not (any not measurement)))
      (let-values (((xb yb zb s) (apply values (first calibration)))
                   ((x y z) (apply values measurement)))
        (vector-scale (/ s) (vector- measurement (first calibration))))
      #f))

(define (compute-accelerometer-calibration measurements)
  (let ((cal (calibrate-biases-and-scale-3d measurements)))
    (if (test-calibration-constraints
         (first cal) '((-2 2) (-2 2) (-2 2) (.2 2)))
        cal #f)))

(define (apply-magnetometer-calibration calibration measurement)
  (if (and calibration (not (any not measurement)))
      (let-values (((xb yb zb s yrs zrs xyc xzc yzc) (apply values (first calibration)))
                   ((x y z) (apply values measurement)))
        (vector-scale (/ s)
                      (let-values (((xs ys zs) (apply values (vector- measurement (first calibration)))))
                        (cond ((not yrs) `(,xs ,ys ,zs))
                              ((not xyc) `(,xs
                                           ,(* yrs ys)
                                           ,(* zrs zs)))
                              (else `(,xs
                                      ,(* yrs (+ ys (* xyc xs)))
                                      ,(* zrs (+ zs (* xzc xs) (* yzc ys)))))))))
        #f))

(define (compute-magnetometer-calibration measurements)
  (let ((cal
         (let ((cal1 (calibrate-biases-and-scale-3d measurements))
               (cal2 (calibrate-biases-scale-and-relative-scales-3d measurements))
               (cal3 (calibrate-biases-scale-relative-scales-and-cross-coupling-3d measurements))
               (really-big 1e100))
           (let ((c1d (if cal1 (second cal1) really-big))
                 (c2d (if cal2 (second cal2) really-big))
                 (c3d (if cal3 (second cal3) really-big)))
             (let ((mc (min c1d c2d c3d)))
               (cond ((= really-big mc) #f)
                     ((= c1d mc) (list (append (first cal1) (make-list 5)) (second cal1)))
                     ((= c2d mc) (list (append (first cal2) (make-list 3)) (second cal2)))
                     ((= c3d mc) (list (append (first cal3) (make-list 0)) (second cal3)))
                     (else (error "this should not happen"))))))))
    (if (test-calibration-constraints
         (first cal) '((-2 2) (-2 2) (-2 2) (.2 2) (.2 2) (.2 2) (-.8 .8) (-.8 .8) (-.8 .8)))
        cal #f)))


; magnetometer alignment calibration...
;
; dip (magnetic declination)
; rvx rvy rvz (rotation vector, rotate magnetometer coordinates around this by its magnitude in radians)
