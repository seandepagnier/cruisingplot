;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
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

(define accelerometer-calibration #f)
(define magnetometer-calibration #f)
(define calibration-measurements '())

(define (update-calibration-measurements accel mag options)
    (define (cdist m1 m2)
      (let ((accel1 `(0 ,(second m1) ,(third m1)))
            (accel2 `(0 ,(second m2) ,(third m2)))
            (mag1 `(,(fourth m1) ,(fifth m1) ,(sixth m1)))
            (mag2 `(,(fourth m2) ,(fifth m2) ,(sixth m2))))
        (let ((adist (- 1 (vector-dot (normalize accel1) (normalize accel2))))
              (mdist (- 1 (vector-dot (normalize mag1) (normalize mag2)))))
          (* adist mdist))))

    (define (sumcdist m1)
      (let each-m2 ((m2 calibration-measurements))
        (if (null? m2) 0
            (+ (each-m2 (cdr m2)) (cdist m1 (car m2))))))

    (define (leastm)
      (let each-m1 ((m1 calibration-measurements)
                    (leastd #f)
                    (leastm #f))
        (if (null? m1) leastm
            (let ((d (sumcdist (car m1))))
              (if (or (not leastd) (< d leastd))
                  (each-m1 (cdr m1) d m1)
                  (each-m1 (cdr m1) leastd leastm))))))

  (let ((nmea (append accel mag)))
    (cond ((any not nmea) #f)
          ((< (length calibration-measurements) (options 'max-calibration-points))
           (set! calibration-measurements (cons nmea calibration-measurements))
           #t)
          (else
           (let ((least-m (leastm)))
             (cond ((> (sumcdist nmea) (sumcdist (car least-m)))
                  (set-car! least-m nmea)
                  #t)
                   (else #f)))))))
                      
(define (magnetometer-setup arg)
  (define options
    (create-options
     `(,(make-number-verifier 'max-calibration-points "number of calibration points to use" 24 0 1000)
       ,(make-string-verifier 'calibration-file
                              "file to use for saving and loading calibration between runs" "magcal"))

     "currently the magnetometer supports the first 3 axes of accelerometero and magnetometer sensors"
     #f))

  (parse-basic-options-string options arg)

  (create-periodic-task
   "magnetometer-calibration" 1
   (lambda ()
     (let ((accel (map sensor-query (map (lambda (n) (list 'accel n)) '(0 1 2))))
           (mag (map sensor-query (map (lambda (n) (list 'mag n)) '(0 1 2)))))
       (cond ((update-calibration-measurements accel mag options)
              (set! accelerometer-calibration (compute-accelerometer-calibration
                                               (map (lambda (m) (list (second m) (third m)))
                                                    calibration-measurements)))
              (set! magnetometer-calibration (compute-magnetometer-calibration
                                               (map (lambda (m) (list (fourth m) (fifth m) (sixth m)))
                                                    calibration-measurements)))))

;      (verbose "accel " (apply-accelerometer-calibration accelerometer-calibration accel)
 ;              " cal " accelerometer-calibration)
  ;    (verbose "mag " (apply-magnetometer-calibration magnetometer-calibration mag)
   ;            " cal " magnetometer-calibration)
;      (verbose "heading " (computation-calculate 'magnetic-heading))

       ))))


(computation-register 'magnetic-heading "The heading derived from the magnetometer" '(mag accel)
                      (lambda ()
                          (let ((accel (sensor-query-indexes 'accel '(0 1 2)))
                                (mag (sensor-query-indexes 'mag '(0 1 2))))
                            (if (or (any not accel) (any not mag))
                                #f
                                (magnetometer-heading accel mag)))))

(computation-register
 'heading "The true heading derived from the magnetometer and declination, if no magnetometer is specified, gps heading is used" '(gps)
 (lambda ()
    (if (sensor-contains? 'mag)
        (- (computation-calculate 'magnetic-heading)
           (computation-calculate 'declination))
        (begin (warning-once "using gps heading for heading, "
                             "this may be very inaccurate.")
               (computation-calculate 'gps-heading)))))

(computation-register 'magnetic-magnitude "The magnitude of the field" '(magnetometer)
                      (lambda ()
                        0))

; rotate vec by whatever rotation is needed to make align axis
(define (align-axis vec align axis)
  (apply-quaternion-to-vector (vector-vector->quaternion align axis) vec))

; give mag coordinates so we can calculate yaw with atan on x and y
(define (rotate-out-mag accel mag)
    (let ((v1 (normalize accel))
          (v2 (normalize `(0 ,(second accel) ,(third accel)))))
      (align-axis (align-axis mag v1 v2) v2 '(0 0 1))))

; given raw vectors for accel and mag, determine yaw direction of x axis
(define (magnetometer-heading accel mag)
  (let ((cal-accel (apply-accelerometer-calibration accelerometer-calibration accel))
        (cal-mag (apply-magnetometer-calibration magnetometer-calibration mag)))
    (if (and cal-accel cal-mag)
        (let ((rmag (rotate-out-mag accel mag)))
          (rad2deg (phase-resolve-positive (atan (second rmag) (first rmag)))))
        #f)))

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

;
; this magnetometer learns calibration automatically to provide heading from
; raw magnetometer and accelerometer measurements
; m = s*(r-b)
;  2
; m = 1
;   2                                2          2
; mx  + (cos(heel)*my + sin(heel)*mz) = cos(inc)
;
; heading = atan2(mx, cos(heel)*my + sin(heel)*mz)

; states: ayb azb mxb myb mzb dip
; measurement: ay az mx my mz
;
; when very little is known, we use equation without 
; simplest possible equations to avoid false convergance
;
; set initial biases to  to average of ay coords, dip can be zero (or if we have gps and mag model...)
;       ((ay-ayb)*(my-myb) + (az-azb)*(mz-mzb))^2 = ((ay-ayb)^2 + (az-azb)^2) * ((mx-mxb) + (my-myb)^2 + (mz-mzb)^2) * dip
;


; this algorithm computes the dip state (cosine of magnetic declination) and
; the sensor biases from a list of raw measurements.  The magnitude
; of the measurements can be arbitrary but it is assumed scale factor
; is 1:1 for each axis, (not computing relative scale factors)

(define (calibrate-dip-and-biases measurements n)
  (least-squares-iterate
   '(0 0 0 0 0 .01)
   (lambda (state measurement)
     (let-values (((ayb azb mxb myb mzb dip) (apply values state))
                  ((ay az mx my mz) (apply values measurement)))
       (let ((ays (- ay ayb)) (azs (- az azb))
             (mxs (- mx mxb)) (mys (- my myb)) (mzs (- mz mzb)))
         (let ((dot (+ (* ays mys) (* azs mzs)))
               (as (+ (square ays) (square azs)))
               (ms (+ (square mxs) (square mys) (square mzs))))
       (list (list (* 2 (- (* ays ms dip) (* mys dot)))
                   (* 2 (- (* azs ms dip) (* mzs dot)))
                   (* 2 as dip mxs)
                   (* 2 (- (* mys as dip) (* ays dot)))
                   (* 2 (- (* mzs as dip) (* azs dot)))
                   (- (* as ms)))
             (- (* as ms dip) (* dot dot)))))))
   measurements n))


; this is just using y and z axis of accel assuming we are flat on x
;
; yb zb (biases) s (scale to 1 earth field) zrs (relative scales z)
;
; when states are set to #f then not enough data to determine them exists
;

(define (apply-accelerometer-calibration calibration measurement)
  (if (and calibration (not (any not measurement)))
      (let-values (((yb zb s zrs) (apply values calibration))
                   ((x y z) (apply values measurement)))
        (vector-scale (/ s) 
                      (let ((biased-calibration `(0 ,(- y yb) ,(- z zb))))
                        (cond ((not zrs) biased-calibration)
                              (else `(0 ,(second biased-calibration)
                                        ,(* (third biased-calibration) zrs)))))))
        #f))


(define (compute-accelerometer-calibration measurements)
      (let ((cal1 (calibrate-biases-and-scale-2d measurements))
            (cal2 (calibrate-biases-scale-and-relative-scale-2d measurements)))
        (cond ((and cal1 cal2) (if (< (second cal1) (second cal2))
                                   (append (first cal1) (make-list 1))
                                   (append (first cal2) (make-list 0))))
              (cal1 (append (first cal1) (make-list 9)))
              (cal2 (error "computed accelerometer relative scale cal but not basic"))
              (else #f))))


; a completely calibrated 3 axis sensor has states for:
;
; xb yb zb (biases) s (scale to 1 earth field) yrs zrs (relative scales for y and z)
; xyc xzc yzc (cross coupling coefficients alignment of axes etc..)
;
; when states are set to #f then not enough data to determine them exists
;

(define (apply-magnetometer-calibration calibration measurement)
  (if (and calibration (not (any not measurement)))
      (let-values (((xb yb zb s yrs zrs xyc xzc yzc) (apply values calibration))
                   ((x y z) (apply values measurement)))
        (vector-scale (/ s) 
                      (let ((biased-calibration (vector- measurement calibration)))
                        (cond ((not yrs) biased-calibration)
                              ((not xyc) `(,(first biased-calibration)
                                                  ,(* (second biased-calibration) yrs)
                                                  ,(* (third biased-calibration) zrs)))
                              (else 'unimplemented)))))
        #f))

(define (compute-magnetometer-calibration measurements)
;    (let ((declination (computation-calculate 'magnetic-declination)))
      (let ((cal1 (calibrate-biases-and-scale-3d measurements))
            (cal2 (calibrate-biases-scale-and-relative-scales-3d measurements)))
        (cond ((and cal1 cal2) (if (< (second cal1) (second cal2))
                                   (append (first cal1) (make-list 5))
                                   (append (first cal2) (make-list 3))))
              (cal1 (append (first cal1) (make-list 9)))
              (cal2 (error "computed magnetometer relative scale cal but not basic"))
              (else #f))))

; magnetometer alignment calibration...
;
; dip (magnetic declination)
; rvx rvy rvz (rotation vector, rotate magnetometer coordinates around this by its magnitude in radians)
