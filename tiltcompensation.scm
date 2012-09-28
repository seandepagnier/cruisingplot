;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;;  Read from intertial sensors (accelerometer and gyroscope..) and command
;;  electric motors to counteract this motion

(declare (unit tiltcompensation))
(declare (uses algebra matrix plot utilities))

(use gl glu)

(include "glshortcuts.scm")

(define tilt-history '())

(define (update-history)
  (let ((cur-time (computation-calculate 'time))
        (value (computation-calculate 'accel.5)))
    (if value
        (set! tilt-history
              (cons (list cur-time value)
                    (let new-history ((tilt-history tilt-history))
                      (if (or (null? tilt-history)
                              (< (caar tilt-history) (- cur-time 4)))
                          '()
                          (cons (car tilt-history) (new-history (cdr tilt-history))))))))))

(define (compute-amplitude-period-phase-bias bail)
  (define noise-level 8)
  (define min-amplitude (* noise-level 2))
  (if (null? tilt-history) (bail #f))
  (let ((avg (apply average (map cadr tilt-history))))
    (define (last-history-cross tilt-history bail)
      (let loop ((tilt-history tilt-history)
                 (cross #f))
        (cond ((null? tilt-history)
               (verbose "no rolling detected")
               (bail #f))
              ((< (abs (- (cadar tilt-history) avg)) noise-level)
               (loop (cdr tilt-history) #t))
              ((and cross
                    (> (abs (- (cadar tilt-history) avg)) noise-level))
               tilt-history)
              (else (loop (cdr tilt-history) #f)))))
    
    (define (sub-history tilt-history stop)
      (if (eq? tilt-history stop) '()
          (cons (car tilt-history) (sub-history (cdr tilt-history) stop))))
    
    (let* ((a (last-history-cross tilt-history bail))
           (b (last-history-cross a bail))
           (c (last-history-cross b bail)))
      (let* ((period (abs (- (caar a) (caar c))))
             (amplitude
              (let ((values (map cadr (sub-history a c))))
                (/ (- (apply max values)
                      (apply min values)) 2)))
             (phase (/ (remainder
                        (- (caar tilt-history)
                           (if (> (cadar a) avg) (caar a) (caar b)))
                        period) period)))
        (verbose "amplitude: " amplitude)
        (verbose "period: " period)
        (verbose "phase: " phase)
        (matrix `((,amplitude) (,period) (,phase) (,avg)))))))

;  Truth equation
;  measurement = amplitude*sin( 2*Pi*time / period + phase) + bias

; Kalman States:  amplitude, period, phase, bias
; measurement
; Z = X[0]*sin(2*Pi * time / X[1] + X[2]) + X[3];
; partials:
; dZ/dX[0] = sin(2*Pi*time/X[1]+X[2])
; dZ/dX[1] = -2*X[0]*Pi*time*cos(2*Pi*time/X[1]+X[2])/X[1]^2;
; dZ/dX[2] = X[0]*cos(2*Pi*time/X[1]+X[2])
; dZ/dX[3] = 1;
(define (build-tilt-jacobian-residual-row state measurement)
  (let ((time (first measurement))
        (value (second measurement)))
    (let ((X0 (first state)) (X1 (second state))
          (X2 (third state)) (X3 (fourth state)))
      `((,(sin (+ (* 2 Pi time (/ X1)) X2))
         ,(* -2 Pi X0 time (cos (+ (* 2 Pi time (/ X1)) X2)) (/ X1) (/ X1))
         ,(* X0 (cos (+ (* 2 Pi time (/ X1)) X2)))
         ,1)
        ,(- value (* X0 (sin (+ (* 2 Pi time (/ X1)) X2))) X3)))))

; States:  frequency, phase, bias
; measurement
; Z = sin(2*Pi * time * X[0] + X[1]) + X[2];
; partials:
; dZ/dX[0] = 2*Pi*time*cos(2*X[0]*Pi*time+X[1])
; dZ/dX[1] = cos(2*Pi*time*X[0]+X[1])
; dZ/dX[2] = 1;
(define (build-fpb-jacobian-residual-row state measurement)
  (let ((time (first measurement))
        (value (second measurement)))
    (let ((X0 (first state)) (X1 (second state)) (X2 (third state)))
      `((,(* 2 Pi time (cos (+ (* 2 Pi time X0) X1)))
         ,(* (cos (+ (* 2 Pi time X0) X1)))
         ,1)
        ,(- value (* (sin (+ (* 2 Pi time X0) X1))) X2)))))


; States:  frequency, phase
; Measurement:  Z = amplitude*sin(2*Pi * time * X[0] + X[1]) +bias
; partials:
; dZ/dX[0] = amplitude*2*Pi*time*cos(2*X[0]*Pi*time+X[1])
; dZ/dX[1] = amplitude*cos(2*Pi*time*X[0]+X[1])
(define (compute-frequency-phase start amplitude bias data)
  (least-squares-iterate start
  (lambda (state measurement)
    (let ((time (first measurement))
          (value (second measurement)))
      (let ((X0 (first state)) (X1 (second state)))
        `((,(* amplitude 2 Pi time (cos (+ (* 2 Pi time X0) X1)))
           ,(* amplitude (cos (+ (* 2 Pi time X0) X1))))
          ,(- value (* amplitude (sin (+ (* 2 Pi time X0) X1))) bias)))))
        data #f 10))

; States:  amplitude, frequency, phase, bias
; Measurement: Z = X[0]*sin(2*Pi * time * X[1] + X[2]) + X[3];
; partials:
; dZ/dX[0] = sin(2*Pi*time*X[1]+X[2])
; dZ/dX[1] = 2*X[0]*Pi*time*cos(2*X[1]*Pi*time+X[2])
; dZ/dX[2] = X[0]*cos(2*Pi*time*X[1]+X[2])
; dZ/dX[3] = 1;
(define (compute-amplitude-frequency-phase-bias start data)
  (least-squares-iterate start
  (lambda (state measurement)
    (let ((time (first measurement))
          (value (second measurement)))
      (let ((X0 (first state)) (X1 (second state))
            (X2 (third state)) (X3 (fourth state)))
        `((,(sin (+ (* 2 Pi time X1) X2))
           ,(* 2 Pi X0 time (cos (+ (* 2 Pi time X1) X2)))
           ,(* X0 (cos (+ (* 2 Pi time X1) X2)))
           ,1)
          ,(- value (* X0 (sin (+ (* 2 Pi time X1) X2))) X3)))))
  data #f 10))

(define (eor x y)
  (cond ((and x y) #f)
        ((or x y) #t)
        (else #f)))

; from data and given frequency lock on
(define (calculate-wave data start-frequency)
    (let* ((rawdata (map second data))
           (minval (apply min rawdata))
           (maxval (apply max rawdata)))
      (let ((amplitude (/ (- maxval minval) 2))
            (bias (apply average rawdata)))
        (let*((frequency-phase-run (compute-frequency-phase
                                    `(,start-frequency 0) amplitude bias data))
              (frequency-phase (first frequency-phase-run)))
          (let*((amplitude-frequency-phase-bias-run
                 (compute-amplitude-frequency-phase-bias
                  `(,amplitude
                    ,(first frequency-phase)
                    ,(second frequency-phase)
                    ,bias) data))
                (amplitude (first (first amplitude-frequency-phase-bias-run)))
                (frequency (second (first amplitude-frequency-phase-bias-run)))
                (phase (third (first amplitude-frequency-phase-bias-run)))
                (bias (fourth (first amplitude-frequency-phase-bias-run)))
                (error (second amplitude-frequency-phase-bias-run)))
            (list (list (abs amplitude) (abs frequency) (if (eor (negative? amplitude) (negative? frequency))
                                                            (+ pi phase) phase) bias)
                  error))))))

; find wave from data using least squares
(define (find-wave data min-period max-period)
  (let each-wave ((best-wave #f)
                  (period min-period))
    (if (< period max-period)
        (each-wave
         (let ((wave (calculate-wave data (/ period))))
           (let ((period (/ (second (first wave)))))
             (if  (and (or (not best-wave)
                           (< (second wave) (second best-wave)))
;                       (> period min-period)
;                       (< period max-period)
                       )
                 wave best-wave)))
         (* period 2))
        (if best-wave
        (let ((amplitude (first (first best-wave)))
              (frequency (second (first best-wave)))
              (phase (third (first best-wave)))
              (bias (fourth (first best-wave)))
              (error (second best-wave)))
          `((,amplitude ,(/ frequency) ,(phase-resolve phase) ,bias) ,error))
        #f))))


(define (tilt-complete? state update)
      ; force phase to be from -Pi to Pi
      (set-car! (cddr state) (phase-resolve (third state)))

      ; force amplitude to be positive by flipping period and phase
      (cond ((negative? (first state))
             (set-car! state (- (first state)))
             (set-cdr! state `(,(- (second state))
                               ,(- (third state))
                               ,(fourth state)))))

      (< update 1e-4))

; don't bother with amplitude first, just find frequency
(define (compute-frequency-phase-bias-least-squares data start-frequency)
  (let* ((rawdata (map second data))
         (minval (apply min rawdata))
         (maxval (apply max rawdata))
         (avgval (apply average rawdata)))
    (least-squares-iterate `(,start-frequency 0 ,avgval)
                           build-tilt-jacobian-residual-row
                           (map (lambda (d) (/ d (- maxval minval) .5) data))
                           #f 10)))

;  Truth equation
;  measurement = amplitude*sin( 2*Pi*time / period + phase) + bias
;
; amplitude = sqrt(a^2 + b^2)
; phase = atan(a, b)
; measurement = a*sin( 2*Pi*frequency*time) + b*cos( 2*Pi*frequency*time) + bias
;
; states are: a, b, frequency, bias
;
; dZ/dX[0] = sin( 2*Pi*X[2]*time)
; dZ/dX[1] = cos( 2*Pi*X[2]*time)
; dZ/dX[2] = 2*X[0]*Pi*time*cos(2*X[2]*Pi*time)-2*X[1]*Pi*time*sin(2*X[2]*Pi*time)
; dZ/dX[3] = 1;
(define (build-polar-tilt-jacobian-row state measurements)
    (let ((X0 (matrix-ref state 0 0)) (X1 (matrix-ref state 1 0))
          (X2 (matrix-ref state 2 0)) (X3 (matrix-ref state 3 0))
          (time (first measurements)))
      (list (sin (* 2 Pi time X2))
            (cos (* 2 Pi time X2))
            (* 2 Pi time (- (* X0 (cos (* 2 Pi X2 time))) (* X1 (sin (* 2 Pi X2 time)))))
            1)))

(define (build-polar-tilt-residual-row state measurements)
    (let ((X0 (matrix-ref X 0 0)) (X1 (matrix-ref X 1 0))
          (X2 (matrix-ref X 2 0)) (X3 (matrix-ref X 3 0))
          (time (first measurements)) (value (second measurements)))
      (list (- value (* X0 (sin (+ (* 2 Pi time (/ X1)) X2))) X3))))

(define (command-relays phase-percent duty-cycle)
  (cond ((> phase-percent duty-cycle)
         (relay-set 1 #t))
        ((> phase-percent 0)
         (relay-set 2 #t))
        ((> (+ phase-percent 1) duty-cycle)
         (relay-set 1 #f))
        (else
         (relay-set 2 #f))))

(define (command-relays-off)
    (relay-set 1 #f)
    (relay-set 2 #f))

(define (run-primitive-tilt-compensation)
  (call/cc (lambda (bail) (compute-amplitude-period-phase-bias bail))))


(define (run-tilt-compensation)
         (call/cc (lambda (bail)
                    (with-exception-handler
                     (lambda _ (bail #f))
                     (lambda () (compute-amplitude-period-phase-bias-least-squares))))))

(define tilt-comp-last #f)

(computation-calculate 'time)

(define min-amplitude 6)
(define relay-phase-shift (* Pi 2))

(define (tilt-compensation-setup)
  (start-periodic-task
   "tilt compensation task" .15
   (lambda ()
     (update-history)
     
     (let ((A (run-tilt-compensation)))
       (print "A: " A)
       (cond (A (set! tilt-comp-last A))
             (tilt-comp-last
              ; if we failed to converge, decay amplitude
              (matrix-set! tilt-comp-last 0 0 (* (matrix-ref tilt-comp-last 0 0) .99)))))

     (print "tilt-comp-last: " tilt-comp-last)

     (if (and (not (null? relay-ports)) tilt-comp-last)
         (let ((amplitude (matrix-ref tilt-comp-last 0 0))
               (period (matrix-ref tilt-comp-last 1 0))
               (phase (matrix-ref tilt-comp-last 2 0))
               (time (computation-calculate 'time)))
           (let ((cur-phase (+ (/ (* 2 Pi time) period) phase relay-phase-shift)))
             (let ((phase-percent (/ (phase-resolve cur-phase) Pi)))
               (if (> amplitude min-amplitude)
                   (command-relays phase-percent .25)
                   (command-relays-off)))))))
  
  'tilt-compensation-update))
   
(use numbers srfi-1)
(define Pi (* 2 (asin 1)))
(define (sequence from to . step)
  (if (> from to) '() (cons from (apply sequence (+ from (if (null? step) 1 (car step))) to step))))

(define i (sqrt -1))

(define (discrete-fourier-transform data)
  (let ((N (length data)))
    (map (lambda (k)
           (fold + 0 (map (lambda (n)
                           (* (list-ref data n) (exp (/ (- (* 2 Pi i k n)) N))))
                         (sequence 0 (- N 1)))))
         (sequence 0 (- N 1)))))


(define (discrete-fourier-transform-sub k data)
  (let ((N (length data)))
    (fold + 0 (map (lambda (n)
                     (* (list-ref data n) (exp (/ (- (* 2 Pi i k n)) N))))
                   (sequence 0 (- N 1))))))

(define (inverse-discrete-fourier-transform data)
  (let ((N (length data)))
    (map (lambda (k)
           (/
            (fold + 0 (map (lambda (n)
                            (* (list-ref data n) (exp (/ (* 2 Pi i k n) N))))
                          (sequence 0 (- N 1))))
            N))
         (sequence 0 (- N 1)))))

(define (linear-interpolate time h1 h2)
  (let ((t1 (car h1)) (v1 (cadr h1))
        (t2 (car h2)) (v2 (cadr h2)))
      (+ v2 (* (/ (- time t2) (- t1 t2)) (- v1 v2)))))

; take history as a list of (time value) lists and
; use simple linear interpolation between pairs to
; build a discrete history (with equal time spacing)
(define (build-discrete-history history time-step)
  (if (or (null? history) (zero? time-step)) '()
      (let*((start (first history))
            (end (last history))
            (start-time (car start))
            (end-time (car end))
            (overall-time (- start-time end-time)))
        (let each-step ((time start-time)
                        (history history))
          (let ((next-history (cdr history)))
            (cond ((or (null? next-history) (> time end-time)) '())
                  ((> time (caar next-history)) (each-step time next-history))
                  (else (cons (linear-interpolate time (car history) (car next-history))
                              (each-step (+ time time-step) history)))))))))

(define (history-frequency-amplitude-phase history)
  (let ((max-frequency 20))
    (let*((discrete-history (build-discrete-history history (/ max-frequency)))
          (dft (discrete-fourier-transform discrete-history))
          (N (length dft))
          (frequency-step (/ max-frequency (- N 1))))
       (map (lambda (dft-value frequency)
              (list frequency
                    (* (if (zero? frequency) 1 2) (/ (complex-amplitude dft-value) N))
                    (+ (complex-phase dft-value) (/ Pi 2))))
            dft (sequence (exact->inexact 0) max-frequency frequency-step)))))

; get rid of frequencies > 1hz
(define (remove-high-frequencies history-frequency-amplitude-phase)
  (cond ((null? history-frequency-amplitude-phase) '())
        ((< (first (car history-frequency-amplitude-phase)) 1)
         (cons (car history-frequency-amplitude-phase)
               (remove-high-frequencies (cdr history-frequency-amplitude-phase))))
        (else '())))

; print estimated frequency, period, amplitude, and bias of signal
(define (history-frequency-short-info history-frequency-amplitude-phase)
  (let*((bias (cadar history-frequency-amplitude-phase))
        (frequency (/ (fold + 0 (map (lambda (frequency-amplitude-phase)
                                       (* (first frequency-amplitude-phase)
                                          (second frequency-amplitude-phase)))
                                     (cdr history-frequency-amplitude-phase)))
                      (fold + 0 (map second (cdr history-frequency-amplitude-phase)))))
        (period (/ frequency))
        (amplitude (sqrt (fold + 0 (map (lambda (frequency-amplitude-phase)
                                          (square (second frequency-amplitude-phase)))
                                        (cdr history-frequency-amplitude-phase))))))
    (print "frequency " frequency " period " period " amplitude " amplitude " bias " bias)))


; print information about frequency, period, amplitude, and phase of history data
(define (history-frequency-long-info history-frequency-amplitude-phase)
  (for-each (lambda (history-value)
              (let ((frequency (first history-value))
                    (amplitude (second history-value))
                    (phase (third history-value)))
                (let ((period (if (zero? frequency) "DC" (/ (round (* 100 (/ frequency))) 100))))
                      (if (and (> amplitude 1e-5))
                          (print "frequency " frequency " period " period
                                 " amplitude " amplitude " phase " (if (zero? frequency) "N/A" phase))))))
            history-frequency-amplitude-phase))

(define (make-sin-history period amplitude phase bias start-time stop-time time-step)
  (let each-time ((time start-time))
    (if (> time stop-time) '()
        (cons `(,time ,(+ (* amplitude (sin (+ (* 2 Pi (/ time period)) phase))) bias))
              (each-time (+ time time-step))))))
