;; Copyright (C) 2011 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;;  Use wind speed and direction along with gps speed to build a database
;;  storing collected data.   This gives an idea of boat performance with
;;  a given sail configuration, and lets the user know if they are sheeted wrong


(declare (unit sailspeed))
(declare (uses algebra matrix plot utilities))

;; store database as vector of wind speeds at each knot of wind 0-19
;;                          of wind directions (each degree)
;;                          of max boat speed through water

(define sailspeed-max-windspeed 20)

(define sailspeed-database
  (make-vector (+ 1 sailspeed-max-windspeed) (make-vector 360 #f)))

(define (sailspeed-database-ref wind-speed wind-direction)
  (vector-ref (vector-ref sailspeed-database wind-speed) wind-direction))

(define (sailspeed-database-true-wind-speed wind-speed wind-direction)
  (first (sailspeed-database-ref wind-speed wind-direction)))

(define (sailspeed-database-true-wind-direction wind-speed wind-direction)
  (second (sailspeed-database-ref wind-speed wind-direction)))

(define (sailspeed-database-water-speed wind-speed wind-direction)
  (third (sailspeed-database-ref wind-speed wind-direction)))

(define (sailspeed-database-set! wind-speed wind-direction
                                 true-wind-speed true-wind-direction water-speed)
  (vector-set! (vector-ref sailspeed-database wind-speed) wind-direction
               `(true-wind-speed true-wind-direction water-speed)))

(define (relative-wind-from-angle-of-attack angle-of-attack wind-speed water-speed water-direction)
  (vector+ `(0 ,water-direction) (course+ `(,wind-speed ,(- angle-of-attack)) `(,water-speed 0))))


(define (sailspeed-database-update angle-of-attack wind-speed water-speed water-direction)
  (sailspeed-database-set! angle-of-attack wind-speed
                           true-wind-speed true-wind-direction
                           (max water-speed
                                (sailspeed-database-water-speed wind-speed wind-direction))))

; The sailboat transform:
;                                   2
;                      / (sin(a/2) \  
;  sin(a0) * sin(a) * |   -------   | = VW * eta
;                      \ sin(a0-a) /
;
;  a0 – real wind direction relatively to boat
;
;  a – apparent wind direction relatively to boat
;  VW – real wind velocity
;  eta – sailing boat's slowly changing function of many parameters (1/velocity units),
;         which incorporates most of the boat's and rigging's specific Physics. 
;
(define (sailspeed-compute-sailboat-transform)

; Find boat speed in terms of transform
;
;         VW * sin(a0 - a) 
;  VB =   ----------------
;              sin(a)
(define (sailspeed-sailboat-transform-boat-speed a0 a VW)
  (/ (* VW (sin (deg2rad (- a0 a))))
     (sin (deg2rad a))))


; compute optimal course, or courses if tacking/jibing is needed to achieve highest
; velocity made good

(define (sailspeed-optimal-course true-wind)
  (let each-angle ((best-angle 0) (best-vmg 0) (current-angle 0))
    (if (= current-angle 360) best-angle
        (let ((best-speed (sailspeed-database-water-speed wind-speed current-angle)))
          (if best-speed
              (let ((vmg (* best-speed (cos (* (if upwind? 1 -1) (deg2rad best-vmg))))))
                (if (> vmg best-vmg)
                    (each-angle current-angle vmg (+ current-angle 1))
                    (each-angle best-angle best-vmg (+ current-angle 1))))
              (each-angle best-angle best-vmg (+ current-angle 1)))))))

(define (sailspeed-optimal-tack-angle wind-speed)
  (sailspeed-optimal-velocity-made-good #t wind-speed))

(define (sailspeed-optimal-jibe-angle wind-speed)
  (sailspeed-optimal-velocity-made-good #f wind-speed))

(define (sailspeed-database-print wind-speed-step wind-direction-step)
  (let ((wind-speeds (sequence 1 sailspeed-max-wind wind-speed step))
        (wind-directions (sequence 0 360 wind-direction-step)))
  (display "\t")
  (for-each
   (lambda (wind-speed) (display wind-speed) (display "\t"))
   wind-speeds)

  (for-each
   (lambda (wind-direction)
     (for-each
      (lambda (wind-speed)
        (display wind-direction)
        (display "\t")
        (display (sailspeed-database-ref wind-speed wind-direction))
        (newline))
      wind-speeds))
   wind-directions)))

(define stability-time 10) ; 10 seconds is a stable reading

(define (make-stable-reading thunk tolerance)
  (let ((stable-sum (thunk))
        (stable-count 1)
        (stable-timer (start-timer)))
    (lambda ()
      (let ((reading (thunk))
            (stable-reading (/ stable-sum stable-count)))
        (set! stable-count (+ stable-count 1))
        (set! stable-sum (+ stable-sum reading))
        (cond ((> (abs (- reading stable-reading)) tolerance)
               (set! stable-timer (start-timer))
               (set! stable-sum 0)
               (set! stable-count 0)))
        (if (> (stable-timer) stability-time)
            stable-sum #f)))))

(define sailspeed-update
  (let ((stable-wind-speed (make-stable-reading (lambda () (computation-calculate 'wind-speed)) 3))
        (stable-wind-direction (make-stable-reading (lambda () (computation-calculate 'wind-direction)) 10))
        (stable-water-speed (make-stable-reading (lambda () (computation-calculate 'water-speed)) 1))
        (stable-heading (make-stable-reading (lambda () (computation-calculate 'water-heading)) 10))
        (stable-gps-speed (make-stable-reading (lambda () (computation-calculate 'gps-speed)) 3))
        (stable-gps-heading (make-stable-reading (lambda () (computation-calculate 'gps-heading)) 10)))
    (lambda ()
      (let ((wind-speed (stable-wind-speed))
            (wind-direction (stable-wind-direction))
            (water-speed (stable-water-speed))
            (heading (stable-heading))
            (gps-speed (stable-gps-speed))
            (gps-heading (stable-gps-heading)))
        (if (and wind-speed wind-direction water-speed heading gps-speed gps-heading)
            (let ((true-wind (calculate-true-wind heading wind-speed wind-direction gps-speed gps-heading)))
              (sailspeed-database-update wind-speed wind-direction
                                         water-speed (first true-wind) (second true-wind))))))))
