;; Copyright (C) 2009, 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; Need to install modules with chicken:
;;
;; chicken-setup opengl glut syntax-case pos args

(use srfi-1 srfi-4 posix srfi-18 srfi-12 srfi-13 srfi-69 glut args)

(declare (uses net plot gps weather wind magnetometer computation utilities leastsquares ahrs config relay tiltcompensation wifiaimer filter task autopilot motor))

(define (setup) (lambda () #t))

(define (help)
  (print "Crusing plot.. no help available, "))

(config-setup)
(setup)
(task-schedule-loop)
