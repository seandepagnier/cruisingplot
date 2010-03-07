;; Copyright (C) 2009, 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This file handles reading data from a gps, either raw or through gpsd

(type-register 'gps '(time latitude longitude altitude heading speed))

  ;; open the gps device, or connect to the gpsd server
(define (gps-setup device)
  (call/cc (lambda (return)
             (with-exception-handler
              (lambda _
                (print "GPS initialization failed on: " device ": " _)
                (return #f))
              (lambda ()
                (let* ((split (string-split device ":"))
                       (port (if (null? (cdr split))
                                 2947
                                 (string->number (cadr split)))))
                  (verbose "Trying to connect to gpsd server at " (car split)
                           " port " port) 
                  (let-values ([(i o) (tcp-connect (car split) port) ])
                              (verbose "Success connecting to gps on " (car split))
                              (let ((index (sensor-index-count 'gps)))
                                (verbose "reporting as gps " index)
                                (write "w\n" o) ; put in query mode
                                (make-line-reader i
                                 (lambda (line)
                                   (let ((report (parse-gpsd-line line)))
                                     (if report
                                         (sensor-update `(gps ,index) report))))))
                              (return #t))))))))

(define (parse-gpsd-line line)
  (define (parse-gpsd-report line)
    (call/cc (lambda (return)
               (let ((vals (string-split line " ")))
                 (define (parse-value ref)
                   (let ((value (string->number (list-ref vals ref))))
                     (if (not value) (return #f))
                     value))
                 (cond ((= (length vals) 15)
                        (list (parse-value 1) (parse-value 3) (parse-value 4)
                              (parse-value 5) (parse-value 8) (parse-value 9)))
                       (else #f))))))
  ; search for "GPSD," at beginning of string, and remove it or return false
  (if (and (> (string-length line) 5)
           (string= "GPSD," (string-take line 5)))
      (case (string-ref line 5)
        ((#\O) (parse-gpsd-report (string-drop line 7)))
        (else #f))
      #f))
 
(define (parse-nmea-line line)
  (let ((vals (string-split line ",")))
    (cond ((and (> (length vals) 9)
		(equal? (car vals) "$GPGGA"))
	   (let ((time (string->number (list-ref vals 1)))
		 (north (string->number (list-ref vals 2)))
		 (ns (list-ref vals 3))
		 (east (string->number (list-ref vals 4)))
		 (ew (list-ref vals 5))
		 (fix (string->number (list-ref vals 6)))
		 (satcount (string->number (list-ref vals 7)))
		 (hd (string->number (list-ref vals 8)))
		 (alt (string->number (list-ref vals 9))))
	     (if (= fix 0)
		 (print "no gps fix, sats: " satcount)
		 (let ()
		   (define (convert-deg val)
		     (+ (floor (/ val 100))
			(/ (remainder val 100) 60)))
		   (if (< fix 1) ; hacked to turn on without DGPS
		       (print "warning, only normal GPS not DGPS fix, discarding")
		       (begin
			 (if (equal? "S" ns)
			     (set! north (- north)))
			 (if (equal? "W" ew)
			     (set! east (- east)))
                         (list time north east alt hd 0))))))))))

(computation-register 'gps-speed "speed in knots from gps"
                      (lambda () (m/s->knots (sensor-field-get 'gps 'speed))))

(computation-register 'gps-heading "heading in degrees from gps"
                      (lambda () (sensor-field-get 'gps 'heading)))

(computation-register 'gps-time "gps timestamp"
                      (lambda () (sensor-field-get 'gps 'time)))

;; Routines for International Geomagnetic Reference Field
;  provide magnetic inclination, declination, and field strength given gps input
(define (compute-IGRF date lattitude longitude altitude)
  1
)

(computation-register 'magnetic-inclination "The inclination of the magtitude of the field, calculated from gps position, or with no gps, measured from angle of magnetometer and accelerometer"
                      (lambda () 0))
(computation-register 'magnetic-declination "The declination of the magnetic field, calculated from gps position, and date"
                      (lambda () 0))
(computation-register 'field-strength "The declination of the magnetic field, calculated from gps position, and date"
                      (lambda () 0))
