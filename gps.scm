;; Copyright (C) 2009, 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

(declare (unit gps))
(declare (uses sensor units types utilities))

(use easyffi gl glu glut)

(include "glshortcuts.scm")

;; This file handles reading data from a gps through libgps

(foreign-parse "
extern int gps_read(struct gps_data_t *gpsdata);
extern bool gps_waiting(struct gps_data_t *gpsdata);
extern int gps_stream(struct gps_data_t *gpsdata, 
		      unsigned int flags, 
		      /*@null@*/void *);

extern struct gps_data_t *libgps_open(const char *host, const char *port);
extern int libgps_close(struct gps_data_t *);
extern void libgps_read(struct gps_data_t *data, double results[6]);
")

(define DEFAULT_GPSD_PORT	"2947")
(define WATCH_ENABLE   1)

(type-register 'gps '(time latitude longitude altitude heading speed
                           heading-from-position speed-from-position))

(define (register-gps-computations)
  (computation-register-first-alias 'gps '(gps))

  (computation-register 'gps-latitude "latitude in degrees from gps" '(gps)
                        (lambda () (sensor-field-get 'gps 'latitude)))
     
  (computation-register 'gps-longitude "longitude in degrees from gps" '(gps)
                        (lambda () (sensor-field-get 'gps 'longitude)))
  
  (computation-register 'gps-altitude "altitude in meters from gps" '(gps)
                        (lambda () (sensor-field-get 'gps 'altitude)))
  
  (computation-register 'gps-speed "speed in knots from gps" '(gps)
                        (lambda () (m/s->knots (sensor-field-get 'gps 'speed))))
  
  (computation-register 'gps-heading "heading in degrees from gps" '(gps)
                        (lambda () (sensor-field-get 'gps 'heading)))
  
  (computation-register 'gps-time "gps timestamp" '(gps)
                        (lambda () (sensor-field-get 'gps 'time)))

  (sensor-update `(gps 0) '(#f #f #f #f #f #f #f #f)))


(define (gps-heading-speed-from-position fix0 fix1)
  (if (or (not fix0) (any not fix0) (not fix1) (any not fix1))
      '(0 0)
      (let ((dt (- (type-field-get 'gps 'time fix1)
                   (type-field-get 'gps 'time fix0)))
            (R 6367))
        (let ((lat0 (type-field-get 'gps 'latitude fix0))
              (lon0 (type-field-get 'gps 'longitude fix0))
              (lat1 (type-field-get 'gps 'latitude fix1))
              (lon1 (type-field-get 'gps 'longitude fix1)))
          (let ((enu1 (* R (- lon1 lon0) (cos (deg2rad lat0))))
                (enu2 (* R (- lat1 lat0))))
            (list (rad2deg  (atan enu2 enu1))
                  (sqrt (+ (square enu1) (square enu2)))))))))
  
  ;; open the gps host, or connect to the gpsd server
(define (gps-setup host)    
  (let* ((split (string-split (if host host "localhost") ":"))
         (port (if (null? (cdr split))
                   DEFAULT_GPSD_PORT
                   (cadr split))))
    (verbose "Trying to connect to gpsd server at "
             (car split) " port " port) 
    (let ((data (libgps_open (car split) port)))
      (if (not data)
          (error "GPS initialization failed on " (string-append (car split) ":" port))
          (let ((index (sensor-new-index 'gps)))
            (verbose "Success connecting to gps on "
                     (car split) " reporting as gps " index)
            (if (zero? index) (register-gps-computations))

            (gps_stream data WATCH_ENABLE #f)
            (push-exit-handler (lambda () (verbose "Closing gps " index) (libgps_close data)))

            (create-periodic-task
             (string-append "gps " (number->string index) " task") .1
             (lambda ()
               (cond ((gps_waiting data)
                      (gps_read data)
                      (let ((results (make-f64vector 6 0)))
                        (libgps_read data results)
                        (sensor-update `(gps ,index)
                                       (let ((current-gps
                                              (map (lambda (result)
                                                     (if (nan? result) #f result))
                                                   (f64vector->list results))))
                                         (append current-gps
                                                 (gps-heading-speed-from-position
                                                  (sensor-query `(gps ,index))
                                                  current-gps))))))))))))))

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
		       (warning "only normal GPS not DGPS fix, discarding")
		       (begin
			 (if (equal? "S" ns)
			     (set! north (- north)))
			 (if (equal? "W" ew)
			     (set! east (- east)))
                         (list time north east alt hd 0))))))))))

;; now hack the geomag c program in place
(foreign-parse "extern int geomag70(char *modelfilename,
                         double latitude, double longitude, double alt,
                         int day, int month, double year,
                         double results[14]);");

;; Routines for International Geomagnetic Reference Field
;  provide magnetic inclination, declination, and field strength given gps input
;
; results gives DIHXYZF and respective rate of change for each in nT

(define (compute-gps-IGRF)
  (let ((results (make-f64vector 14 0))
        (lat (sensor-field-get 'gps 'latitude))
        (lon (sensor-field-get 'gps 'longitude))
        (alt  (sensor-field-get 'gps 'altitude))
;        (date (current-date))
        )
    (cond ((and lat lon alt)
           (if (= -1 (geomag70
                      "geomag70/IGRF11.COF"
                      lat lon alt
;               (date-day date) (date-month date) (date-year date)
                      3 10 2012
                   results))
               (warning "geomag70 failed"))
           results)
          (else #f))))

(computation-register 'gps-magnetic-declination "The declination of the magnetic field, calculated from gps position, and date" '(gps)
                      (lambda () (let ((res (compute-gps-IGRF)))
                                   (if res (f64vector-ref res 0) #f))))

(computation-register 'gps-magnetic-inclination "The inclination of the magtitude of the field, calculated from gps position, or with no gps, measured from angle of magnetometer and accelerometer"  '(gps)
                      (lambda () (let ((res (compute-gps-IGRF)))
                                   (if res (f64vector-ref res 1) #f))))

(computation-register 'gps-magnetic-fieldstrength "The declination of the magnetic field, calculated from gps position, and date" '(gps)
                      (lambda () (let ((res (compute-gps-IGRF)))
                                   (if res (f64vector-ref res 6) #f))))

(define gps-plot #f)

(define (create-gps-plot-from-string arg)
  (glut:ReshapeFunc
   (lambda (w h)
     (gl:Viewport 0 0 w h)
         ; setup projection matrix
     (gl:MatrixMode gl:PROJECTION)
     (gl:LoadIdentity)))

    (glut:DisplayFunc
     (let ((lastspd 0) (lasthdg 0))
       (lambda ()
         (define (draw-print . args)
           (glLetMatrix
            (let each-char ((l (string->list (with-output-to-string (lambda () (for-each display args))))))
              (cond ((not (null? l))
                     (glut:StrokeCharacter glut:STROKE_MONO_ROMAN (car l))
                     (each-char (cdr l)))))))
         
         (gl:ClearColor 0 0 0 0)
         (gl:Clear gl:COLOR_BUFFER_BIT)
         
         (glColor 1 1 1)
         (glLetMatrix
          (gl:LineWidth 4)
          (gl:Translated -1 .9 0)
          (gl:Scaled .001 .001 .001)
          
          (let ((spd (computation-calculate 'gps-speed))
                (hdg (computation-calculate 'gps-heading)))
            (draw-print "Speed: " (round-to-places spd 2) " knts")
            (gl:Translated 0 -200 0)
            (draw-print "Accel: " (round-to-places (- spd lastspd) 2))
            (set! lastspd spd)

            (gl:Translated 0 -200 0)
            (draw-print "Heading: " (round-to-places hdg 2))
            (gl:Translated 0 -200 0)
            (draw-print "Turn Rt: " (round-to-places (- hdg lasthdg) 2))
            (set! lasthdg hdg)))

       (glut:SwapBuffers)
       (glut:TimerFunc 1000 glut:PostWindowRedisplay (glut:GetWindow)))))

    (glut:KeyboardFunc
     (lambda (key x y)
       (case key
         ((#\esc #\q) (exit))
         ((#\f) (glut:FullScreen)))
       (glut:PostRedisplay)))
    
    (glut:SpecialFunc
     (lambda (key x y)
       (if (glut-HasModifiers glut:ACTIVE_SHIFT)
           (let ((rs 1))
             (cond
              ((= key glut:KEY_LEFT) (RotateAfter rs 0 1 0))
              ((= key glut:KEY_RIGHT) (RotateAfter rs 0 -1 0))
              ((= key glut:KEY_UP) (RotateAfter rs 1 0 0))
              ((= key glut:KEY_DOWN) (RotateAfter rs -1 0 0))
              ((= key glut:KEY_PAGE_UP) (RotateAfter rs 0 0 1))
              ((= key glut:KEY_PAGE_DOWN) (RotateAfter rs 0 0 -1))))
           (let ((ts 1))
             (cond
              ((= key glut:KEY_LEFT) (gl:Translatef ts 0 0))
              ((= key glut:KEY_RIGHT) (gl:Translatef (- ts) 0 0))
              ((= key glut:KEY_UP) (gl:Translatef 0 (- ts) 0))
              ((= key glut:KEY_DOWN) (gl:Translatef 0 ts 0))
              ((= key glut:KEY_PAGE_UP) (set-zoom .5))
              ((= key glut:KEY_PAGE_DOWN) (set-zoom 2))
              )))
       (glut:PostRedisplay))))

