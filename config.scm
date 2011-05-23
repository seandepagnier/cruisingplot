;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

(declare (unit config))

(use args glut)

(define config-file #f)

(define (usage)
  (print "Usage: " (car (argv)) " [options...]")
  (newline)
  (print (args:usage opts))
  (print "Report bugs to sean at depagnier dot com")
  (exit))

(define output-file #f)

(define opts
 (list
  (args:make-option (a ahrs) (optional: "OPTIONS") "Create an ahrs which runs kalman filter algorithms.  Invoke -a help for more information."
                    (make-ahrs-from-string arg))
  (args:make-option (C config) (required: "FILENAME") "include a config file which contentss specifies command line options"
                    (with-input-from-file arg
                      (lambda ()
                        (let loop ()
                          (let ((line (read-line)))
                            (if (not (eof-object? line))
                                (let ((space (string-index line #\space)))
                                  (if space
                                      (args:parse
                                       (list (string-append "-" (string-take line space))
                                             (string-drop line (+ space 1)))
                                       opts)
                                      (error "invalid configuration line" line))
                                  (loop))))))))

  (args:make-option (c client) (required: "HOST") "Connect to a remote host to obtain sensor data"
                    (sensor-net-client arg))
  (args:make-option (d debug) (required: "EXPRESSION1[,EXPESSION2...]")
                    "Perform the given computation at 1hz and print to stdout, help for info"
                    (debug arg))
  (args:make-option (f filter) (required: "EXPRESSION1[,options]")
                    "Filter this computation, help for info"
                    (create-filter arg))
  (args:make-option (g gps) (optional: "DEVICE") "device or url for gps data, if none is specified, then the default gpsd location (localhost:2947) is tried."
                    (gps-setup arg))
  (args:make-option (h help) #:none "Display this text" (usage))
  (args:make-option (i input) (required: "FILENAME,options")
                    "Use a file with sensor data as input (replay), -i help for info"
                    (sensor-replay-logfile arg))
  (args:make-option (m magnetometer)   (required: "DEVICE") "device or url for mag data."
                    (magnetometer-setup arg))
  (args:make-option (o output) (required: "FILENAME")
                    "Write all input sensor data to a log file log for future replay"
                    (verbose "adding output log file: " (if (equal? arg "-") "<stdout>" arg))
                     (if (equal? arg "-")
                         (sensor-log-to-port (current-output-port))
                         (sensor-log-to-file arg)))
  (args:make-option (p plot) (required: "PLOT") "Plot computations.  Invoke with -p help for more information."
                    (set! setup (lambda ()
                                 (sleep 1)
                                 (plots-setup plots)))
                    (set! plots (append plots `(,(lambda () (create-plot-from-string arg))))))
  (args:make-option (s server) (optional: "PORT")
                    (string-append "Run a server listening on the specified port or "
                                   (number->string net-default-port)
                                   " by default")
                    (net-server arg))
  (args:make-option (v verbose) #:none  "Print debugging info, -vv for extra info"
                    (cond ((eq? verbose nice-print)
                           (verbose "Extra Debugging output "
                                    (if (eq? very-verbose verbose) "already " "")
                                    "enabled")
                           (set! very-verbose verbose))
                          (else
                           (set! verbose nice-print)
                           (verbose "Debugging output enabled"))))
  (args:make-option (w weather)   (required: "DEVICE") "device or url for weather data"
                    (weather-setup arg))

  (args:make-option (2 gps-plot) (optional: "ARGS") "draw gps plot display."
                    (set! plots (append plots `(,(lambda () (create-gps-plot-from-string arg))))))
  (args:make-option (3 relay) (required: "RELAY") "Device to use for relay control"
                    (relay-setup arg))
  (args:make-option (4 wifiaimer) (required: "options")
                    "Device to control servo to aim antenna, -wifiaimer help for more info"
                    (create-wifi-aimer arg))
  (args:make-option (6 tiltcompensate) #:none "Enable tilt-compensation routines"
                    (tilt-compensation-setup))
  (args:make-option (9 9DOF) (required: "DEVICE") "device to use for sparkfun 9DOF"
                    (sensor-9dof-setup arg))
  ))

(define (config-setup)
  (very-verbose "Configuration setup")
  (args:parse (command-line-arguments) opts)
  (verbose "Configuration complete."))
