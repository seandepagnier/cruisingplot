;; Copyright (C) 2009, 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; Need to install modules with chicken:
;;
;; chicken-setup opengl glut syntax-case pos args

(use srfi-1 srfi-4 posix srfi-18 srfi-12 srfi-13)

(define (read-from-string string) (with-input-from-string string read))

(define (die . args)
  (apply print "Error: " args)
  (exit 1))

(define (warning . args)
  (apply print "Warning: " args))

(define warning-once
  (let ((table (make-hash-table)))
    (lambda args
      (cond ((not (hash-table-exists? table args))
             (hash-table-set! table args #t)
             (apply warning args))))))

(define replayrate 1)

(define current-time
  (let ((start (current-milliseconds)))
    (lambda () (* replayrate (/ (- (current-milliseconds) start) 1000)))))

(use args)

(define (usage)
  (print "Usage: " (car (argv)) " [options...]")
  (newline)
  (print (args:usage opts))
  (print "Report bugs to geckosenator at gmail.")
  (exit 1))

(define output-file #f)
(define noshow #f)

(define verbose (lambda _ #t))

(define (push-exit-handler handler)
 (exit-handler (let ((old-handler (exit-handler))) (lambda () (handler) (old-handler)))))

(define plots '())

(define opts
 (list
  (args:make-option (c client) (required: "HOST") "Connect to a remote host to obtain data"
                    (net-add-client arg))
  (args:make-option (g gps)   (required: "DEVICE") "device or url for gps data, if none is specified, then the default gpsd location (localhost:2947) is tried."
                    (gps-setup arg))
  (args:make-option (h help) #:none "Display this text" (usage))
  (args:make-option (i input) (required: "FILENAME") "Use a file log for replay"
                    (verbose "replaying data from log file '" arg "' at " replayrate "x speed")
                    (sensor-replay-logfile arg replayrate))
  (args:make-option (m magnetometer)   (required: "DEVICE") "device or url for mag data."
                    (magnetometer-setup arg))
  (args:make-option (o output) (required: "FILENAME")
                    "Write all input sensor data to a log file log for future replay"
                    (verbose "setting output log file to " (if (eq? arg "-") "<stdout>" arg))
                    (set! sensor-log-port
                          (if (eq? arg "-")
                              (current-output-port)
                              (open-output-file arg))))
  (args:make-option (r replayrate) (required: "RATE") "Rate to replay the log (0 for infinite)"
                    (set! replayrate (string->number arg)))
  (args:make-option (p plot) (required: "PLOT") "Plot setup invoke with -p help for info"
                    (set! plots (append plots (list arg))))
  (args:make-option (settings) (required: "FILENAME") "Read and write runtime settings from file"
                    (settings-read arg)
                    (push-exit-handler (lambda () (settings-write arg))))
  (args:make-option (s server) (optional: "PORT")
                    (string-append "Run a server listening on the specified port or "
                                   (number->string net-default-port)
                                   " by default")
                    (net-server arg))
  (args:make-option (v verbose) #:none  "Print debugging info"
		    (set! verbose print))
  (args:make-option (w weather)   (required: "DEVICE") "device or url for weather data"
                    (weather-setup arg))
  ))

(args:parse (command-line-arguments) opts)

(if (null? plots) (let x () (thread-sleep! 1) (x)))

(glut:InitDisplayMode (+ glut:DOUBLE glut:RGB glut:ALPHA))
(plots-setup plots)
(glut:IdleFunc (lambda () (thread-sleep! .01)))
(glut:MainLoop)
