;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version. 
;; This file handles storing values (primarily from sensors)
;; values can be installed, queried, and callbacks can be installed as well
;; this should work even across a network 

;(use srfi-69)

(define sensor-log-port #f)

(define sensors (make-hash-table))

; do we have all these sensors?
(define (sensor-contains? . name-keys)
  (cond ((null? name-keys) #t)
        ((hash-table-exists? sensors (sensor-key (car name-keys)))
         (apply sensor-contains? (cdr name-keys)))
        (else #f)))

; a key is a list of (name index) or just name
(define (sensor-name name-key)
  (if (list? name-key) (car name-key) name-key))

; get the index out of they key, or use default index of 0
(define (sensor-index name-key)
  (if (list? name-key) (cadr name-key) 0))

; get the proper hash key for a name or key
(define (sensor-key name-key)
  (list (sensor-name name-key) (sensor-index name-key)))

; query a sensor from the table directly, or #f if it is not there
(define (sensor-query name-key)
  (let ((key (sensor-key name-key)))
    (if (hash-table-exists? sensors key)
        (car (hash-table-ref sensors key))
        (error "sensor-query on nonexistant key " key))))

; Open a file and automatically update values from it
(define (sensor-replay-logfile filename rate)
  (thread-start!
   (lambda ()
     (with-input-from-file filename
       (lambda ()
         (let each-line ()
           (let ((line (read-line)))
             (if (not (eof-object? line))
                 (let ((split (read-from-string line)))
                   (if (> (length split) 2)
                       (let ((time (car split))
                             (key (cadr split))
                             (value (caddr split)))
                             (thread-sleep! (/ (- time (current-time)) replayrate))
                         (sensor-update key value)))
                   (each-line))
                 (verbose "log file '" filename "' complete")))))))))

; whenever a value needs to be updated, call this
; if the table did not have this value, it is now added
(define (sensor-update name-key value)
  (verbose "sensor update: " name-key " " value)
  (if sensor-log-port
      (with-output-to-port
          sensor-log-port
          (lambda ()
            (print `(,(current-time) ,name-key ,value)))))

  (let ((key (sensor-key name-key)))
    (if (not (hash-table-exists? sensors key))
           (hash-table-set! sensors key (list value))
           (let ((ref (hash-table-ref sensors key)))
             (set-car! ref value)
             (for-each (lambda (callback) (callback)) (cdr ref))))))

; return the number of entrees in the table with this name
(define (sensor-index-count name)
  (let index-loop ((index 0))
    (if (hash-table-exists? sensors (list name index))
        (index-loop (+ index 1))
        index)))

; return a list of valid keys
(define (sensor-query-keys) (hash-table-keys sensors))

; the callback function is called with the updated sensor whenever it is updated
; eg: (sensor-install-callback 'gps (lambda (gpssensor) (print "gpssensor: " gpssensor)))
(define (sensor-register-callback name-keys callback)
  (let ((keys (map sensor-key name-keys)))
    (cond ((let all ((keys keys))
             (cond ((null? keys) #t)
                   ((hash-table-exists? sensors (car keys)) (all (cdr keys)))
                   (else #f)))
           (let ((refs (map (lambda (key) (hash-table-ref sensors key)) keys)))
            (for-each (lambda (key ref)
                         (hash-table-set! sensors key
                                          (cons (car ref) (cons (lambda ()
                                                                  (callback (map car refs))
                                                                  (cdr ref))))))
                       keys refs))
           #t)
          (net-client ; attempt to install a remote callback on the server
           (hash-table-set! sensors key (list #f callback)) ; local callback
           (sensor-register-remote-callback key)
           #t)
          (else
           (verbose "failed to register callback for " key)
           #f))))

; register a callback remotely with the server to provide this name-key
(define (sensor-register-remote-callback name-key)
  (let ((name (sensor-name name-key))
        (index (sensor-index name-key)))
    (let ((net-index (- index (sensor-index-count name))))
      (verbose "configuring server to send updates for " key)
      (if (not net-client)
          (error "sensor-register-remote-callback called without server connection"))
      (with-output-to-port
          `(if (not (sensor-install-callback
                     ,(name net-index)
                     (let ((port (current-output-port)))
                       (lambda (sensor)
                         (write (list 'sensor-update ,key sensor) port)))))
               (write (list 'display "server failed to provide " ,key) port))
        net-client))))



;;; types for fields within each physical sensor

(define types (make-hash-table))

; provide a means to extract elements from a list by name
; handler should take the subfield and a value
(define (type-register name fields)
  (hash-table-set! types name fields))

; generic means to operate on the type
(define (type-field-apply name field values func)
  (let each-def ((definition (hash-table-ref types name))
                 (values values))
    (cond 
     ((null? definition) (error "Field " field " does not exist in " name))
     ((eq? field (car definition)) (func values))
     (else (each-def (cdr definition) (cdr values))))))

; get the right field from a list
(define (type-field-get name field values)
  (type-field-apply name field values car))

; set a field in a list
(define (type-field-set! name field values value)
  (type-field-apply name field values (lambda (values) (set-car! values value))))

;;;; line reader used by various specific sensors

;; handles reading data from input sensors and executing callback functions
(define readers '())

(define (make-reader port proc end?)
  (thread-start!
   (lambda ()
     (let ((cur ""))
         (let loop ()
           (if (char-ready? port)
               (let ((c (read-char port)))
                 (call/cc
                  (lambda (flushed)
                    (cond ((end? c (lambda ()
                                     (set! cur "")
                                     (flushed #t)))
                           (proc cur)
                           (set! cur ""))
                          (else (set! cur (string-append cur (string c))))))))
               (thread-sleep! .1))
           (loop))))))

(define (make-line-reader port proc)
  (make-reader port proc (lambda (c flush) (equal? c #\newline))))

(define (sensor-field-get name-key field)
  (if (sensor-contains? name-key)
      (type-field-get (sensor-name name-key) field
                      (sensor-query name-key))
      0))

(define (open-serial-device device baud)
  (call/cc
   (lambda (return)
     (with-exception-handler
      (lambda _
        (print "device initialization failed on: " device ": " _)
        (return #f))
      (lambda ()
        (system (string-append "stty -F " device " "  (number->string baud)))
        (verbose "Trying to open serial device at " device)
        (verbose "Success opening " device)
        (values (open-input-file device)
                (open-output-file device)))))))
