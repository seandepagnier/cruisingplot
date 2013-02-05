;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version. 


;; This file handles storing values (primarily from sensors)
;; values can be installed, queried, and callbacks can be installed as well
;; this should work even across a network 

(declare (unit sensor))

(use srfi-69)

(define sensor-log-ports '())

(define (sensor-log-to-port port)
  (set! sensor-log-ports (cons port sensor-log-ports)))

(define (sensor-log-to-file filename)
  (sensor-log-to-port (open-output-file filename)))

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

; get a list if indicies of of sensors by name
(define (sensor-indicies name)
  (let each-value ((values (hash-table->alist sensors)))
    (cond ((null? values) '())
          ((eq? (caar values) name)
           (cons (cadar values)
                 (each-value (cdr values))))
          (else
           (each-value (cdr values))))))

; get the proper hash key for a name or key
(define (sensor-key name-key)
  (list (sensor-name name-key) (sensor-index name-key)))

; query a sensor from the table directly, or #f if it is not there
(define (sensor-query name-key)
  (let ((key (sensor-key name-key)))
    (if (hash-table-exists? sensors key)
        (hash-table-ref sensors key)
        #f)))

(define (sensor-query-indexes name indexes)
  (map (lambda (index) (sensor-query (list name index)))
       indexes))

; Open a file and automatically update values from it
(define (sensor-replay-logfile arg)
  (let ((options (create-options
                  (list (make-number-verifier 'rate "rate of replay" 1 .001 1000))
                  (string-append "  Set input log file, and option\n")
                  #f)))
    (let ((filename (parse-basic-arg-options-string options arg)))
      (verbose "replaying data from log file '" filename "' at " (options 'rate) "x speed")

  (create-task
   (string-append "replay task for file: " filename)
   (lambda ()
     (with-input-from-file filename
       (lambda ()
         (let ((sensor-key-mapping (make-hash-table)))
         (let each-line ()
           (let ((line (read-line)))
             (if (not (eof-object? line))
                 (let ((split (read-from-string line)))
                   (if (> (length split) 2)
                       (let ((time (car split))
                             (key (cadr split))
                             (value (caddr split)))
                         (task-sleep! (- (/ time (options 'rate)) (elapsed-seconds)))
                         (if (hash-table-exists? sensor-key-mapping key)
                             (sensor-update (hash-table-ref sensor-key-mapping key) value)
                             (let ((new-key `(,(sensor-name key)
                                              ,(sensor-new-index (sensor-name key)))))
                               (verbose "replay sensor from file '" filename "' " key " -> " new-key)
                               (hash-table-set! sensor-key-mapping key new-key))))))))
                   (each-line))
                 (verbose "log file '" filename "' complete")))))))))

(define (make-raw-sensor-computation name index)
;  (print "make-raw-sensor-computation " name " " index)
  (computation-register (string->symbol (string-append (symbol->string name)
                                                       "."
                                                       (number->string index)))
                        (string-append (symbol->string name) " " (number->string index))
                        `(,name)
                        (lambda () (sensor-query `(,name ,index)))))

; whenever a value needs to be updated, call this
; if the table did not have this value, it is now added
(define (sensor-update name-key value)
;  (very-verbose "sensor update: " name-key " " value)
  (set! sensor-log-ports
        (let each-port ((ports sensor-log-ports))
          (cond ((null? ports) '())
                (else
                 (if (call/cc
                      (lambda (cont)
                        (with-exception-handler
                         (lambda _ (cont #f))
                         (lambda ()
                           (with-output-to-port (car ports)
                             (lambda () (print `(,(elapsed-seconds) ,name-key ,value))))
                           (cont #t)))))
                     (cons (car ports) (each-port (cdr ports)))
                     (each-port (cdr ports)))))))

  ; add computation for raw sensor data
  (if (not (hash-table-exists? sensors (sensor-key name-key)))
      (make-raw-sensor-computation (sensor-name name-key) (sensor-index name-key)))

  ; store value locally
  (hash-table-set! sensors (sensor-key name-key) value))


(define (sensor-net-client address)
  (net-add-client address
                  '(let ((port (current-output-port))) (sensor-log-to-port port) 'ok)
                  (lambda (data)
                    (if (not (eq? data 'ok))
                        (sensor-update (second data) (third data))))))

; return the number of entrees in the table with this name
(define (sensor-index-count name)
  (let index-loop ((index 0))
    (if (hash-table-exists? sensors (list name index))
        (index-loop (+ index 1))
        index)))

; register a new sensor index without updating it yet, return the index
(define (sensor-new-index name)
  (let ((index (sensor-index-count name)))
    (make-raw-sensor-computation name index)
    (hash-table-set! sensors `(,name ,index) #f)
    index))

; return a list of valid keys
(define (sensor-query-keys) (hash-table-keys sensors))

;;;; line reader used by various specific sensors

;; handles reading data from input sensors and executing callback functions
(define readers '())

(define (make-reader port proc end?)
  (create-periodic-task
   (string-append "reader for " (port->string port)) .05
   (let ((cur ""))
     (lambda ()
       (if (char-ready? port)
           (let ((c (read-char port)))
             (call/cc
              (lambda (flushed)
                (cond ((end? c (lambda ()
                                 (set! cur "")
                                 (flushed #t)))
                       (proc cur)
                       (set! cur ""))
                      (else (set! cur (string-append cur (string c)))))))))))))

(define (make-line-reader port proc)
  (create-periodic-task
   (string-append "line reader for " (port->string port)) .02
   (lambda ()
     (let loop ()
       (if (char-ready? port)
           (let ((l (read-line port)))
             (proc l)
             (loop)))))))


(define (sensor-field-get name-key field)
  (if (sensor-contains? name-key)
      (type-field-get (sensor-name name-key) field
                      (sensor-query name-key)) #f))

(define (open-serial-device-with-handler device baud handler)
  (call/cc (lambda (bail)
             (verbose "Trying to open serial device at " device)
             (let ((ret (with-exception-handler
                         (lambda _
                           (apply bail (handler _)))
                         (lambda ()
                           (if (not (eq? baud 'none))
                               (let ((cmd (string-append "stty -F " device " "  (number->string baud))))
                                 (verbose "executing shell command: " cmd)
                                 (system cmd)))
                           (let ((cmd (string-append "stty -F " device
                                                     " ignbrk -icrnl -opost -onlcr -isig"
                                                     " -icanon -iexten -echo -echoe -echok"
                                                     " -echoctl -echoke min 1 time 5")))
                             (verbose "executing shell command: " cmd)
                             (system cmd))
                           (list (open-input-file device)
                                 (open-output-file device))))))
                   (verbose "Success opening " device)
                   (apply values ret)))))
             
(define (open-serial-device device baud)
  (open-serial-device-with-handler
   device baud (lambda _
                 (print "device initialization failed on: " device ": " _)
                 (exit))))

(define (try-opening-serial-devices devices baud)
  (if (null? devices)
      (values #f #f)
      (let-values (((i o) (open-serial-device-with-handler (car devices) baud
                                                           (lambda _ '(#f #f)))))
        (if (and i o) (values i o)
            (try-opening-serial-devices (cdr devices) baud)))))

(define (generic-serial-sensor-reader arg)
  (define options
    (create-options
     `(,(make-string-verifier 'device "serial device" "/dev/ttyUSB0")
       ,(make-boolean-verifier 'windvane-control "control windvane with winch servo thru this device" #f)
       ,(make-baud-verifier 'baudrate "serial baud rate to use" 'none))
     "no examples" #f))

  (parse-basic-options-string options arg)

  (let ((i #f) (o #f)
        (possible-sensors '((accel 1333 #f)
                            (mag 50000 #f)
                            (windvane 1023 #f))))
    (define (config)
      (display "set /sensors/accel/outputrate " o)
      (display rate o) (newline o)
      (display "set /sensors/mag/outputrate " o)
      (display rate o) (newline o))

    (define (connect-serial-port)
      (let-values (((ni no) (try-opening-serial-devices
                             `(,(options 'device) "/dev/ttyUSB0" "/dev/ttyUSB1"
                               "/dev/ttyACM0" "/dev/ttyACM1")
                             (options 'baudrate))))
        (if (options 'windvane-control)
            (set-windvane-control-port! no))
        (set! i ni) (set! o no)))

    (connect-serial-port)
    (cond ((not (and i o))
           (print "failed to initialize generic serial reader at " (options 'device)))
          (else

;    (config)
    (make-line-reader
     i
     (lambda (line)
       (cond ((eof-object? line)
              (verbose "serial end of file: we will try to reconnect")
              (connect-serial-port)
              (cond ((not (and i o))
                     (for-each
                      (lambda (sensor)
                        (let ((sensor-indexes (third sensor)))
                          (if sensor-indexes
                              (for-each (lambda (sensor-index)
                                          (sensor-update (list (first sensor)
                                                               sensor-index) #f))
                                        sensor-indexes))))
                        possible-sensors)
                     (task-sleep 1))))
             (else
              (let ((words (string-split line)))
                (if (> (length words) 1)
                    (let ((sensor (find (lambda (sensor)
;                                          (print "lambda (sensor) " (car sensor) " " (car words))
                                          (equal? (car sensor)
                                                  (string->symbol (remove-last-character (car words)))))
                                        possible-sensors))
                          (sensor-values (map string->number (cdr words))))
                      (cond (sensor
                             (if (not (third sensor))
                                 (set-car! (cddr sensor)
                                           (let sensor-new-indexes ((local-index 0))
                                             (if (< local-index (- (length words) 1))
                                                 (cons (sensor-new-index (first sensor))
                                                       (sensor-new-indexes (+ local-index 1)))
                                                 '()))))
                             (for-each (lambda (sensor-value sensor-index)
                                         (sensor-update (list (first sensor) sensor-index)
                                                        (/ sensor-value (second sensor))))
                                       sensor-values (third sensor)))
                            (else
                             (warning-once "unrecognized sensor: " words))))))))))))))
