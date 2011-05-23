;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; Control relays connected to virtual comm port

(declare (unit relay))

(define relay-ports '())

(define (relay-setup device)
  (verbose "relay-setup " device)
  (let-values (((i o) (open-serial-device device 9600)))
    (set! relay-ports (append relay-ports (list o)))))

(define (relay-set index on?)
  (let ioloop ((index index) (relay-ports relay-ports))
    (cond ((null? relay-ports) (error "relay index invalid" index))
          ((> index 8) (ioloop (- index 8) (cdr relay-ports)))
          (else
           (with-output-to-port (car relay-ports)
             (lambda ()
               (write-char (integer->char 255))
               (write-char (integer->char index))
               (write-char (integer->char (if on? 1 0)))
               (flush-output)))))))
