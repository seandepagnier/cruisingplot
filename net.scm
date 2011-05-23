;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

; This type of networking provides both client and server capabilities and
; gives unlimited control.  The basic mechanic is all scheme expressions
; delivered over the socket are evaluated remotely, and the results sent back
; over the network

(declare (unit net))

(use tcp srfi-18)

(define net-default-port 23227)

; given an input and output port, in a loop,
; read from input, evaluate, and write to output
(define (net-repl i o)
  (call/cc (lambda (cont)
             (with-exception-handler
              (lambda _ (cont #f))
              (lambda ()
                (let loop ()
                  (with-input-from-port i
                    (lambda () (with-output-to-port o
                                 (lambda ()
                                   (write (eval (read)))
                                   (newline)))))
                  (task-sleep .001) (loop)))))))

; start a server on a specified port
; the server handles many clients and simply performs evaluation
(define (net-server port)
  (create-task
   (lambda ()
     (let ((port (if port port net-default-port)))
       (verbose "starting server task on port " port)
       (let ((listener (tcp-listen port)))
         (let accept-loop ()
           (let-values
            (((i o) (tcp-accept listener)))
            (create-task (lambda () (net-repl i o))))
           (accept-loop)))))))

; connect to a server at a given address, write expression, and call receiver
; repeatedly for each lien
(define (net-add-client address expression receiver)
  (let-values (((i o)
                (if (string-contains address ":")
                    (tcp-connect address)
                    (tcp-connect address net-default-port))))
              (create-task (lambda () ; handle return requests
                               (verbose "connected to server")
                               (write expression o)
                               (newline o)
                               (let loop () (receiver (read i)) (loop))))

              ; delay after connecting to server to allow some data
              (task-sleep .5)))
