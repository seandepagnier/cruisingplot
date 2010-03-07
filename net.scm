;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

; This type of networking provides both client and server capabilities and
; gives unlimited control.  The basic mechanic is all scheme expressions
; delivered over the socket are evaluated remotely.

(use tcp srfi-18)

(define net-default-port 23227)

(define (net-repl)
  (call/cc (lambda (cont)
             (with-exception-handler
              (lambda _ (cont #f))
              (lambda () (write (eval (read))) (newline)))))
  (net-repl))

; run a server on a specified port
; the server handles many clients and simply performs evaluation
(define (net-server port)
  (thread-start!
   (lambda ()
     (let ((port (if port port net-default-port)))
       (verbose "starting server thread on port " port)
       (let ((listener (tcp-listen port)))
         (let accept-loop ()
           (let-values
            (((i o) (tcp-accept listener)))
            (thread-start!
             (lambda () (with-input-from-port i
                          (lambda () (with-output-to-port o
                                       net-repl))))))
           (accept-loop)))))))

; connect to a server at a given address
(define (net-add-client address expression)
  (let-values (((i o)
                (if (string-contains address ":")
                    (tcp-connect address)
                    (tcp-connect address net-default-port))))
              (thread-start! (lambda () ; handle return requests
                               (with-input-from-port i
                                 net-repl)))))
