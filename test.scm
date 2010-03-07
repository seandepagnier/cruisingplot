(use srfi-1 srfi-69)

(define computations (make-hash-table))

(define (computation-register name info calculation)
  (hash-table-set! computations name
                   (list info calculation)))

(computation-register 'time "time in seconds since start" current-time)
