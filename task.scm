;; Copyright (C) 2011 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

(declare (unit task))
(declare (uses srfi-1 srfi-18 utilities))

; list of tasks
(define tasks '())

(define task-info first)
(define task-period second)
(define task-sleep-until third)
(define task-thunk fourth)
(define task-continuation fifth)

(define current-task #f) ; task currently running
(define current-task-cont #f) ; continuation to exit current task

(define (task-set-period! task period)
  (set-car! (cdr task) period))

(define (task-set-sleep-until! task sleep-until)
  (set-car! (cddr task) sleep-until))

(define (task-set-continuation! task cont)
  (set-car! (cddddr task) cont))

; sleep for the task period if seconds not specified
(define (task-sleep . seconds)
  (if (not current-task) (error "task-sleep called from non-task"))
  (call/cc (lambda (cont)
             (task-set-continuation! current-task (lambda () (cont #t)))
             (current-task-cont (if (null? seconds) #t (first seconds))))))

(define (task-exit)
  (if (not current-task) (error "task-exit called from non-task"))
  (current-task-cont 'exit))

; Run thunk every period seconds (resolution milliseconds)
(define (create-periodic-task info period thunk)
  (very-verbose "create-periodic-task " info " " tasks)
  (set! tasks (cons (list info period 0 thunk thunk) tasks)))

; Run the thunk as a task, when it returns the task exits
(define (create-task info thunk)
  (create-periodic-task info 0
                        (lambda () thunk
                                (task-exit))))

; could be replaced with a heap if there were many tasks
; returns sorted task list
(define (insert-periodic-task task tasks)
    (cond ((null? tasks) (list task))
          ((< (task-sleep-until task) (task-sleep-until (car tasks)))
           (cons task tasks))
          (else
           (cons (car tasks) (insert-periodic-task task (cdr tasks))))))

(define (create-task-scheduler sleep-while-idle)
  (let ((timer (start-timer))
        (total-run-time 0)
        (total-sleep-time 0)
        (last-total-run-time 1)
        (task-time 0))

    (create-periodic-task
     "task-stats-task" 1
     (let ((last-time 0))
     (lambda ()
       (let ((time (timer)))
         (let ((elapsed (- time last-time)))
           (set! last-time time)
           (define (time-percentage t)
             (string-append (if (zero? elapsed) "N/A"
                                (number->string (round-to-places (* 100 (/ t elapsed)) 2)))
                            "%"))
           (verbose "<" (length tasks) " tasks>"
                         " task-time: " (time-percentage total-run-time)
                         " non-task: " (time-percentage (- elapsed total-run-time total-sleep-time))
                         " sleep: " (time-percentage total-sleep-time))
           
           (set! last-total-run-time total-run-time)
           (set! total-run-time 0)
           (set! total-sleep-time 0)

             )))))

    (lambda ()
        (cond ((null? tasks)
               (print "No tasks exist, exiting.")
               (exit)))
        (let ((task (first tasks)))
          (cond ((< (task-sleep-until task) (timer))
                 (set! current-task task)
                 
                 (let ((ret (call/cc (lambda (cont)
                                       (set! current-task-cont cont)
                                       (set! task-time (timer))
                                       ((task-continuation task))
                                       (task-set-continuation! task (task-thunk task))
                                       'return))))
                   (let ((run-time (- (timer) task-time)))
                     (set! current-task #f)
                     (set! total-run-time (+ total-run-time run-time))
                     (cond ((eq? ret 'exit)
                            (verbose "task " (task-info task) " exited")
                            (set! tasks (cdr tasks))) ; delete this task
                           (else
                            (cond ((and (> task-time 5) ; dont do this first 5 seconds
                                        (> run-time (/ (task-period task) (length tasks)))
                                        (> (- (timer) (task-sleep-until task))
                                           (* 2 (task-period task))))
                                   (verbose "task " (task-info task) " took too long "
                                            " " (round-to-places run-time 5) " seconds"
                                            " doubling period to " (* 2 (task-period task)))
                                   (task-set-period! task (* 2 (task-period task)))))
                            (task-set-sleep-until! task
                                                   (if (number? ret)
                                                       (+ (timer) ret)
                                                       (+ (task-sleep-until task)
                                                          (task-period task))))
                            (set! tasks (insert-periodic-task task (cdr tasks)))
                            )))))
          
                 (else ; sleep it off
                  (let ((sleep-start-time (timer)))
                    (sleep (if sleep-while-idle
                                       (- (task-sleep-until task) sleep-start-time)
                                       .001))
                    (let ((sleep-time (- (timer) sleep-start-time)))
                      (set! total-sleep-time (+ total-sleep-time sleep-time))))))))))

(define (task-schedule-loop)
  (let ((scheduler (create-task-scheduler #t)))
    (let loop () (scheduler) (loop))))

(define (current-task-period)
  (if current-task (task-period current-task)
      (error "call to current-task-period outside of task")))
