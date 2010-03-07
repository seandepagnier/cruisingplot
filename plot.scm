;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

; plotting makes use of simple math expressions
; the expressions are evaluated with the scheme eval function
; and are used to calculate values of the data,
; as well as the new minimum and maximum boundaries needed.
; The special variables are given:
;  T - current time
;  (data 'data-key 'field)  Calculates the value as needed
;     eg:  (wind speed)n
;  Otherwise normal lisp constructs are allowed

(use srfi-1 srfi-69 gl glu glut)

; an axis is a list with 3 thunks that calculate the value, the min and the max
(define (parse-color value)
  (case value
    ((white) '(1 1 1))    ((yellow) '(1 1 0))    ((green) '(0 1 0))
    ((blue) '(0 0 1))     ((red) '(1 0 0))       ((magenta) '(1 0 1))
    ((cyan) '(0 1 1))     ((gray) '(.5 .5 .5))   ((black) '(0 0 0))
    (else (if (and (list? value) (= 3 (length value)))
              value
              (error "Unrecognized color" value)))))

(define (glutPrint . args)
  (for-each (lambda (c)
              (glut:BitmapCharacter glut:BITMAP_9_BY_15 c))
            (string->list
             (apply string-append (map (lambda (arg)
                                         (with-output-to-string
                                           (lambda () (display arg))))
                                       args)))))

(define (round-decimals value places)
  (let ((f (expt 10 places)))
    (/ (round (* value f)) f)))

;  Verifiers for plot and trace options
;  these store the name, default value, and a method to verify a new value
;  and update it
(define (make-verifier name default verify)
  (lambda (op)
    (case op
      ((name) name)
      ((default) (verify default))
      ((verify) verify))))

(define (make-number-verifier name default min max)
  (make-verifier
   name default
   (lambda (num)
     (cond ((< num min) (print "Value given for option '" name "' is below minimum of " min) min)
           ((> num max) (print "Value given for option '" name "' is above maximum of " max) max)
           (else num)))))

(define (make-discrete-verifier name default validvalues)
  (make-verifier
   name default
   (lambda (value)
     (if (member value validvalues)
         value
         (die "value " value "for option " name
              "invalid.  Valid values include:\n" validvalues)))))

(define (make-boolean-verifier name default)
  (make-verifier
   name default
   (lambda (value)
     (cond ((boolean? value) value)
           ((eq? value 'true) #t)
           ((eq? value 'false) #f)
           (else (error "boolean option " name " must be either true or false\n"))))))

(define (make-color-verifier default)
  (make-verifier 'color default parse-color))

(define (make-bounds-verifier)
  (make-verifier 'bounds '()
                 (lambda (value)
                   (cond ((eq? value 'auto) '())
                         (else value)))))

(define (create-options verifiers parent-options)
  (let ((values (alist->hash-table
                 (map (lambda (verifier) (cons (verifier 'name) (verifier 'default))) verifiers)))
        (verifiers (alist->hash-table
                    (map (lambda (verifier) (cons (verifier 'name) (verifier 'verify))) verifiers))))
    (lambda (op . args)
      (if (eq? op 'update)
          (let ((value (if (null? (cdr args)) 'true (second args))))
            (cond ((hash-table-exists? values (first args))
                   (hash-table-set! values (first args)
                                    ((hash-table-ref verifiers (first args)) value)))
                  (parent-options (apply parent-options op args))
                  (else (error "cannot update unrecognized option" (car args)
                               (hash-table-ref values op)))))
          (if (hash-table-exists? values op)
              (hash-table-ref values op)
              (error "unrecognized option" op))))))

(define (default-plot-options)
  (create-options
   (list
     (make-bounds-verifier)
     (make-number-verifier 'fov 90 0 180)
     (make-number-verifier 'gridcount 5 0 100)
     (make-boolean-verifier 'gridnumbering 'true)
     (make-number-verifier 'framerate 4 .001 1000))
   #f))

(define (default-trace-options default-color plot-options)
  (create-options
   (list
    (make-color-verifier default-color)
    (make-number-verifier 'period 1 0 1000000)
    (make-number-verifier 'thickness 2 0 100)
    (make-number-verifier 'timeout 0 0 100000000)
    (make-discrete-verifier 'type '2d '(2d 3d polar))
    (make-discrete-verifier 'mode 'lines '(points lines)))
   plot-options))

; a trace is like the plot of one variable, it renders each time invoked
(define (create-trace options axes)
  (case (+ (length axes) (if (eq? (options 'type) '3d) 1 0))
    ((1) (set! axes (cons 'time axes))) ; add time by default
    ((2) #t) ; we are ok
    (else (error "invalid axis count for plot" (length axes))))

  (let ((history (create-history)))
    (thread-start! (lambda ()
                     (let ((computations (apply computations-revaluate axes)))
                       (let loop ()
                         (history 'update
                                  (let ((c (computations))
                                        (type (options 'type)))
                                  (case type
                                    ((2d 3d) c)
                                    ((polar) `(,(* (first c) (cos (second c)))
                                               ,(* (first c) (sin (second c)))))
                                    (else (error "unknown plot type" type)))))
                         (thread-sleep! (options 'period))
                         (loop)))))
    (lambda (op . args)
      (case op
        ((axis-count) (length axes))
        ((bounds) (zip (history 'min) (history 'max)))
        ((options) options)
        ((display) ; draw using opengl
         (glBegin (case (options 'mode)
                    ((points) (gl:PointSize (options 'thickness)) gl:POINTS)
                    ((lines) (gl:LineWidth (options 'thickness)) gl:LINE_STRIP)
                    (else (error "unknown plot mode" mode)))
                  (for-each
                   (lambda (values)
                     (apply gl:Color3f (options 'color))
                     (apply glVertex values))
                   (history 'dump)))
         (if (> (options 'timeout) 0)
             (history 'apply-timeout (options 'timeout))))))))

; take take min, max pairs, and give overall min max
(define (bounds-union . bounds)
  (list (apply min (map first bounds))
        (apply max (map second bounds))))

(define (find-plot-bounds defaultbounds traces)
;         (print "bounds " defaultbounds traces)

  (let ((axis-count ((car traces) 'axis-count)))
    (let each-bound ((defaultbounds defaultbounds)
                     (tracebounds (remove null? (map (lambda (trace) (trace 'bounds)) traces))))
      (cond ((or (null? tracebounds) (null? (car tracebounds))) '())
            ((null? defaultbounds)
             (cons (apply bounds-union (map car tracebounds))
                   (each-bound '() (map cdr tracebounds))))
            ((or (null? (car defaultbounds)) (eq? (car defaultbounds) 'auto))
             (cons (apply bounds-union (map car tracebounds))
                   (each-bound (cdr defaultbounds) (map cdr tracebounds))))
            (else
             (cons (car defaultbounds)
                   (each-bound (cdr defaultbounds) (map cdr tracebounds))))))))

(define (plot-display options traces)
  (gl:Clear gl:COLOR_BUFFER_BIT)

   ; update bounds and set the modelview matrix
  (gl:MatrixMode gl:MODELVIEW)
  (gl:LoadIdentity)
  
  (let ((bounds (find-plot-bounds (options 'bounds) traces)))
    (if (>= (length bounds) 2)
        (let ((left (first (first bounds)))
              (right (second (first bounds)))
              (top (first (second bounds)))
              (bottom (second (second bounds)))
              (near (if (< (length bounds) 3) -1 (first (third bounds))))
              (far (if (< (length bounds) 3) 1 (second (third bounds)))))
          
          (gl:LoadIdentity)
          (gl:Ortho left right top bottom near far)
          
            ; Draw the grid lines
          (gl:Enable gl:LINE_STIPPLE)
          (gl:LineStipple 1 17)
          
          (let ((hspacing (/ (- right left) (+ 1 (options 'gridcount)))))
            (let each-hgridline ((offset (+ left hspacing)))
              (cond ((< offset (- right (/ hspacing 10)))
                     (glColor .6 .6 .6)
                     (glBegin gl:LINES
                              (glVertex offset top)
                              (glVertex offset bottom))
                     (cond ((options 'gridnumbering)
                            (glColor 1 1 1)
                            (glRasterPos offset (/ (+ bottom (* 99 top)) 100))
                            (glutPrint (round-decimals offset 3))))
                     (each-hgridline (+ offset hspacing))))))

          (let ((vspacing (/ (- bottom top) (+ 1 (options 'gridcount)))))
            (let each-vgridline ((offset (+ top vspacing)))
              (cond ((< offset (- bottom (/ vspacing 10)))
                     (glColor .6 .6 .6)
                     (glBegin gl:LINES
                              (glVertex left offset)
                              (glVertex right offset))
                     (cond ((options 'gridnumbering)
                            (glColor 1 1 1)
                            (glRasterPos (/ (+ (* 99 left) right) 100)
                                         (- offset (/ vspacing 10)))
                            (glutPrint (round-decimals offset 3))))
                     (each-vgridline (+ offset vspacing))))))
          
          (gl:Disable gl:LINE_STIPPLE)
          
          (gl:Translatef 0 0 (- near)))))
  
   ; Draw the traces
  (for-each (lambda (trace) (trace 'display)) traces))

; the plot may have multiple instances of various axes, therefore a list
; which of lists of axes is used.  Options for the plot for the first element
; are first

(define (create-plot options traces)
  (glut:ReshapeFunc
   (lambda (w h)
     (gl:Viewport 0 0 w h)
         ; setup projection matrix
     (gl:MatrixMode gl:PROJECTION)
     (gl:LoadIdentity)
         ; if we have any traces that are 3d
     (if (any (lambda (trace) (eq? ((trace 'options) 'type) '3d)) traces)
         (glu:Perspective (options 'fov) (/ w h) .1 100))))

    (glut:DisplayFunc
     (let ((last-frame-time (current-milliseconds))
           (period (round (/ 1000 (options 'framerate)))))
       (lambda ()
         (let ((frame-time (current-milliseconds)))
           ; redisplay again at the framerate
           (cond ((> frame-time (+ last-frame-time (* 2 period)))
                  (warning-once "system too SLOW to render at "
                                "the framerate set for plot")
                  (set! last-frame-time frame-time))
                 ((> frame-time (+ last-frame-time period))
                  (set! last-frame-time (+ last-frame-time period))
                  (plot-display options traces)
                  (glut:SwapBuffers)))

           (glut:TimerFunc (- (+ last-frame-time period) frame-time)
                           glut:PostWindowRedisplay
                           (glut:GetWindow))))))

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

(define (plot-help)
  (print "Plot setup in the form:  TRACE0[!TRACE1]...[!TRACEn][,option0]...[,optionn]\n"
         "options for type of plot include:\n"
         "     bounds      minimum and maximum values for each axis (rectangular), eg:\n"
         "                 ((-10 10) (-5 5))     (default auto)\n"
         "     fov         field of view, angle in degrees for 3d projection (default 90)\n"
         "     gridcount             set number of grid lines (default 5)\n"
         "     gridnumbering         enable or disable grid numbering\n"
         "     framerate             rate to render the plot (default 4)\n"
         "The form for TRACE is:\n"
         "  AXIS0[:AXIS1]...[:AXISn][,option0=value][,option1=value]...[optionn=value]\n"
         "     color -- can be by name, eg:\n"
         "            red green blue cyan magenta\n"
         "            yellow gray black white (default white)\n"
         "            or can be a normalized triple, for red:\n"
         "            (1 0 0)\n"
         "     period -- how often to update this trace in seconds (default 1)\n"
         "     type             2d (default), 3d, polar\n"
         "     mode     points or lines (default lines)\n"
         "     thickness   -- how fat to draw (default 2)\n"
         "     timeout               how long to keep data in history, 0 for never (default 0)\n"
         "Example of windspeed plot:\n"
         "  --plot windspeed\n"
         "Here, only 1 axis is given, so time is automatically added.\n"
         "Another example:\n"
         "  --plot speedintowind,color=yellow;speed,color=blue\n"
         "Some more examples:\n"
         "-p speedintowind:winddirection,mode=points;heading,mode=points,type=polar\n"))
  
(define (create-plot-from-string arg)
  (if (eq? arg "help") (plot-help)
      (let ((plotoptions (default-plot-options)))
        (create-plot
         plotoptions
         (map (lambda (trace-string default-color)
                (let ((traceoptions (default-trace-options default-color plotoptions)))
                  (create-trace
                   traceoptions
                   (map (lambda (axis-string)
                          (let ((options-string (string-split axis-string ",")))
                            (for-each (lambda (option)
                                        (apply traceoptions 'update
                                               (map read-from-string (string-split option "="))))
                                      (cdr options-string))
                            (read-from-string (car options-string))))
                        (string-split trace-string ":")))))
              (string-split arg ";")
              '(white red green blue yellow cyan magneta gray))))))

(define glutMainWindow #f)

(define (plots-setup args)
  (set! glutMainWindow (glut:CreateWindow "cruisingplot"))
  (if (= (length args) 1) 
      (create-plot-from-string (car args))
      (let* ((plot-count (length args))
             (plots-x-count (ceiling (sqrt plot-count)))
             (plots-y-count (ceiling (/ plot-count plots-x-count))))
        (let ((plots
               (let each-plot ((args args) (x 0) (y 0))
                 (if (null? args) '()
                     (cons (list (let ((win (glut:CreateSubWindow glutMainWindow 0 0 64 64)))
                                   (create-plot-from-string (car args))
                                   win)
                                 x y)
                           (if (< x (- plots-x-count 1))
                               (each-plot (cdr args) (+ x 1) y)
                               (each-plot (cdr args) 0 (+ y 1))))))))
          
          (glut:SetWindow glutMainWindow)
          
          (glut:DisplayFunc
           (lambda ()
             (gl:Clear gl:COLOR_BUFFER_BIT)
             (glut:SwapBuffers)))
          
          (glut:ReshapeFunc
           (lambda (w h)
             (gl:Viewport 0 0 w h)
             (let ((x-spacing (/ w plots-x-count))
                   (y-spacing (/ h plots-y-count)))
               (for-each (lambda (plot)
                           (glut:SetWindow (first plot))
                           (glut:PositionWindow (* (second plot) x-spacing)
                                                (* (third plot) y-spacing))
                           (glut:ReshapeWindow x-spacing y-spacing))
                       plots))))))))
