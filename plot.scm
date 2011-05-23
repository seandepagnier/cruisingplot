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
;
;  Otherwise normal lisp expressions are allowed

(declare (unit plot))
(declare (uses history computation utilities options))

(use srfi-1 srfi-69 gl glu glut)

(include "glshortcuts.scm")

(define plots '())

; an axis is a list with 3 thunks that calculate the value, the min and the max
(define (parse-color value)
  (case value
    ((white) '(1 1 1))    ((yellow) '(1 1 0))    ((green) '(0 1 0))
    ((blue) '(0 0 1))     ((red) '(1 0 0))       ((magenta) '(1 0 1))
    ((cyan) '(0 1 1))     ((gray) '(.5 .5 .5))   ((orange) '(1 .5 0))
    ((black) '(0 0 0))
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

(define (default-plot-options)
  (create-options
   (list
     (make-bounds-verifier)
     (make-number-verifier 'fov "field of view, angle in degrees for 3d projection" 90 0 180)
     (make-number-verifier 'gridcount "set number of grid lines" 6 0 100)
     (make-number-verifier 'period "how often to update this trace in seconds" .25 0 1000000)
     (make-boolean-verifier 'gridnumbering "enable or disable grid numbering" 'true)
     (make-number-verifier 'framerate "rate to render te plot" 4 .001 1000)
     (make-number-verifier 'timeout "how long to keep data in history, 0 for forever" 0 0 100000000)
     (make-discrete-verifier 'type "type of plot" '2d '(2d 3d polar)))
   "no examples yet"
   #f))

(define (default-trace-options default-color plot-options)
  (create-options
   (list
    (make-color-verifier default-color)
    (make-number-verifier 'thickness "how fat to draw" 2 0 100)
    (make-discrete-verifier 'mode "plot mode" 'lines '(points lines)))
   "no examples yet"
   plot-options))

; a trace is like the plot of one variable, it renders each time invoked
(define (create-trace options axes)
  (case (+ (length axes) (if (eq? (options 'type) '3d) 1 0))
    ((1) (set! axes (cons 'time axes))) ; add time by default
    ((2) #t) ; we are ok
    (else (error "invalid axis count for plot" (length axes))))

  (let ((history (create-history)))
    (let ((computations (apply computations-revaluate axes)))
      (create-periodic-task
       `(trace-update ,axes)
       (options 'period)
       (lambda ()
         (let ((c (computations))
               (type (options 'type)))
           (if c (history 'update
                          (case type
                            ((2d 3d) c)
                            ((polar) `(,(* (first c) (sin (deg2rad (second c))))
                                       ,(* (first c) (cos (deg2rad (second c))))))
                            (else (error "unknown plot type" type)))))))
       ))
    (lambda (op . args)
      (case op
        ((axis-count) (length axes))
        ((bounds) (zip (history 'min) (history 'max)))
        ((options) options)
        ((apply-timeout) (if (> (options 'timeout) 0) (history 'apply-timeout (options 'timeout))))
        ((display) ; draw using opengl

         ; hack to avoid thick points and lines wrapping to the left side of screen
         (glLetMatrix
          (gl:LoadIdentity)
          (gl:ClipPlane gl:CLIP_PLANE0
                        (f64vector -1 0 0
                                   (- 1 (/  (- (options 'thickness) 1)
                                            (glut:Get glut:WINDOW_WIDTH))))))

         (gl:Enable gl:CLIP_PLANE0)


         (apply glColor (options 'color))
         (glBegin (case (options 'mode)
                    ((points) (gl:PointSize (options 'thickness)) gl:POINTS)
                    ((lines) (gl:LineWidth (options 'thickness)) gl:LINE_STRIP)
                    (else (error "unknown plot mode" mode)))
                  (for-each
                   (lambda (values)
                     (apply glVertex values))
                   (history 'dump)))

         (gl:Disable gl:CLIP_PLANE0))))))

; take take min, max pairs, and give overall min max
(define (bounds-union . bounds)
  (list (apply min (map first bounds))
        (apply max (map second bounds))))

(define (find-plot-bounds defaultbounds traces)
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

  (let ((bounds (find-plot-bounds (options 'bounds) traces)))
    (if (>= (length bounds) 2)
        (let ((left (first (first bounds)))
              (right (second (first bounds)))
              (top (first (second bounds)))
              (bottom (second (second bounds)))
              (near (if (< (length bounds) 3) -1 (first (third bounds))))
              (far (if (< (length bounds) 3) 1 (second (third bounds)))))
          
          (gl:MatrixMode gl:MODELVIEW)
          (gl:LoadIdentity)

          (gl:Ortho left right top bottom near far)

            ; Draw the grid lines
          (gl:Enable gl:LINE_STIPPLE)
          (gl:LineStipple 1 17)
          (gl:LineWidth 2)
          
          (case (((first traces) 'options) 'type)
            ((2d 3d)
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
                               (glutPrint (round-to-places offset 3))))
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
                               (glutPrint (round-to-places offset 3))))
                        (each-vgridline (+ offset vspacing)))))))
            ((polar)
             (let ((rspacing (/ (sqrt (+ (square (- bottom top))
                                         (square (- right left))))
                                (+ (options 'gridcount) 1)))
                   (aspacing (/ (* 2 pi) (options 'gridcount)))
                   (number-angle (atan (+ top bottom) (+ left right))))
               (let each-rgridline ((offset (sqrt (+ (if (negative? (* top bottom))
                                                         0
                                                         (min (square top) (square bottom)))
                                                     (if (negative? (* left right))
                                                         0
                                                         (min (square left) (square right))))))
                                    (rcount 0))
                 (cond ((<= rcount (options 'gridcount))
                        (glColor .6 .6 .6)
                        (glBegin gl:LINE_LOOP
                                 (let each-theta ((theta 0))
                                   (cond ((< theta (* 2 pi))
                                          (glVertex (* offset (sin theta)) (* offset (cos theta)))
                                          (each-theta (+ theta .1))))))

                        (cond ((options 'gridnumbering)
                               (glColor 1 1 1)
                               (glRasterPos (* offset (cos number-angle))
                                            (* offset (sin number-angle)))
                               (glutPrint (round-to-places offset 3))))
                        (each-rgridline (+ offset rspacing) (+ rcount 1)))))

               (let each-agridline ((offset 0))
                   (cond ((< offset (* 2 pi))
                          (glColor .6 .6 .6)
                          (glBegin gl:LINES
                                          (glVertex 0 0)

                                          (let ((max-r (sqrt (max
                                                              (+ (square left) (square top))
                                                              (+ (square right) (square top))
                                                              (+ (square left) (square bottom))
                                                              (+ (square right) (square bottom))))))
                                          (glVertex (* max-r (cos offset)) (* max-r (sin offset)))))
                          (each-agridline (+ offset aspacing))))))))

             (gl:Disable gl:LINE_STIPPLE)
          
          (gl:Translatef 0 0 (- near))
                                        ; Draw the traces

          (for-each (lambda (trace) (trace 'display)) traces)

  ; Do any additional plot rendering
          (for-each (lambda (display) (display left right top bottom)) plot-extra-display)

          ))))

(define plot-extra-display '())

(define (plot-add-extra-display-function thunk)
  (set! plot-extra-display (cons thunk plot-extra-display)))

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
   (lambda ()
       (for-each (lambda (trace) (trace 'apply-timeout)) traces)
       (plot-display options traces)
       (glut:SwapBuffers)))
 
  (let ((win (glut:GetWindow)))
    (create-periodic-task "plot redraw task" (/ (options 'framerate))
                          (lambda () (glut:PostWindowRedisplay win))))

    (glut:KeyboardFunc
     (lambda (key x y)
       (case key
         ((#\esc #\q) (exit))
         ((#\f) (glut:FullScreenToggle)))
       (glut:PostRedisplay)))
    
    (glut:SpecialFunc
     (lambda (key x y)
       (if (= (bitwise-and (glut:GetModifiers) glut:ACTIVE_SHIFT) glut:ACTIVE_SHIFT)
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
  
(define (create-plot-from-string arg)
  (let ((plot-options (default-plot-options))
        (split-arg (string-split arg ";"))
        (trace-colors '(white red green blue yellow cyan magenta gray orange)))
    (if (> (length split-arg) (length trace-colors))
        (error "too many traces specified for plot"))

    (create-plot
     plot-options
     (map (lambda (trace-string default-color)
            (let ((traceoptions (default-trace-options default-color plot-options)))
              (create-trace
               traceoptions
               (map (lambda (axis-string)
                      (read-from-string (parse-basic-arg-options-string traceoptions axis-string)))
                    (string-split trace-string ":")))))
          split-arg trace-colors))))

(define glutMainWindow #f)

(define (plots-setup plots)
  (glut:InitDisplayMode (+ glut:DOUBLE glut:RGB glut:ALPHA))
  (set! glutMainWindow (glut:CreateWindow "cruisingplot"))
  (if (= (length plots) 1) 
      ((car plots))
      (let* ((plot-count (length plots))
             (plots-x-count (ceiling (sqrt plot-count)))
             (plots-y-count (ceiling (/ plot-count plots-x-count))))
        (let ((plots
               (let each-plot ((plots plots) (x 0) (y 0))
                 (if (null? plots) '()
                     (cons (list (let ((win (glut:CreateSubWindow glutMainWindow 0 0 64 64)))
                                   ((car plots))
                                   win)
                                 x y)
                           (if (< x (- plots-x-count 1))
                               (each-plot (cdr plots) (+ x 1) y)
                               (each-plot (cdr plots) 0 (+ y 1))))))))
          
          (glut:SetWindow glutMainWindow)
  
          (glut:DisplayFunc (lambda () #t))

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
                       plots)))))))

;  This is a bug workaround, for some reason glut:IdleFunc is never
;  called with multiple sub windows, and the only way to get it to work
;  is to create a thread for the glut main loop.  With only 1 plot we
;  can avoid creating threads all together

  (if (= (length plots) 1) 
      (let ((scheduler (create-task-scheduler #t)))
        (glut:IdleFunc (lambda ()
                         (scheduler)
                         ))
        (glut:MainLoop)))

  (glut:IdleFunc (lambda () (thread-sleep! .01)))
  (thread-start! glut:MainLoop))
