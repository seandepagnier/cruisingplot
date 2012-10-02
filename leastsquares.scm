;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version. 

(declare (unit leastsquares))
(declare (uses matrix vector))

; return (HH')^-1*H'Z'
(define (least-squares-apply H Z)
  (let ((i (matrix* (matrix-transpose H) H)))
    (let ((ii (matrix-inverse i)))
      (if ii
          (matrix* ii (matrix-transpose H) Z)
          (zeroed-matrix (matrix-rows i) (matrix-cols i))))))

; the functions build build-jacobian-row takes state and measurements
(define (compute-least-squares state build-jacobian-residual-row measurements)
  (let ((HZ (map (lambda (measurement)
                   (build-jacobian-residual-row state measurement))
                 measurements)))    
    (list (matrix (map car HZ)) (matrix (map cdr HZ)))))

(define (least-squares-iterate initial-state build-jacobian-residual-row measurements
                               complete? max-iterations)
  (let each-iteration ((state initial-state) (iteration 0))
    (let*((HZ (compute-least-squares state build-jacobian-residual-row measurements))
          (updates (first (matrix->list (matrix-transpose (apply least-squares-apply HZ)))))
          (nstate (map + state updates))          
          (update (vector-magnitude updates)))
      (cond ((or (> iteration max-iterations)
                 (and complete? (complete? nstate update)))
             (list nstate (vector-magnitude
                           (first (matrix->list (matrix-transpose (second HZ)))))))
            (else (each-iteration nstate (+ iteration 1)))))))

; least squares to fit data
; use y = a
(define (constant-regression data)
    (least-squares-iterate '(1) (lambda (state measurement)
                             (list (list 1) ; derivative
                                   (-  (second measurement) (first state)))) ;residual
                                  data 1))

; use y = a*x + b
(define (linear-regression data)
    (least-squares-iterate '(1 1) (lambda (state measurement)
                                    (list (list (first measurement) 1)
                                          (-  (second measurement)
                                              (* (first state) (first measurement))
                                              (second state))))
                           data #f 1))

; use y = a*x^2 + b*x + c
(define (quadratic-regression data)
  (least-squares-iterate
   (cons 1 (first (linear-regression data)))
   (lambda (state measurement)
     (let ((j (list (square (first measurement)) (first measurement) 1)))
              (list j (- (second measurement)
                         (vector-dot j state)))))
   data #f 10))

;y = a*x^3 + b*x^2 + c*x + d
(define (cubic-regression data)
  (least-squares-iterate
   (cons 1 (first (quadratic-regression data)))
   (lambda (state measurement)
     (let ((j (list (cube (first measurement)) (square (first measurement)) (first measurement) 1)))
              (list j (-  (second measurement)
                          (vector-dot j state)))))
   data #f 10))

; y = a + b*x + c*x^2 + ...
(define (polynomial-regression data max-order)
  (let each-order ((order 0) (last-state '()))
    (cond ((<= order max-order)
           (let ((state
                  (least-squares-iterate
                   (cons 1 last-state)
                   (lambda (state measurement)
                     (let ((fm (first measurement)))
                       (let ((j (cons 1 (let build-j ((o 1) (m fm))
                                          (cond ((<= o order)
                                                 (cons m (build-j (+ o 1) (* m fm))))
                                                (else '()))))))
                         (list j (- (second measurement)
                                    (vector-dot j state))))))
                     data #f 10)))
             (cond (state
                    (display "order ")
                    (write order)
                    (display " state ")
                    (for-each (lambda (n)
                                (display-scientific-notation-places n 3)
                                (display " "))
                              (first state))
                    (display (string->symbol "residual "))
                    (display-scientific-notation-places (second state) 6)
                    (newline)
                    (each-order (+ 1 order) (first state)))
                   (else
                    (print "failed to compute least squares after order " order))))))))

; calibrate x and y offsets as well as radius
; best fit circle to data
;
; measurements: x y
; states: yb zb (bias) s (scale)
;
; truth equation:
; (x-xb)^2 + (y-yb)^2 = s^2
;
(define (calibrate-biases-and-scale-2d measurements)
  (least-squares-iterate
   `(,(average-list (map first measurements))
     ,(average-list (map second measurements))
     1)
   (lambda (state measurement)
     (let-values (((xb yb s) (apply values state))
                  ((x y) (apply values measurement)))
       (let ((xs (- x xb)) (ys (- y yb)))
       (list (list (* 2 xs)
                   (* 2 ys)
                   (* 2 s))
             (-  (+ (square xs) (square ys)) (square s))))))
   measurements #f 10))

; measurements: x y
; states: xb yb (biase) s (scale) yrs (y relative scale)
; (x-xb)^2 + (yrs*(y-yb))^2 = s^2
;
; fit non-rotated ellipse to data
;
; convert yrs to sqrt on return
(define (calibrate-biases-scale-and-relative-scale-2d measurements)
  (least-squares-iterate
   `(,(average-list (map first measurements))
     ,(average-list (map second measurements))
     1 1)
   (lambda (state measurement)
     (let-values (((xb yb s yrs) (apply values state))
                  ((x y) (apply values measurement)))
       (let ((xs (- x xb)) (ys (- y yb)))
       (list (list (* 2 xs)
                   (* 2 (* (square yrs) ys))
                   (* 2 s)
                   (- (* 2 yrs (square ys))))
             (-  (+ (square xs) (square (* yrs ys))) (square s))))))
   measurements #f 10))

; this equation calibrates biases and relative scales
; calibrate x, y and z offsets as well as radius
;
; measurements: x y z
; states: xb yb zb (bias) s (scale)
;
; truth equation:
; (x-xb)^2 + (y-yb)^2 + (z-zb)^2 - s^2 = 0
;
(define (calibrate-biases-and-scale-3d measurements)
  (least-squares-iterate
   `(,(average-list (map first measurements))
     ,(average-list (map second measurements))
     ,(average-list (map third measurements))
     1)
   (lambda (state measurement)
     (let-values (((xb yb zb s) (apply values state))
                  ((x y z) (apply values measurement)))
       (let ((xs (- x xb)) (ys (- y yb)) (zs (- z zb)))
       (list (list (* 2 xs)
                   (* 2 ys)
                   (* 2 zs)
                   (* 2 s))
             (- (+ (square xs) (square ys) (square zs)) (square s))))))
   measurements #f 20))


(define (constrain-states! state constraints)
  (cond ((not (= (length state) (length constraints)))
         (error "number of constraints does not match number of states"))
        ((not (null? state))
         (cond ((< (car state) (caar constraints))
                (set-car! state (caar constraints)))
               ((> (car state) (cadar constraints))
                (set-car! state (cadar constraints))))
         (constrain-states! (cdr state) (cdr constraints)))))


; measurements: x y z
; states: xb yb zb (biase) s (scale) yrs zrs (relative scales)
; (x-xb)^2 + (yrs*(y-yb))^2 + (zrs*(z-zb))^2 = s^2
;
; fit non-rotated ellipse to data
;
(define (calibrate-biases-scale-and-relative-scales-3d measurements)
  (least-squares-iterate
   (let ((lc (calibrate-biases-and-scale-3d measurements)))
     (if lc
         (append (first lc) '(1 1))
         '(0 0 0 1 1 1)))
   (lambda (state measurement)
     (let-values (((xb yb zb s yrs zrs) (apply values state))
                  ((x y z) (apply values measurement)))
       (let ((xs (- x xb)) (ys (- y yb)) (zs (- z zb)))
       (list (list (* 2 xs)
                   (* 2 yrs ys)
                   (* 2 zrs zs)
                   (* 2 s)
                   (- (square ys))
                   (- (square zs)))
             (-  (+ (square xs) (* yrs (square ys)) (* zrs (square zs))) (square s))))))
   measurements (lambda (state update)
                  (constrain-states! state '((-2 2) (-2 2) (-2 2)
                                             (.4 2) (.4 2) (.4 2)))
                  #f)
   20))


; measurements: x y z
; states: xb yb zb (bias) s (scale) yrs zrs (relative scales) xyc xzc yzc (cross coupling)
; (x-xb)^2 + yrs*((y-yb) + xyc*(x-xb))^2 + zrs*((z-zb) + xzc*(x-xb) + yzc*(y-yb))^2 - s^2 = 0
;
; fit rotated ellipse to data
;
(define (calibrate-biases-scale-relative-scales-and-cross-coupling-3d measurements)
  (least-squares-iterate
   (let ((lc (calibrate-biases-scale-and-relative-scales-3d measurements)))
     (if lc
         (append (first lc) '(0 0 0))
         '(0 0 0 1 1 1 0 0 0)))
   (lambda (state measurement)
     (let-values (((xb yb zb s yrs zrs xyc xzc yzc) (apply values state))
                  ((x y z) (apply values measurement)))
       (let ((xs (- x xb)) (ys (- y yb)) (zs (- z zb)))
         (let ((xc xs)
               (yc (+ ys (* xyc xs)))
               (zc (+ zs (* xzc xs) (* yzc ys))))
           (list (list (* 2 (+ xc (* xyc yrs yc) (* xzc zrs zc)))
                       (* 2 (+ (* yrs yc) (* yzc zrs zc)))
                       (* 2 zrs zc)
                       (* 2 s)
                       (- (square ys))
                       (- (square zs))
                       (* -2 xs yc yrs)
                       (* -2 xs zc zrs)
                       (* -2 ys zc zrs))
                 (- (+ (square xc) (* yrs (square yc)) (* zrs (square zc)))
                    (square s)))))))
   measurements (lambda (state update)
                  (constrain-states! state '((-2 2) (-2 2) (-2 2)
                                             (.4 2) (.4 2) (.4 2)
                                             (-.5 .5) (-.5 .5) (-.5 .5)))
                  #f)
   20))

; measurements: x y z
; states: a - f
;
; cx = a*x + b
; cy = c*y + d
; cz = e*z + f

(define (calibrate-basic-sensor-3d measurements)
  (define avgx (average-list (map first measurements)))
  (define avgy (average-list (map second measurements)))
  (define avgz (average-list (map third measurements)))

  (least-squares-iterate
   (list 1 (- avgx) 1 (- avgy) 1 (- avgz))
   (lambda (state measurement)
     (let-values (((a b c d e f) (apply values state))
                  ((ax ay az) (apply values measurement)))
       (let ((cax (+ (* a ax) b)) (cay (+ (* c ay) d)) (caz (+ (* e az) f)))
             (list (list (* 2 ax cax) (* 2 cax)
                         (* 2 ay cay) (* 2 cay)
                         (* 2 az caz) (* 2 caz))
                   (- 1 (square cax) (square cay) (square caz))))))
   measurements
   (lambda (state update)
     (constrain-states! state '((.8 1.2) (-1 1) (.8 1.2) (-1 1) (.8 1.2) (-1 1)))
     #f)
   20))

; measurements: x y z
; states: a - i
;
; cx = a*x + b
; cy = c*x + d*y + e
; cz = f*x + g*y + h*z + i

(define (calibrate-sensor-3d measurements)
  (least-squares-iterate
   (let ((basic-cal (calibrate-basic-sensor-3d measurements)))
     (let ((b (if (< (second basic-cal) 1) (first basic-cal) '(1 0 1 0 1 0))))
       (list (first b)                     (second b)
                     0 (third b)           (fourth b)
                     0         0 (fifth b) (sixth b))))
   (lambda (state measurement)
     (let-values (((a b c d e f g h i) (apply values state))
                  ((x y z) (apply values measurement)))
       (let ((cx (+ (* a x)                 b))
             (cy (+ (* c x) (* d y)         e))
             (cz (+ (* f x) (* g y) (* h z) i)))
            (list (list (* 2 x cx)                       (* 2 cx)
                        (* 2 x cy) (* 2 y cy)            (* 2 cy)
                        (* 2 x cz) (* 2 y cz) (* 2 z cz) (* 2 cz))
                      (- 1 (square cx) (square cy) (square cz))))))
   measurements
   (lambda (state update)
     (constrain-states! state '((.8 1.2)                   (-1 1)
                                (-.2 .2) (.8 1.2)          (-1 1)
                                (-.2 .2) (-.2 .2) (.8 1.2) (-1 1)
                                ))
     #f)
   20))

; measurements: x y z
; states: a - o
;
; rx = j*x^3 + k*x^2 + x
; ry = l*y^3 + m*y^2 + y
; rz = n*z^3 + o*z^2 + z
;
; cx = a*rx + b
; cy = c*rx + d*ry + e
; cz = f*rz + g*ry + h*rz + i
;
; cx = a*j*x^3 + a*k*x^2 + a*x + b
; cy = c*j*x^3 + c*k*x^2 + c*x + d*l*y^3 + d*m*y^2 + d*y + e
; cz = f*j*x^3 + f*k*x^2 + f*x + g*l*y^3 + g*m*y^2 + g*y + h*n*z^3 + h*o*z^2 + h*z + i
;

(define (calibrate-sensor-3rd-order-3d measurements)
  (least-squares-iterate
   (append (let ((linear-cal (calibrate-sensor-3d measurements)))
             (first linear-cal))
           (list 0 0 0 0 0 0))
   (lambda (state measurement)
     (let-values (((a b c d e f g h i j k l m n o) (apply values state))
                  ((x y z) (apply values measurement)))
       (let ((rx (+ (* j (cube x)) (* k (square x)) x))
             (ry (+ (* l (cube y)) (* m (square y)) y))
             (rz (+ (* n (cube z)) (* o (square z)) z)))
         (let ((cx (+ (* a rx)                   b))
               (cy (+ (* c rx) (* d ry)          e))
               (cz (+ (* f rx) (* g ry) (* h rz) i)))
           (list (list (* 2 rx cx)                         (* 2 cx)
                       (* 2 rx cy) (* 2 ry cy)             (* 2 cy)
                       (* 2 rx cz) (* 2 ry cz) (* 2 rz cz) (* 2 cz)
                       (* 2 (cube x)   (+ (* f cz) (* c cy) (* a cx)))
                       (* 2 (square x) (+ (* f cz) (* c cy) (* a cx)))
                       (* 2 (cube y)   (+ (* g cz) (* d cy)))
                       (* 2 (square y) (+ (* g cz) (* d cy)))
                       (* 2 (cube z)         h cz)
                       (* 2 (square z)       h cz))
                 (- 1 (square cx) (square cy) (square cz)))))))
   measurements
   (lambda (state update)
     (constrain-states! state '((.8 1.2)                   (-1 1)
                                (-.2 .2) (.8 1.2)          (-1 1)
                                (-.2 .2) (-.2 .2) (.8 1.2) (-1 1)
                                (-.001 .001) (-.01 .01)
                                (-.001 .001) (-.01 .01)
                                (-.001 .001) (-.01 .01)
                                ))
     #f)
   20))

; measurements: ax ay az mx my mz
; states: a - s
;
; cax = a*ax + b
; cay = c*ay + d
; caz = e*az + f
;
; cmx = g*mx + h*my + i*mz + j
; cmy = k*mx + l*my + m*mz + n
; cmz = o*mx + p*my + q*mz + r
;
; cax^2 + cay^2 + caz^2 - 1 = 0
; cmx^2 + cmy^2 + cmz^2 - 1 = 0
; cmx*cax + cmy*cay + cmz*caz + s = 0
;

(define (apply-basic-accel-mag-3d accel mag calibration)
  (let-values (((a b c d e f g h i j k l m n o p q r s) (apply values (first calibration)))
               ((ax ay az) (apply values accel))
               ((mx my mz) (apply values mag)))

       (let ((cax (+ (* a ax) b)) (cay (+ (* c ay) d)) (caz (+ (* e az) f))
             (cmx (+ (* g mx) (* h my) (* i mz) j))
             (cmy (+ (* k mx) (* l my) (* m mz) n))
             (cmz (+ (* o mx) (* p my) (* q mz) r)))
         (list (list cax cay caz) (list cmx cmy cmz)))))


    
(define (calibrate-basic-accel-mag-3d measurements)
  (let ((initial-accel (calibrate-basic-sensor-3d
                        (map (lambda (measurement) (take measurement 3)) measurements)))
        (initial-mag (calibrate-basic-sensor-3d
                         (map (lambda (measurement) (take-right measurement 3)) measurements))))
  (least-squares-iterate
   (append
    (if (< (second initial-accel) 1)
        (let ((ia (first initial-accel)))
          ia)
        '(1 0 1 0 1 0))
    (if (< (second initial-mag) 1)
        (let ((im (first initial-mag)))
          (list (first im) 0 0 (second im) 0 (third im) 0 (fourth im) 0 0 (fifth im) (sixth im)))
        '(1 0 0 0  0 1 0 0  0 0 1 0))
    '(.9)
       )
   (lambda (state measurement)
     (let-values (((a b c d e f g h i j k l m n o p q r s) (apply values state))
                  ((index ax ay az mx my mz) (apply values measurement)))
       (let ((cax (+ (* a ax) b)) (cay (+ (* c ay) d)) (caz (+ (* e az) f))
             (cmx (+ (* g mx) (* h my) (* i mz) j))
             (cmy (+ (* k mx) (* l my) (* m mz) n))
             (cmz (+ (* o mx) (* p my) (* q mz) r)))
         (case index
           ((0) (list (list (* 2 ax cax) (* 2 cax)
                            (* 2 ay cay) (* 2 cay)
                            (* 2 az caz) (* 2 caz)
                            0 0 0 0 0 0 0 0 0 0 0 0 0
                            )
                      (- 1 (square cax) (square cay) (square caz))))
           ((1)
            (list (list 0 0 0 0 0 0
                            (* 2 mx cmx) (* 2 my cmx) (* 2 mz cmx) (* 2 cmx)
                            (* 2 mx cmy) (* 2 my cmy) (* 2 mz cmy) (* 2 cmy)
                            (* 2 mx cmz) (* 2 my cmz) (* 2 mz cmz) (* 2 cmz) 0
                            )
                  (- 1 (square cmx) (square cmy) (square cmz))))
           ((2) (let ((nca (normalize (list cax cay caz)))
                      (ncm (normalize (list cmx cmy cmz))))
                  (let ((ncax (first nca)) (ncay (second nca)) (ncaz (third nca))
                        (ncmx (first ncm)) (ncmy (second ncm)) (ncmz (third ncm)))
                  (list (list
                              (* ax cmx)                       cmx
                              (* ay cmy)                       cmy
                              (* az cmz)                       cmz
                              (* mx cax) (* my cax) (* mz cax) cax
                              (* mx cay) (* my cay) (* mz cay) cay
                              (* mx caz) (* my caz) (* mz caz) caz
                              1
                              )
                        (- (+ (* cax cmx) (* cay cmy) (* caz cmz) s))))))
           ((3) (let ((amag (+ (square cax) (square cay) (square caz)))
                      (mmag (+ (square cmx) (square cmy) (square cmz)))
                      (madot (+ (* cax cmx) (* cay cmy) (* caz cmz))))
                  (list (list
                         0 0 0 0 0 0
                              (* -2 ax (+ (* cax mmag s) (* cmx madot)))
                              (* -2    (+ (* cax mmag s) (* cmx madot)))
                              (* -2 ay (+ (* cay mmag s) (* cmy madot)))
                              (* -2    (+ (* cay mmag s) (* cmy madot)))
                              (* -2 az (+ (* caz mmag s) (* cmz madot)))
                              (* -2    (+ (* caz mmag s) (* cmz madot)))
                              (* -2 mx (+ (* cmx amag s) (* cax madot)))
                              (* -2 my (+ (* cmx amag s) (* cax madot)))
                              (* -2 mz (+ (* cmx amag s) (* cax madot)))
                              (* -2    (+ (* cmx amag s) (* cax madot)))
                              (* -2 mx (+ (* cmy amag s) (* cay madot)))
                              (* -2 my (+ (* cmy amag s) (* cay madot)))
                              (* -2 mz (+ (* cmy amag s) (* cay madot)))
                              (* -2    (+ (* cmy amag s) (* cay madot)))
                              (* -2 mx (+ (* cmz amag s) (* caz madot)))
                              (* -2 my (+ (* cmz amag s) (* caz madot)))
                              (* -2 mz (+ (* cmz amag s) (* caz madot)))
                              (* -2    (+ (* cmz amag s) (* caz madot)))
                              (* amag mmag)
                              )
                      (+ (square (+ (* cax cmx) (* cay cmy) (* caz cmz))) (* s amag mmag)))))))))
   (apply append (map (lambda (measurement)
                        (list (cons 0 measurement)
                              (cons 1 measurement)
                              (cons 2 measurement)
                              ))
                      measurements))
   (lambda (state update)
     (constrain-states! state '((.8 1.2) (-.2 .2) (.8 1.2) (-.2 .2) (.8 1.2) (-.2 .2)
                                (.8 1.2) (-.2 .2) (-.2 .2) (-1 1)
                                (-.2 .2) (.8 1.2) (-.2 .2) (-1 1)
                                (-.2 .2) (-.2 .2) (.8 1.2) (-1 1)
                                (-1 1)
                                ))
     #f)
   20)))

; measurements: ax ay az mx my mz
; states: a - v
;
; cax = a*ax + b
; cay = c*ax + d*ay + e
; caz = f*ax + g*ay + h*az + i
;
; cmx = j*mx + k*my + l*mz + m
; cmy = n*mx + o*my + p*mz + q
; cmz = r*mx + s*my + t*mz + u
;
; cax^2 + cay^2 + caz^2 - 1 = 0
; cmx^2 + cmy^2 + cmz^2 - 1 = 0
; cmx*cax + cmy*cay + cmz*caz + v = 0
;
(define (apply-accel-mag-3d accel mag calibration)
  (let-values (((a b c d e f g h i j k l m n o p q r s t u v) (apply values (first calibration)))
               ((ax ay az) (apply values accel))
               ((mx my mz) (apply values mag)))

       (let ((cax (+ (* a ax) b))
             (cay (+ (* c ax) (* d ay) e))
             (caz (+ (* f ax) (* g ay) (* h az) i))
             (cmx (+ (* j mx) (* k my) (* l mz) m))
             (cmy (+ (* n mx) (* o my) (* p mz) q))
             (cmz (+ (* r mx) (* s my) (* t mz) u)))
         (list (list cax cay caz) (list cmx cmy cmz)))))

(define (calibrate-accel-mag-3d measurements)
  (let ((initial-accel (calibrate-sensor-3d
                        (map (lambda (measurement) (take measurement 3)) measurements)))
        (initial-mag (calibrate-sensor-3d
                         (map (lambda (measurement) (take-right measurement 3)) measurements))))
  (least-squares-iterate
   (append
    (if (< (second initial-accel) 1)
        (first initial-accel)
        '(1 0  0 1 0  0 0 1 0))
    (if (< (second initial-mag) 1)
        (let ((im (first initial-mag)))
          (list (first im) 0 0 (second im) 0 (fourth im) 0 (fifth im) 0 0 (eighth im) (ninth im)))
        '(1 0 0 0  0 1 0 0  0 0 1 0))
    '(.9)
       )


   (lambda (state measurement)
     (let-values (((a b c d e f g h i j k l m n o p q r s t u v) (apply values state))
                  ((index ax ay az mx my mz) (apply values measurement)))
       (let ((cax (+ (* a ax) b))
             (cay (+ (* c ax) (* d ay) e))
             (caz (+ (* f ax) (* g ay) (* h az) i))
             (cmx (+ (* j mx) (* k my) (* l mz) m))
             (cmy (+ (* n mx) (* o my) (* p mz) q))
             (cmz (+ (* r mx) (* s my) (* t mz) u)))
         (case index
           ((0) (list (list (* 2 ax cax)                           (* 2 cax)
                            (* 2 ax cay) (* 2 ay cay)              (* 2 cay)
                            (* 2 ax caz) (* 2 ay caz) (* 2 az caz) (* 2 caz)
                            0 0 0 0 0 0 0 0 0 0 0 0 0)
                      (- 1 (square cax) (square cay) (square caz))))
           ((1)
            (list (list 0 0 0 0 0 0 0 0 0
                        (* 2 mx cmx) (* 2 my cmx) (* 2 mz cmx) (* 2 cmx)
                        (* 2 mx cmy) (* 2 my cmy) (* 2 mz cmy) (* 2 cmy)
                        (* 2 mx cmz) (* 2 my cmz) (* 2 mz cmz) (* 2 cmz) 0)
                      (- 1 (square cmx) (square cmy) (square cmz))))
           ((2) (let ((nca (normalize (list cax cay caz)))
                      (ncm (normalize (list cmx cmy cmz))))
                  (let ((ncax (first nca)) (ncay (second nca)) (ncaz (third nca))
                        (ncmx (first ncm)) (ncmy (second ncm)) (ncmz (third ncm)))
                  (list (list
                              (* ax cmx)                       cmx
                              (* ax cmy) (* ay cmy)            cmy
                              (* ax cmz) (* ay cmz) (* az cmz) cmz
                              (* mx cax) (* my cax) (* mz cax) cax
                              (* mx cay) (* my cay) (* mz cay) cay
                              (* mx caz) (* my caz) (* mz caz) caz
                              1
                              )
                      (- (+ (* cax cmx) (* cay cmy) (* caz cmz) v))))))))))
   (apply append (map (lambda (measurement)
                        (list (cons 0 measurement)
                              (cons 1 measurement)
                              (cons 2 measurement)
                              ))
                      measurements))
   (lambda (state update)
     (constrain-states! state '((.8 1.2)                   (-.2 .2)
                                (-.1 .1) (.8 1.2)          (-.2 .2)
                                (-.1 .1) (-.1 .1) (.8 1.2) (-.2 .2)
                                (.8 1.2) (-.2 .2) (-.2 .2) (-1 1)
                                (-.2 .2) (.8 1.2) (-.2 .2) (-1 1)
                                (-.2 .2) (-.2 .2) (.8 1.2) (-1 1)
                                (-1 1)
                                ))
     #f)
   20)))
