;; Copyright (C) 2009 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.



; make syntax for various opengl features to make them nicer
(define-syntax glBegin (syntax-rules ()
			 ((glBegin val body ...)
			  (begin (gl:Begin val) body ... (gl:End)))))

(define-syntax glLetMatrix (syntax-rules ()
			 ((glLetMatrix body ...)
			  (begin (gl:PushMatrix) body ... (gl:PopMatrix)))))

(define-syntax glLetEnable (syntax-rules ()
			     ((glLetEnable (val ...) body ...)
			      (begin (begin (gl:Enable val) ...)
				     (begin body ...)
				     (begin (gl:Disable val) ...)))))

(define (RotateAfter ang x y z)
  (let ((m (f32vector)))
    (gl:GetFloatv gl:MODELVIEW_MATRIX m)
    (gl:LoadIdentity)
    (gl:Rotated ang x y z)
    (gl:MultMatrixf m)))

(define (glVertex x y . zw)
  (if (null? zw)
      (gl:Vertex3d x y 0)
      (if (null? (cdr zw))
          (gl:Vertex3d x y (car zw))
          (gl:Vertex4d x y (car zw) (cadr zw)))))

(define (glColor r g b . a)
  (gl:Color4d r g b (if (null? a) 0 (car a))))

(define (glRasterPos x y #!optional (z 0))
  (gl:RasterPos3d x y z))
