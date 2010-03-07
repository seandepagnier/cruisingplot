;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

(define (draw-vector-arrow a b)
  (let ((m (map average a b))
        (n `(,(* .15 (- (cadr a) (cadr b)))
             ,(* .15 (- (car b) (car a))))))
    (glBegin gl:LINES
             (apply glVertex a)
             (apply glVertex b)
             (apply glVertex (map + m n))
             (apply glVertex b)
             (apply glVertex (map - m n))
             (apply glVertex b))))

(define (drawcoastlines)
  (let ((filename "/usr/local/share/xtide/wvs/wvsfull.dat"))
    #t
    )
)
