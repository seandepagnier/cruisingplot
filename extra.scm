


; convert lat lon and alt (spherical coordinates)
; to x, y, z (rectangular coordinates) used for plotting
(define (convert-position lat lon alt)
  (let ((earthradius 6360000))
    (let ((radius (+ earthradius alt))
          (theta (deg2rad lon))
          (phi (deg2rad lat)))
    (list (* earthradius (sin theta) (cos phi))
          (* earthradius (cos theta) (cos phi))
          (* earthradius (sin phi))))))
