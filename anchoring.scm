;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; routines specific to a vessel at anchor

(define (earth-coordinates-in-meters lat lon)
  (spherical-coordinates-in-meters lat lon 6371000))

; Use least squares with gps data to attempt to calculate where the anchor is
; (x-cx)^2 + (y-cy)^2 = r
(define (compute-anchor-location positions)
  (let ((positions-in-meters (map earth-coordinates-in-meters positions)))
    
; Use wind speed and direction data (if available) along with the vessels
; Windage constant to 


; When the wind speed is highest (gusts) we should keep a more careful
; eye on our gps position to attempt to detect dragging


; With accelerometer data, we can use the current state of the vessel
; in the waves to calculate the surge...
