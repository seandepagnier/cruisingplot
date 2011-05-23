;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.


;; The goal is to take input from various sources, and over time and find the
;; optimal adjustment update.   The most important and obvious input is wind
;; direction data, and is the only required input

;; simple corellation between position, wind direction and wind speed as well
;; as a wide varience to take care of factors like 

(define (simple-sheet-control)
  (
