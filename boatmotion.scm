;; Copyright (C) 2012 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version. 

; boat motion kalman filter
;
; after we already have accurate pitch roll and yaw data as well as accelerations
; we can attempt to calibrate unknown coefficients to model the boat motion
;
; the sensors are not at the center of moment of the boat therefore we must
; calculate its relative position in the vessel (which does not change)
;
; over time model zero pitch, but allow heel and heading to be varying
;
; boat motion period is the same for pitch roll and yaw,
;
; yaw   = heading + yawing_amplitude*cos(time/boat_motion_period + heading_phase)
; roll  = heel    + rolling_amplitude*cos(time/boat_motion_period + rolling_phase)
; pitch = 0       + pitching_amplitude*cos(time/boat_motion_period + pitching_phase)
;
; these equations can be applied for gyroscopes only to determine heading and heel
;
; with accelerometers we can calculate the location of the sensors in the vessel
;
; accel_z = cos(heel) + heaving_amplitude*cos(time/boat_motion_period + heaving_phase)
;                       + x*current_pitch' + y*current_roll'
; accel_x = (0 or gps acceleration with lowpass)
;                       + surging_amplitude*cos(time/boat_motion_period + surging_phase)
;                       + y*current_yaw' + z*current_pitch'
; accel_y = sin(heel)   + swaying_amplitude*cos(time/boat_motion_period + swaying_phase)
;                       + x*current_yaw' + z*current_roll'
; 
; x y z
; boat_motion_period heading heel
; yawing_amplitude yawing_phase rolling_amplitude rolling_phase pitching_amplitude pitching_phase
; heaving_amplitude heaving_phase surge_amplitude surge_phase swaying_amplitude swaying_phase
;
; 18 unknowns in basic model
