;; Copyright (C) 2011 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version. 

(declare (unit autopilot))

;  The goal is to keep the boat on course by measuring it's motion,
;  then estimating what the motion will be in the future with various
;  helm changes.  The basic forces involved:
;
;  Water Helm from Keel and Rudder pushing water
;  waterhelm(waterspeed, waterdirection, rudderposition) =
;        waterspeed*rudderconstant*rudderposition - keelconstant*heading'
;
;  Weather Helm from sails pushing against air
;                  + weatherhelm(windspeed, winddirection)  + current/bias
;
;  the forces turning the boat are the total of 
;  heading'' = waterhelm + weatherhelm
;
;; We can predict the heading for sailboats with integration of the forces
;; turning the boat:
;
;  heading(t+1) = heading(t) + heading'(t)*t
;  heading'(t+1) = heading'(t) + heading''(t)*t
;
;  the yawmoment is the inertia of yawing the vessel, to be predicte
;
;  we will try a simple quadratic and estimate weatherhelm
;  when sail trim, wind speed, or direction change, these values change
;  weatherhelm(windspeed, winddirection) = a*windspeed^2 + b*winddirection + c
;
;  the rudder constant varies from vessel to vessel
;  rudderhelm(headingrate, rudderposition, waterspeed) = rudderconstant*waterspeed*rudderposition
;       could add optional term:  kappa*rudderspeed to account for lateral water movement from helm change
;
;  since we only command the rudderspeed on or off, we can integrate to get position
;  the position can saturate, so we can apply that as well
;  rudderposition = rudderspeed*time, saturate(rudderposition, +maxrudder, -maxrudder)
;
; to command autopilot, simply calculate the predicted heading at T in the future
; where T is the minimum autopilot movement with rudder speed set to +- and 0, and determine
; which result has the closest heading to the desired heading and is not yawing.

; to determine which move to make, apply critical to underdampening depending
; on how hard we want to work the autopilot


;  m*f''(x) + c*f'(x) + k*x = 0


; for roll pitch and heave we can model like a pendulum:
; M(t) - Mx = Ia
;
; where M(t) is the external healing moment,  Mx is the resisting moment (bouancy and ballast)
; I is the moment of intertia, and a is the angulat acceleration

;

(define (create-autopilot arg)
  (define options
    (create-options
     (append `(,(make-number-verifier 'gain "gain in feedback" 1 0 10)
               ,(make-number-verifier 'gain "max turn speed (deg/s)" 10 0 100)
               ,(make-discrete-verifier 'filter "what autopilot to use" 'basic '(basic derivative))
               ,(make-sensor-indicies-verifier "index of which ahrs to use" 'ahrs 'ahrs)
               ,(make-number-verifier 'update-rate "how often to command motor in hz" 2 .1 100))
               ,(make-number-verifier 'wind-angle "apparent wind angle to hold" 0 -180 180))
             (motor-options)))

  (start-periodic-task
   (/ (options 'update-rate))
   (case (options 'filter)
     ((basic) (autopilot-basic options))
     ((derivative (autopilot-derivative options)))
     (else (error "unrecognized autopilot filter: " (options 'filter))))))

 


;; basic autopilot feedback loop
;  error = wind direction - desired wind direction
; command = error * gain / rate
(define (basic-autopilot options)
  (let ((error (- (computation-calculate 'wind-direction) (options 'wind-angle))))
    (motor-command (/ (* error (options 'gain)) (options 'update-rate)))))

;; derivative autopilot uses the rate of change of heading (gyro) to do a better
; job of heading correction
(define (derivative-autopilot options)
  (let ((error (- (computation-calculate 'wind-direction) (options 'wind-angle)))
        (ahrs (computation-calculate 'ahrs)))
    (let ((yaw-rate (type-field-get 'ahrs 'yawrate ahrs))
          (desired-yaw-rate (saturate (* (options 'gain) error)
                                      (- (options 'max-turn-rate)) (options 'max-turn-rate))))
          (let ((rate-error (- yaw-rate desired-yaw-rate)))
            (motor-command (/ (+ (* rate-error motor-constant))
                              (/ (options 'update-rate))))))))

