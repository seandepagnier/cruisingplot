; Korteweg-de Vries Equation
;
; df/dt + d^3f/dx^3 + 6*f*df/dx = 0
;
; f(x, t) = w(x-c*t)  c is phase speed
;
; -c*dw/dx + d^3*w/dx^3 + 6*w*dw/dx = 0
;   or
; 3*w^2 + d^2w/dx^2 - cw = A
; 
;
; Calculate fourier transform of waves derived from
; heave (z component of acceleromter), and try to fit
; constants to relate pitching vs rolling motion relative to wave direction
