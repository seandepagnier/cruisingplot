;; Copyright (C) 2010 Sean D'Epagnier <sean@depagnier.com>
;;
;; This Program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

; Support various sound drivers
(define (sound-play-wav-file filename)
  (warning "No sound driver loaded, using alsaplayer... ")
  (system (string-append "mplayer " filename)))

(define (sound-init driver)
  (case driver
    ((openal)
     (use openal)
     (let ((device (alc:OpenDevice #f))
           (context (alc:CreateContext device #f)))
       (alc:MakeContextCurrent context)
       (set! sound-play-wav-file
             (lambda (filename)
               (let* ((buf (openal:load-buffer "example.wav"))
                     (source (openal:make-source buf)))
                 (al:Sourcei source al:LOOPING 1)
                 (al:SourcePlay source))))
       (push-exit-handler (lambda ()
                            (alc:MakeContextCurrent #f)
                            (alc:DestroyContext context)
                            (alc:CloseDevice device)))))
    ((qt) 
;     (use qt)
       (set! sound-play-wav-file
             (lambda (filename)
               (let ((qsound (qt:sound filename)))
                 (qt:play filename)))))
    ))
