
(define (play-sound snd)
  (values))
#|
  (let ((s (string-append "/usr/local/np/music/audioman/snd/" snd ".snd")))
    (run "sox" 
         "-t" ".au" s
         "-t" "ossdsp" "/dev/dsp")))
|#
