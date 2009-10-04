
(define (my-configure-notify-handler display
				     #rest ignore
				     #key window x y width height above-sibling)
  (dm 131 "configure-notify on ~s: above ~s" window (or above-sibling 'none))
  #t)

; (format #t "new configuration for ~s: ~s\n"
;	  window
;	  (make-rect x y width height)))
