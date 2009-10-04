
(define (event-mask? item)
  (or (fixnum? item)
      (and (list? item)
	   (every? event-mask-bit-number item))))

(define (make-event-mask item)
  (if (fixnum? item)
      item
      (let loop ((mask 0)
		 (lst item))
	(if (null? lst)
	    mask
	    (loop (bitwise-or mask 
			      (logical-shift-left 1 (event-mask-bit-number (car lst))))
		  (cdr lst))))))
	    

(define (event-mask-bit-number item)
  (vmemq item
	 '#(key-press
	    key-release
	    button-press
	    button-release
	    enter-window
	    leave-window
	    pointer-motion
	    pointer-motion-hint
	    button1-motion
	    button2-motion
	    button3-motion
	    button4-motion
	    button5-motion
	    button-motion
	    keymap-state
	    exposure
	    visibility-change
	    structure-notify
	    resize-redirect
	    substructure-notify
	    substructure-redirect
	    focus-change
	    property-change
	    colormap-change
	    owner-grab-button)))

