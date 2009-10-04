
(define (change-keyboard-control (display <x-display>)
				 #key (key-click-percent default: #f)
				      (bell-percent default: #f)
				      (bell-pitch default: #f)
				      (bell-duration default: #f)
				      (led default: #f)
				      (led-mode default: #f)
				      (key default: #f)
				      (auto-repeat-mode default: #f))
  (make-value-list (mask len vec)
    ((0 key-click-percent
	(or (eq? key-click-percent 'default)
	    (and (fixnum? key-click-percent)
		 (>= key-click-percent 0)
		 (<= key-click-percent 100)))
	(if (eq? key-click-percent 'default)
	    -1
	    key-click-percent))
     ;
     (1 bell-percent
	(or (eq? bell-percent 'default)
	    (and (fixnum? bell-percent)
		 (>= bell-percent 0)
		 (<= bell-percent 100)))
	(if (eq? bell-percent 'default)
	    -1
	    bell-percent))
     ;
     (2 bell-pitch
	(or (eq? bell-pitch 'default)
	    (and (fixnum? bell-pitch)
		 (>= bell-pitch 1)
		 (<= bell-pitch 32767)))
	(if (eq? bell-pitch 'default)
	    -1
	    bell-pitch))
     ;
     (3 bell-duration (fixnum? bell-duration) bell-duration)
     ;
     (4 led 
	(and (fixnum? led)
	     (>= led 1)
	     (<= led 32))
	led)
     ;
     (5 led-mode 
	(memq led-mode '(on off)) 
	(case led-mode
	  ((off) 0)
	  ((on) 1)))
     ;
     (6 key 
	(and (fixnum? key)
	     (>= key 8)
	     (<= key 255)) 
	key)
     ;
     (7 auto-repeat-mode 
	(memq auto-repeat-mode '(on off default))
	(case auto-repeat-mode
	  ((off) 0)
	  ((on) 1)
	  ((default) 2))))
    (internal-send
     display
     (vector (make-buffer u1: 102 ; ChangeKeyboardControl
			  u1: 0
			  u2: (+ 2 len)
			  u4: mask)
	     vec))))

			  