
(define (keyboard-mapping (display <x-display>)
			  #key (first-keycode 
				default: (display-min-keycode display))
			       (start 
				default: first-keycode)
			       (end 
				default: (+ 1 (display-max-keycode display)))
			       (data default: #f))
  (let* ((m (- end start))
	 (r (internal-rpc
	     display
	     (make-buffer u1: 101 ; GetKeyboardMapping
			  u1: 0
			  u2: 2
			  u1: first-keycode
			  u1: m
			  u2: 0))))
    (with-unpacked (common-reply r)
		   (u1: -
		    u1: keysyms-per-keycode   ; `n'
		    u2: sequence-number
		    u4: reply-length)
     (let (((m2 <fixnum>) (quotient reply-length keysyms-per-keycode))
	   ((data <vector>) (or data (make-vector end #f)))
	   ((rr <string>) (remainder-reply r)))
       ;(dm 121 "keysyms/keycode: ~d" keysyms-per-keycode)
       ;(dm 122 "reply-length: ~d" reply-length)
       ;(dm 123 "start: ~d  end: ~d  n: ~d" start end keysyms-per-keycode)
       ;
       (if (not (= reply-length (* m keysyms-per-keycode)))
	   (em 129 "reply length ~d, expected ~d*~d" 
	       reply-length
	       m keysyms-per-keycode))
       (let loop (((remainder-reply-index <fixnum>) 0)
		  ((array-index <fixnum>) start))
	 ;(dm 124 "index ~d at: ~d" array-index remainder-reply-index)
	 (if (< array-index end)
	     (let ((kv (make-vector keysyms-per-keycode #f)))
	       (vector-set! data array-index kv)
	       (let kloop (((i2 <fixnum>) remainder-reply-index)
			   ((k2 <fixnum>) 0))
		 (if (< k2 keysyms-per-keycode)
		     (begin
		       (vector-set! kv k2 (xbo-read-u4 rr i2))
		       (kloop (fixnum+ i2 4) (add1 k2)))
		     (loop i2 (add1 array-index)))))
	     data))))))
