
(define (default-property-transform datum)
  (if (string? datum)
      datum
      (apply string-append (map (lambda (d)
				  (make-buffer u4: d))
				datum))))
      
(define (change-property (window <x-window>) 
			 property
			 data
			 type
			 format
			 #key (mode default: 'replace)
			      (start default: 0)
			      (transform default: default-property-transform))
  (bind ((t (transform data))
	 (n (string-length t))
	 (p pad-str (pad4 n)))
    (internal-send
     (x-display window)
     (vector
      (make-buffer u1: 18 ; ChangeProperty
		   u1: (case mode
			 ((replace) 0)
			 ((prepend) 1)
			 ((append) 2)
			 (else (error "change-property: mode `~s' invalid"
				      mode)))
		   u2: (+ 6 (quotient (+ n p) 4))
		   u4: (x-id window)
		   u4: (find-atom (x-display window) property)
		   u4: (find-atom (x-display window) type)
		   u1: format
		   u1: 0
		   u2: 0
		   u4: (quotient n (quotient format 8)))
      t
      pad-str))))

  