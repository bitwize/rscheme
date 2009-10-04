     
      
#|
    (if background
	(begin
	  (assert (fixnum? background))
	  (inc! value-buffer-size)))
    ;
    (if cursor
        (begin
          (assert (or (eq? cursor 'none) (cursor? cursor)))
          (inc! value-buffer-size)))
    ;
    ; build value vector
    ;
    (let (((value-buffer <byte-vector>) (bvec-alloc <byte-vector>
						    (* 4 value-buffer-size)))
	  ((value-buffer-index <fixnum>) 0)
	  ((value-bitmask <fixnum>) 0))
      ;
       


      (if background
	  (begin
	    (xbo-write-u4 value-buffer 
			  value-buffer-index
			  background)
	    (inc! value-buffer-index 4)
	    (inc! value-bitmask #x0002)))
      ;
      (if cursor
	  (begin
	    (xbo-write-u4 value-buffer
			  value-buffer-index 
			  (if (eq? cursor 'none)
			      0
			      (cursor-id cursor)))
	    (inc! value-bitmask #x4000)
	    (inc! value-buffer-index)))
|#
