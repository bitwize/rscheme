(define-syntax inc!
  (syntax-form (v)
    (set! v (+ v 1)))
  (syntax-form (v d)
    (set! v (+ v d))))

(define-macro (pre-setup ix key check val)
  `(if ,key
       (begin
	 (assert ,check)
	 (inc! value-buffer-size))))

(define-macro (post-setup ix key check val)
  `(if ,key
       (begin
	 (xbo-write-u4 value-buffer 
		       value-buffer-index
		       ,val)
	 (inc! value-buffer-index 4)
	 (inc! value-bitmask ,(logical-shift-left 1 ix)))))

(define-macro (window/with-value-list props . body)
  `(let (((value-buffer-size <fixnum>) 0))
     ,@(map (lambda (b)
	      (cons 'pre-setup b))
	    props)
     ;
     ; build value vector
     ;
     (let (((value-buffer <byte-vector>) (bvec-alloc <byte-vector>
						     (* 4 value-buffer-size)))
	   ((value-buffer-index <fixnum>) 0)
	   ((value-bitmask <fixnum>) 0))
       ,@(map (lambda (b)
		(cons 'post-setup b))
	      props)
       ,@body)))

(define-macro (encode-enum key enums)
  `(case ,key
     ,@(map (lambda (i v)
	      `((,v) ,i))
	    (range (length enums))
	    enums)
     (else
      (error "create-window: ~s is not a valid value for ~s" ,key ',key))))

(define (create-window #key (parent type: <x-window>) 
		            (x type: <fixnum>)
			    (y type: <fixnum>)
			    (width type: <fixnum>)
			    (height type: <fixnum>)
                            (depth type: <fixnum> default: 0)
                            (border-width type: <fixnum> default: 0)
                            (class default: 'copy)
                            (visual default: 'copy)
			    ;
                            (background default: #f)
                            (border default: #f)
                            (gravity default: #f)
                            (bit-gravity default: #f)
                            (backing-store default: #f)
                            (backing-planes default: #f)
                            (backing-pixel default: #f)
                            (save-under default: #f)
                            (event-mask default: #f)
                            (do-not-propagate-mask default: #f)
                            (override-redirect default: #f)
                            (colormap default: #f)
                            (cursor default: #f))
  ;
  (let (((class-num <fixnum>) (encode-enum 
			       class
			       (copy input-output input-only))))
    ;
    ; validate parameters
    ;
    (or (eq? visual 'copy)
	(visual? visual))
    ;
    (window/with-value-list
     ((1 background
	 (fixnum? background)
	 background)
      (3 border
	 (fixnum? border)
	 border)
      (9 override-redirect (boolean? override-redirect) 1)
      (10 save-under (boolean? save-under) 1)
      (11 event-mask
	  (event-mask? event-mask)
	  (make-event-mask event-mask))
      (14 cursor
	  (or (eq? cursor 'none) (cursor? cursor))
	  (if (eq? cursor 'none) 
	      0
	      (cursor-id cursor))))
     ;
     ; issue the request
     ;
     (let ((id (alloc-x-id (x-display parent))))
       ; XXX need a `with-display' here?
       (internal-send
	(x-display parent)
	(vector (make-buffer u1: 1       ;; CreateWindow
			     u1: depth
			     u2: (+ 8 value-buffer-size)
			     u4: id
			     u4: (x-id parent)
			     u2: x
			     u2: y
			     u2: width
			     u2: height
			     u2: border-width
			     u2: class-num
			     u4: (if (eq? visual 'copy)
				     0
				     (x-id visual))
			     u4: value-bitmask)
		value-buffer))
       (let ((w (make <x-window>
		      drawable-root: (drawable-root parent)
		      x-display: (x-display parent)
		      x-id: id)))
	 (table-insert! (xid-table (x-display parent)) id w)
	 w)))))

