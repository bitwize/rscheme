
(define-macro (value-list%pre (ix key check val) mask-var len-var)
  `(if ,key
       (begin
	 (assert ,check)
	 (set! ,len-var (add1 ,len-var))
	 (set! ,mask-var (fixnum+ ,mask-var ,(logical-shift-left 1 ix))))))

(define-macro (value-list%post (ix key check val) vec-var ptr-var)
  `(if ,key
       (begin
	 (xbo-write-u4 ,vec-var ,ptr-var ,val)
	 (set! ,ptr-var (fixnum+ ,ptr-var 4)))))

(define-macro (make-value-list (mask-var len-var vec-var) items . body)
  (let ((ix (gensym)))
    `(let (((,mask-var <fixnum>) 0)
	   ((,len-var <fixnum>) 0)
	   ((,ix <fixnum>) 0))
       ,@(map (lambda (x)
		`(value-list%pre ,x ,mask-var ,len-var))
	      items)
       (let (((,vec-var <byte-vector>) (bvec-alloc <byte-vector> 
						   (fixnum* 4 ,len-var))))
	 ,@(map (lambda (x)
		  `(value-list%post ,x ,vec-var ,ix))
		items)
	 (begin ,@body)))))

(define $line-styles '#(solid on-off-dash double-dash))
(define $cap-styles '#(not-last butt round projecting))

(define $join-styles '#(miter round bevel))
(define $fill-styles '#(solid tiled stippled opaque-stippled))
(define $fill-rules '#(even-odd winding))
(define $arc-modes '#(chord pie-slice))

(define $boole-constant '#(boole-clr
			   boole-and
			   boole-andc2
			   boole-1
			   boole-andc1
			   boole-2
			   boole-xor
			   boole-ior
			   boole-nor
			   boole-eqv
			   boole-c2
			   boole-orc2
			   boole-c1
			   boole-orc1
			   boole-nand
			   boole-set))

(define (create-gcontext #key (arc-mode default: #f)
			      (background default: #f)
			      (cache? default: #t)
			      (cap-style default: #f)
			      (clip-mask default: #f)
			      (clip-ordering default: #f)
			      (clip-x default: #f)
			      (clip-y default: #f)
			      (dash-offset default: #f)
			      (dashes default: #f)
			      (drawable type: <x-drawable>)
			      (exposures default: #f)
			      (fill-rule default: #f)
			      (fill-style default: #f)
			      (font default: #f)
			      (foreground default: #f)
			      (function default: #f)
			      (join-style default: #f)
			      (line-style default: #f)
			      (line-width default: #f)
			      (plane-mask default: #f)
			      (stipple default: #f)
			      (subwindow-mode default: #f)
			      (tile default: #f)
			      (ts-x default: #f)
			      (ts-y default: #f))
  (let ((id (alloc-x-id (x-display drawable))))
    (make-value-list (value-bitmask num-values values-bvec)
	             ((0 function 
			 (vmemq function $boole-constant)
			 (vmemq function $boole-constant))
		      (1 plane-mask (fixnum? plane-mask) plane-mask)
		      (2 foreground (fixnum? foreground) foreground)
		      (3 background (fixnum? background) background)
		      (4 line-width (fixnum? line-width) line-width)
		      (5 line-style
			 (vmemq line-style $line-styles)
			 (vmemq line-style $line-styles))
		      (6 cap-style
			 (vmemq cap-style $cap-styles)
			 (vmemq cap-style $cap-styles))
		      (7 join-style
			 (vmemq join-style $join-styles)
			 (vmemq join-style $join-styles))
		      (8 fill-style
			 (vmemq fill-style $fill-styles)
			 (vmemq fill-style $fill-styles))
		      (9 fill-rule
			 (vmemq fill-rule $fill-rules)
			 (vmemq fill-rule $fill-rules))
		      (10 tile (pixmap? tile) (x-id tile))
		      (11 stipple (pixmap? stipple) (x-id stipple))
		      (12 ts-x (fixnum? ts-x) ts-x)
		      (13 ts-y (fixnum? ts-y) ts-y)
		      (14 font (font? font) (x-id font))
		      ;(15 subwindow-mode ...)
		      (16 exposures 
                          (boolean? exposures) 
                          (if exposures 1 0))
		      (17 clip-x (fixnum? clip-x) clip-x)
		      (18 clip-y (fixnum? clip-y) clip-y)
		      ;(19 clip-mask ...)
		      (20 dash-offset (fixnum? dash-offset) dash-offset)
		      (21 dashes 
			  (and (fixnum? dashes)
			       (>= dashes 1)
			       (<= dashes 255))
			  dashes)
		      (22 arc-mode
			  (vmemq arc-mode $arc-modes)
			  (vmemq arc-mode $arc-modes)))
      (internal-send
       (x-display drawable)
       (vector
	(make-buffer u1: 55 ;; CreateGC
		     u1: 0
		     u2: (+ 4 num-values)
		     u4: id
		     u4: (x-id drawable)
		     u4: value-bitmask)
	values-bvec)))
    (make <x-gcontext>
	  x-id: id
	  x-display: (x-display drawable))))
