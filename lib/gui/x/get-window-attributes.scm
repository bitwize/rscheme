
(define (get-geometry (self <x-drawable>))
  (with-unpacked (common-reply
		  (internal-rpc (x-display self)
				(make-buffer u1: 14
					     u1: 0
					     u2: 2
					     u4: (x-id self))))
    (u1: -
     u1: depth 
     u2: sequence-number
     u4: reply-length
     u4: root-window
     s2: x
     s2: y
     u2: width
     u2: height
     u2: border-width)
    (vector x y width height border-width #f #f root-window depth)))

(define-macro (get-window-attribute w field)
  `(let ((%r (internal-rpc (x-display ,w)
			   (make-buffer u1: 3  ; GetWindowAttributes
					u1: 0
					u2: 2
					u4: (x-id ,w)))))
     (with-unpacked (common-reply %r)
       (u1: -
	u1: backing-store 
	; ((0 NotUseful) (1 WhenMapped) (2 Always))
	u2: sequence-number
	u4: reply-length
	u4: visual
	u2: class
	;enum: ((1 InputOutput) (2 InputOnly))
	u1: bit-gravity
	u1: win-gravity
	u4: backing-planes
	u4: backing-pixel
	u1: save-under
	;enum: ((0 off) (1 on))
	u1: map-is-installed
	u1: map-state
	;enum: ((0 Unmapped) (1 Unviewable) (2 Viewable))
	u1: override-redirect
	u4: colormap)
       (with-unpacked (remainder-reply %r)
	 (u4: all-event-mask
	  u4: your-event-mask
	  u2: do-not-propagate-mask)
	,field))))
      
(define (window-colormap (w <x-window>))
  (let ((cmap-x-id (get-window-attribute w colormap))
	((dpy <x-display>) (x-display w)))
    (or (table-lookup (xid-table dpy) cmap-x-id)
	(make <x-colormap>
	      colormap-visual-type: #f  ; unknown
	      x-display: dpy
	      x-id: cmap-x-id))))

(define-syntax (with-state drawable . body)
  (with-state* drawable (lambda () (begin . body))))

(define-class <with-state-cache> (<object>)
  (for-drawable type: <x-drawable>)
  (have-geometry? type: <boolean> init-value: #f)
  (geometry-value-mask type: <fixnum> init-value: 0)
  (geometry-values type: <vector>)
  (have-attributes? type: <boolean> init-value: #f)
  (attributes-value-mask type: <fixnum> init-value: 0)
  (attributes-values type: <vector>))

(define (flush-with-state (self <with-state-cache>))
  (if (not (zero? (geometry-value-mask self)))
      (begin
	(bind ((v n (pick-dirty-values (geometry-value-mask self)
				       (geometry-values self))))
	  ;(print v)
	  (internal-send
	   (x-display (for-drawable self))
	   (vector (make-buffer u1: 12 ; ConfigureWindow
				u1: 0
				u2: (+ 3 n)
				u4: (x-id (for-drawable self))
				u2: (geometry-value-mask self)
				u2: 0)
		   v))
	  (set-geometry-value-mask! self 0)))))

(define (pick-dirty-values (dirty-mask <fixnum>) (all-values <vector>))
  ;; loop in reverse so we build the list in the right order
  (let loop ((k (vector-length all-values))
	     (n 0)
	     (mask (logical-shift-left 1 (sub1 (vector-length all-values))))
	     (r '()))
    (if (eq? k 0)
	(values (list->vector r) n)
	(if (eq? (bitwise-and dirty-mask mask) 0)
	    (loop (sub1 k) n (logical-shift-right mask 1) r)
	    (loop (sub1 k) (add1 n) (logical-shift-right mask 1) 
		  (cons (vector-ref all-values (sub1 k)) r))))))

;;;----------------------------------------------------------------------

(define-macro (define-geometric-property prop k op . opt)
  (let ((setter (if (equal? opt '(:read-only))
		    #f
		    (symbol-append "set-" prop "!"))))
    `(begin
       (define-method ,prop ((self <with-state-cache>))
	 (if (not (have-geometry? self))
	     (begin
	       (flush-with-state self) ; flush any pending updates
	       (set-geometry-values! self (get-geometry (for-drawable self)))
	       (set-have-geometry?! self #t)))
	 (vector-ref (geometry-values self) ,k))
       ;;
       ,(if setter
	    `(define-method ,setter ((self <with-state-cache>) value)
	       (set-have-geometry?! self #f)
	       (set-geometry-value-mask! 
		self
		(bitwise-or (geometry-value-mask self) 
			    ,(logical-shift-left 1 k)))
	       (vector-set! (geometry-values self) ,k (,op value))
	       value)
	    '(begin))
       ;;
       (define-method ,prop ((self <x-drawable>))
	 (if (cache self)
	     (,prop (cache self))
	     (with-state* self (lambda () (,prop self)))))
       ;;
       ,(if setter
	    `(define-method ,setter ((self <x-drawable>) value)
	       (if (cache self)
		   (,setter (cache self) value)
		   (with-state* self (lambda () (,setter self value)))))
	    '(begin)))))

(define-geometric-property drawable-x 0 int16->string)
(define-geometric-property drawable-y 1 int16->string)
(define-geometric-property drawable-width 2 card16->string)
(define-geometric-property drawable-height 3 card16->string)
(define-geometric-property drawable-border-width 4 card16->string)
(define-geometric-property drawable-depth 8 card8->string :read-only)

#|
(define-method drawable-x ((self <with-state-cache>))
  (if (not (have-geometry? self))
      (begin
	(flush-with-state self) ; flush any pending updates
	(set-geometry-values! self (get-geometry (for-drawable self)))
	(set-have-geometry?! self #t)))
  (vector-ref (geometry-values self) 0))

(define-method set-drawable-x! ((self <with-state-cache>) x)
  (set-have-geometry?! self #f)
  (set-geometry-value-mask! self (bitwise-or (geometry-value-mask self) #x001))
  (vector-set! (geometry-values self) 0 x)
  x)
|#

;;;----------------------------------------------------------------------

(define (with-state* drawable thunk)
  (let ((cache (make <with-state-cache>
		     for-drawable: drawable
		     geometry-values: (make-vector 7)
		     attributes-values: (make-vector 15))))
    (set-cache! drawable cache)
    (bind ((#rest r (thunk)))
      (set-cache! drawable #f)
      (flush-with-state cache)
      (list->values r))))
