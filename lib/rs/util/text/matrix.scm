,(use tables sort)

(define (table-update! tbl key proc)
  (table-insert! tbl key (proc (table-lookup tbl key))))


(define-method hash-value ((self <object>))
  (transient->hash self))

(define-method hash-value ((self <fixnum>))
  (integer->hash self))

(define-method hash-value ((self <pair>))
  (+ (hash-value (car self))
     (hash-value (cdr self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     General matrix utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (vector-range end #key (start default: 0))
  (let* ((len (- end start))
	 (v (make-vector len #f)))
    (let loop ((i start)
	       (j 0))
      (if (< i end)
	  (begin
	    (vector-set! v j i)
	    (loop (+ i 1) (+ j 1)))
	  v))))

(define (make-matrix h w #optional fill)
  (vector-map
   (lambda (y)
     (make-vector w fill))
   (vector-range h)))

(define (matrix-update! matrix y x proc)
  (let ((row (vector-ref matrix y)))
    (vector-set! row x (proc (vector-ref row x)))))

(define (matrix-for-each matrix proc)
  (vector-for-each/i
   (lambda (y row)
     (vector-for-each/i
      (lambda (x elem)
	(proc y x elem))
      row))
   matrix))

(define (matrix-dimen matrix)
  (values (vector-length matrix)
	  (reduce (lambda (a b)
		    (max a (vector-length b)))
		  0
		  (vector->list matrix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;      Cell Formatting
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <cell-format> (<object>)
  ;;
  ;; horz
  ;;
  (cf-horz-align init-value: 'left)
  (cf-left-border-style init-value: #f)
  (cf-right-border-style init-value: #f)
  (cf-left-margin init-value: 1)
  (cf-right-margin init-value: 1)
  (cf-horz-span init-value: 1)
  ;;
  ;; vert
  ;;
  (cf-vert-align init-value: 'top)
  (cf-bottom-border-style init-value: #f)
  (cf-top-border-style init-value: #f)
  (cf-top-margin init-value: 0)
  (cf-bottom-margin init-value: 0)
  (cf-vert-span init-value: 1)
  ;;
  )

;;; these procedures apply to format descriptions
;;; (ie, a format is a list of two format descriptions)

(define-syntax fmtd-align
  (syntax-form (('fmt-h f)) (cf-horz-align f))
  (syntax-form (('fmt-v f)) (cf-vert-align f)))

(define-syntax fmtd-l-style
  (syntax-form (('fmt-h f)) (cf-left-border-style f))
  (syntax-form (('fmt-v f)) (cf-top-border-style f)))

(define-syntax fmtd-r-style
  (syntax-form (('fmt-h f)) (cf-right-border-style f))
  (syntax-form (('fmt-v f)) (cf-bottom-border-style f)))

(define-syntax fmtd-l-margin
  (syntax-form (('fmt-h f)) (cf-left-margin f))
  (syntax-form (('fmt-v f)) (cf-top-margin f)))

(define-syntax fmtd-r-margin
  (syntax-form (('fmt-h f)) (cf-right-margin f))
  (syntax-form (('fmt-v f)) (cf-bottom-margin f)))

;;;

(define *default-format*
  (make <cell-format>))

(define $empty-format
  (make <cell-format>
	cf-left-margin: 0
	cf-right-margin: 0))

(define *default-empty-format* $empty-format)

(define (with-default-cell-format spec thunk)
  (fluid-let ((*default-format* (interp-cell-format spec)))
    (thunk)))

(define *named-cell-formats*
  (list
   (cons 'plain *default-empty-format*)
   (cons 'borders (make <cell-format>
			cf-top-border-style: 1
			cf-bottom-border-style: 1
			cf-left-border-style: 1
			cf-right-border-style: 1))))

(define (interp-cell-format spec)
  (cond
   ((instance? spec <cell-format>)
    spec)
   ((symbol? spec)
    (case spec
      ;; special case these because they are thread variables
      ((default)
       *default-format*)
      ((empty)
       *default-empty-format*)
      (else
       (let ((b (assq spec *named-cell-formats*)))
	 (if b
	     (cdr b)
	     (error "~s: not a known named format" spec))))))
   (else
    (let ((fmt (clone (interp-cell-format (car spec)))))
      (let loop ((s (cdr spec)))
	(if (null? s)
	    fmt
	    (begin
	      (case (car s)
		((horz-align:) (set-cf-horz-align! fmt (cadr s)))
		((left-border:) (set-cf-left-border-style! fmt (cadr s)))
		((right-border:) (set-cf-right-border-style! fmt (cadr s)))
		((left-margin:) (set-cf-left-margin! fmt (cadr s)))
		((right-margin:) (set-cf-right-margin! fmt (cadr s)))
		((vert-align:) (set-cf-vert-align! fmt (cadr s)))
		((top-border:) (set-cf-top-border-style! fmt (cadr s)))
		((bottom-border:) (set-cf-bottom-border-style! fmt (cadr s)))
		((top-margin:) (set-cf-top-margin! fmt (cadr s)))
		((bottom-margin:) (set-cf-bottom-margin! fmt (cadr s)))
		((horz-span:) (set-cf-horz-span! fmt (cadr s)))
		((vert-span:) (set-cf-vert-span! fmt (cadr s)))
		(else
		 (error "invalid cell format spec: ~s" (car s))))
	      (loop (cddr s)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;      Matrix Cells
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <cell> (<object>)
  (cell-w type: <fixnum>)
  (cell-h type: <fixnum>)
  (cell-format type: <cell-format>)
  (lines type: <vector>))

(define-class <non-cell> (<cell>))

;;; this is the cell used for missing entries in the input,

(define $missing-cell
  (make <non-cell>
	cell-w: 0
	cell-h: 0
	cell-format: $empty-format
	lines: '#()))

;;; this is the cell used for covered cells (ie, cells
;;; skipped by spanning).  The vertical renditon procedure
;;; relies on its object identity to suppress output

(define $covered-cell
  (make <non-cell>
	cell-w: 0
	cell-h: 0
	cell-format: $empty-format
	lines: '#()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;      Constraint Satisfaction
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; increase the widths of a subvector, weighted by the
;;; current width, so that exactly `total-growth' is added
;;; to the subvector sum
;;;
;;; P.S. I don't know if this algorithm is "fair" in any
;;; sense -- accumulated error or other quirks may shift
;;; its behavior
;;;
;;; P.P.S. I _do_ know that this algorithm sometimes makes
;;; the table bigger than it needs to be to satisfy the
;;; constraints.  An overlapping constraint can be satisfied
;;; and minimize table growth by expanding just the intersection
;;; cells.  This algorithm, however, will process the constraints
;;; separately.

(define (grow-weighted vec from to cur-sum total-growth)
  (let ((per-unit (/ total-growth cur-sum)))
    (let loop ((i from)
	       (num-left (- to from))
	       (amt-left total-growth))
      (if (eq? num-left 0)
	  ;; the last one has to absorb all the rest
	  (vector-set! vec i (+ (vector-ref vec i) amt-left))
	  ;; otherwise, allot its growth in proportion to its
	  ;; size contribution
	  (let ((grow1 (min amt-left ; must be no more than this!
			    (inexact->exact
			     (round (* (vector-ref vec i) per-unit))))))
	    (vector-set! vec i (+ (vector-ref vec i) grow1))
	    (loop (+ i 1)
		  (- num-left 1)
		  (- amt-left grow1)))))))

;;; incorporate the constraints implied by the pair-table
;;; into the width-vec

(define (solve-constraints wvec ptbl)
  (for-each
   (lambda (constraint)
     (let* ((from (caar constraint))
	    (to (cdar constraint))
	    (value (cdr constraint))
	    (cur (subvector wvec from to))
	    (cursum (reduce + 0 (vector->list cur))))
       (format #t "over ( ~d, ~d ) min size is ~d, current is ~s = ~d\n" 
	       from to value cur cursum)
       (let ((grow-by (- value cursum)))
	 (if (> grow-by 0)
	     (grow-weighted wvec from to cursum grow-by)))))
   ;; order the constraints so we are working from the
   ;; narrowest (tightest) to the broadest.  This 
   ;; sounds good, but I'm not sure why...  It seems
   ;; it should guarantee none of a certain kind of conflict
   ;; or poor choice as may happen if you grow one cell due
   ;; to an outer constraint and then find out you might as
   ;; well have grown a different one because of a narrower
   ;; constraint
   (sort
    (map cons (key-sequence ptbl) (value-sequence ptbl))
    (lambda (a b)
      (let ((span (- (- (cdar a) (caar a))
		     (- (cdar b) (caar b)))))
	(if (= span 0)
	    (< (caar a) (caar b))  ; if span is equal, do top/left one first
	    (< span 0)))))))

(define (compute-axis-dimens h w get-cell col-seps cf-span cell-axis-len)
  (let ((wvec (make-vector w 0))
	(ptbl (make-table equal? hash-value)))
    ;;
    (define (enforce-min from to min)
      (if (= to (+ from 1))
	  (vector-set! wvec from (max min (vector-ref wvec from)))
	  (table-update! ptbl
			 (cons from to)
			 (lambda (old-min)
			   (max min (or old-min 0))))))
    ;; loop over the rows
    (do ((y 0 (+ y 1)))
	((eq? y h))
      ;; loop over the columns
      (let loop ((from 0))
	(if (< from w)
	    (let* ((cell (get-cell y from))
		   (colspan (cf-span (cell-format cell)))
		   (to (+ from colspan))
		   ; for these purposes, we bill the preceding separator
		   ; against this one; this has the effect of
		   ; making more space available when multiple
		   ; columns (or rows) are spanned
		   (cw (+ (cell-axis-len cell)
			  (vector-ref col-seps from))))
	      #|(format #t "cell ~s --> cols (~d, ~d) width >= ~d\n"
		      (lines cell) from to cw)|#
	      (enforce-min from to cw)
	      (loop to)))))
    ;; 
    (solve-constraints wvec ptbl)
    (vector-map - wvec col-seps)))

(define (compute-col-widths h w cells col-seps)
  (compute-axis-dimens h w
		       (lambda (i j)
			 (vector-ref (vector-ref cells i) j))
		       col-seps cf-horz-span cell-w))
    
(define (compute-row-heights h w cells row-seps)
  (compute-axis-dimens w h 
		       (lambda (j i)
			 (vector-ref (vector-ref cells i) j))
		       row-seps cf-vert-span cell-h))

#|
(define (t)
  (enforce-min 0 2 10) ;; A
  (enforce-min 2 3 3)  ;; B
  (enforce-min 0 1 3)  ;; C
  (enforce-min 1 2 3)  ;; D
  (enforce-min 0 1 3)  ;; E  (note how nicely C and E get merged)
  (enforce-min 1 3 10) ;; F
  (enforce-min 0 3 5)  ;; other
  (solve-constraints mvec ptbl))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Layout Computation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compute-row-seps h w cells)
  (let ((rowsep (make-vector (+ h 1) 0)))
    (vector-for-each/i
     (lambda (y row)
       (vector-for-each/i
	(lambda (x cell)
	  (if (fmtd-l-style (fmt-v (cell-format cell)))
	      (vector-set! rowsep y 1))
	  (if (fmtd-r-style (fmt-v (cell-format cell)))
	      (vector-set! rowsep (+ y 1) 1)))
	row))
     cells)
    rowsep))

(define (compute-col-seps h w cells)
  (let ((colsep (make-vector (+ w 1) 0)))
    (vector-for-each/i
     (lambda (y row)
       (vector-for-each/i
	(lambda (x cell)
	  (if (fmtd-l-style (fmt-h (cell-format cell)))
	      (vector-set! colsep x 1))
	  (if (fmtd-r-style (fmt-h (cell-format cell)))
	      (vector-set! colsep (+ x 1) 1)))
	row))
     cells)
    colsep))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Matrix Initialization (Parsing and Setup)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (default-render-proc row col cell)
  (if cell
      (if (pair? cell)
	  (values (to-string (car cell))
		  (interp-cell-format (cadr cell)))
	  (to-string cell))
      (values)))

(define (compute-cell str fmt)
  (let* ((lines (string-split str #\newline))
	 (linesv (list->vector lines))
	 (h-margin (+ (fmtd-l-margin (fmt-h fmt))
		      (fmtd-r-margin (fmt-h fmt))))
	 (v-margin (+ (fmtd-l-margin (fmt-v fmt))
		      (fmtd-r-margin (fmt-v fmt)))))
    (if (equal? linesv '#(""))
	(make <cell>
	      cell-w: h-margin
	      cell-h: v-margin
	      cell-format: fmt
	      lines: '#())
	(make <cell>
	      cell-w: (+ h-margin (reduce max 0 (map string-length lines)))
	      cell-h: (+ v-margin (vector-length linesv))
	      cell-format: fmt
	      lines: linesv))))

;;;

(define-class <non-empty-covered-cell> (<condition>)
  cell-x
  cell-y
  cell-contents)

(define-method display-object ((self <non-empty-covered-cell>) port)
  (format port "Matrix cell in row ~d, column ~d is covered\n"
	  (cell-x self)
	  (cell-y self))
  (format port "   by a spanning cell, yet contents ~#@*30s was given.\n"
	  (cell-contents self)))

;;;

(define (matrix-cells h w matrix render-proc)
  (let ((cells (make-vector h))
	(dflt *default-format*)
	(absent-cell $missing-cell)
	(vspan-left (make-vector w 0)))
    ;; fill in the cells
    (vector-for-each/i
     (lambda (y row)
       (let ((crow (make-vector w absent-cell)))
	 (vector-set! cells y crow)
	 ; fill in the left
	 (let loop ((x 0))
	   (if (< x (vector-length row))
	       (let ((c (vector-ref row x)))
		 (if (> (vector-ref vspan-left x) 0)
		     ;; we're covered by a spanning cell
		     (begin
		       (if c
			   (signal (make <non-empty-covered-cell>
					 cell-x: x
					 cell-y: y
					 cell-contents: c)))
		       (loop (+ x 1)))
		     ;; we're exposed
		     (if c
			 (bind ((str fmt (render-proc y x c))
				(cell (compute-cell str (or fmt dflt))))
			   (vector-set! crow x cell)
			   (loop (+ x (cf-vert-span (cell-format cell)))))
			 (loop (+ x 1)))))))))
     matrix)
    cells))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Border Management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; compute the matrix describing borders that run
;;; horizontally (ie, are defined by vertical formatting rules)
;;;

(define (compute-h-border-matrix h w cells)
  (let ((hb (make-matrix (+ h 1) w 0)))
    (matrix-for-each
     cells
     (lambda (y x (cell <cell>))
       (let ((cf (cell-format cell)))
	 (for-each
	  (lambda (along)
	    (let ((s (fmtd-l-style (fmt-v cf))))
	      (if s
		  (matrix-update! hb
				  y (+ x along)
				  (lambda (v) (max v s)))))
	    (let ((s (fmtd-r-style (fmt-v cf))))
	      (if s
		  (matrix-update! hb 
				  (+ y (cf-vert-span cf)) (+ x along)
				  (lambda (v) (max v s))))))
	  (range (cf-horz-span cf))))))
    hb))

(define (compute-v-border-matrix h w cells)
  (let ((vb (make-matrix h (+ w 1) 0)))
    (matrix-for-each
     cells
     (lambda (y x (cell <cell>))
       (let ((cf (cell-format cell)))
	 (for-each
	  (lambda (along)
	    (let ((s (fmtd-l-style (fmt-h cf))))
	      (if s
		  (matrix-update! vb
				  (+ y along) x
				  (lambda (v) (max v s)))))
	    (let ((s (fmtd-r-style (fmt-h cf))))
	      (if s
		  (matrix-update! vb 
				  (+ y along) (+ x (cf-horz-span cf))
				  (lambda (v) (max v s))))))
	  (range (cf-vert-span cf))))))
    vb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Rendition Procedures
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-cell-line cell l outside-w)
  (let* ((cf (cell-format cell))
	 (w (- outside-w
	       (fmtd-l-margin (fmt-h cf))
	       (fmtd-r-margin (fmt-h cf))))
	 (fill (- w (string-length l))))
    (string-append
     (make-string (fmtd-l-margin (fmt-h cf)) #\space)
     (cond
      ((> fill 0)
       (case (fmtd-align (fmt-h cf))
	 ((left top)
	  (string-append l (make-string fill #\space)))
	 ((right bottom)
	  (string-append (make-string fill #\space) l))
	 ((center)
	  (string-append
	   (make-string (quotient (+ fill 1) 2) #\space)
	   l
	   (make-string (quotient fill 2) #\space)))))
      ((= fill 0)
       l)
      ((< fill 0)
       ;; this normally shouldn't happen... do truncation
       (substring l 0 w)))
     (make-string (fmtd-r-margin (fmt-h cf)) #\space))))

;;;
;;;  build a "cell box", which is a vector of `h' strings,
;;;  all of length `w'
;;;
;;; (nb -- our coroutine approach means we don't need to pre-generate
;;;        the lines any more -- we could render them directly!)

(define (build-cell-box cell w h)
  (let ((lines (vector-map
		(lambda (l)
		  (build-cell-line cell l w))
		(lines cell))))
    (let ((vfill (- h (vector-length lines))))
      (cond
       ((= vfill 0)
	lines)
       ((> vfill 0)
	(let ((blank (make-string w #\space)))
	  (case (fmtd-align (fmt-v (cell-format cell)))
	    ((left top)
	     (vector-append lines (make-vector vfill blank)))
	    ((right bottom)
	     (vector-append (make-vector vfill blank) lines))
	    ((center)
	     (vector-append
	      (make-vector (quotient (+ vfill 1) 2) blank)
	      lines
	      (make-vector (quotient vfill 2) blank))))))
       ((< vfill 0)
	;; this normally shouldn't happen either... do truncation
	(subvector lines 0 h))))))

;;; this procedure generates appropriate chars
;;; for ASCII, ie, the canonical #\+, #\|, #\-, #\=

;;;  (there is a font called `block' and/or an encoding called `special'
;;;  on some X systems that I should investigate for providing the
;;;  full assortment of single/double line joins -- this procedure
;;;  could be specialized to provide the appropriate line and corner
;;;  characters;  I wonder if doing it in Unicode would work?  Does
;;;  the Bitsream font have the corner chars?)
;;;
;;;  this procedure could also be speeded up by pre-computing everything
;;;  into a big string -- if we limit styles to { 0, 1, 2 }, then there
;;;  are only 81 entries.

(define (style->border-char l r t b)
  (cond
   ((and (eq? l 0) (eq? r 0) (eq? t 0) (eq? b 0))
    #\space)
   ((and (eq? l 0) (eq? r 0))
    #\|)
   ((and (eq? t 0) (eq? b 0))
    (if (or (> l 1) (> r 1))
	#\=
	#\-))
   (else
    #\+)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Main Control Procedures
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (display-matrix matrix
			#key (render-proc default: default-render-proc)
			     (default-format default: *default-format*))
  (with-default-cell-format default-format
    (lambda ()			   
      (bind ((h w (matrix-dimen matrix))
	     (cells (matrix-cells h w matrix render-proc))
	     (row-seps (compute-row-seps h w cells))
	     (col-seps (compute-col-seps h w cells))
	     (col-ws (compute-col-widths h w cells col-seps))
	     (row-hs (compute-row-heights h w cells row-seps))
	     (h-borders (compute-h-border-matrix h w cells))
	     (v-borders (compute-v-border-matrix h w cells)))
	(draw-matrix h w cells 
		     row-seps col-seps 
		     col-ws row-hs 
		     h-borders v-borders)))))
#|
	(list h w col-seps row-seps 
	      col-ws row-hs
	      h-borders
	      v-borders
	      cells)))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Testing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(define (t)
  (display-matrix
   '#(#((A (default horz-span: 2 horz-align: center))
	#f
	("B\n2\n3" (default vert-span: 2)))
      #(C
	D)
      #(E
	(F (default horz-span: 2))))
   default-format: 'borders))
|#
