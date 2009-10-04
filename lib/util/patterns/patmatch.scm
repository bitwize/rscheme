;; returns some bindings or #f if no match

(define (match-rule rule input)
  (let ((b (make-bindings)))
    (if (pat-match-item (pattern rule) input b)
	(begin
	  (for-each (lambda (extra)
		      (add-extra-binding extra b))
		    (extras rule))
	  b)
	#f)))

(define (add-extra-binding extra-spec b)
  (add-binding! b (car extra-spec) ((cdr extra-spec) b)))

;;;

(define (pattern-var? (s <symbol>))
  (eq? #\? (string-ref (symbol->string s) 0)))

(define (var-restriction (s <symbol>))
  (let ((a (assq s '((?k . #\n)
		     (?l . #\n)
		     (?m . #\n)
		     (?n . #\n)
		     (?qu . #\q)
		     (?qv . #\q)
		     (?qw . #\q)))))
    (if a
	(cdr a)
	(let (((str <string>) (symbol->string s)))
	  (if (and (eq? #\? (string-ref str 0))
		   (eq? #\: (string-ref str (- (string-length str) 2))))
	      (string-ref str (- (string-length str) 1))
	      #f)))))

(define (constant? inp)
  (or (number? inp)
      (string? inp)
      (boolean? inp)
      (char? inp)
      (and (pair? inp) (eq? (car inp) 'quote))))

(define (constant-value inp)
  (if (and (pair? inp)
           (eq? (car inp) 'quote))
      (cadr inp)
      inp))

(define (var-match-restriction? (s <symbol>) inp)
  (case (var-restriction s)
    ((#\n) (or (number? inp)
	       (and (constant? inp)
		    (number? (constant-value inp)))))
    ((#\q) (constant? inp))
    ((#\s) (symbol? inp))
    ((#f) #t)
    (else (error "invalid var restriction: ~s"
		 (var-restriction s)))))

;;; we tend to use `$foo' for constants...

(define (gensym-var? (s <symbol>))
  (and (> (string-length (symbol->string s)) 1)
       (eq? #\_ (string-ref (symbol->string s) 0))))

;;;

(define (pat-match-item pattern input b)
  ;(format #t "pat-match-item: ~#*60s\n" pattern)
  ;(format #t "        against ~#*60s\n" input)
  (cond
   ((pair? pattern)
    ;; lists must be have the same contents
    (pat-match-list pattern input b))
   ((symbol? pattern)
    (cond
     ((pattern-var? pattern)
      (if (var-match-restriction? pattern input)
	  (merge-binding b pattern input)
	  #f))
     (else
      ;; symbols must be eq
      (eq? pattern input))))
   (else
    ;; everything else must be equivalent
    (equal? pattern input))))

;;; if the given key is already present, check to make sure
;;; it is equal (a primitive unification).  Otherwise, add it
;;; and return #t

(define (merge-binding b key value)
  (bind ((#rest r (binding-lookup b key)))
    (if (null? r)
	(begin
	  (add-binding! b key value)
	  #t)
	(equal? (car r) value))))

;;;

(define (pat-match-list pattern input b)
  (cond
   ((null? pattern)
    (null? input))
   ((and (symbol? pattern)
	 (pattern-var? pattern))
    (pat-match-repeat pattern input b))
   ; support `...' notation also
   ((and (pair? pattern)
	 (pair? (cdr pattern))
	 (eq? (cadr pattern) '...)
	 (null? (cddr pattern)))
    (pat-match-repeat (car pattern) input b))
   ;
   ((pair? pattern)
    (and (pair? input)
	 (pat-match-item (car pattern) (car input) b)
	 (pat-match-list (cdr pattern) (cdr input) b)))
   (else
    #f)))

;;;


;;;----------------------------------------------------------------------
;;;   support for Scheme-like `...' patterns

(define (pat-match-repeat repeating input b)
  (if (null? input)
      ;; matched 0 times -- bind each pattern var to the empty list
      (null-ddd-results (collect-pattern-vars repeating) b)
      (let loop ((blist '())
		 (in input))
	(if (null? in)
	    (merge-ddd-results (reverse blist) b)
	    ;; create our own bindings context for each iteration
	    ;; (note that this means you can't unify across a `...' boundary)
	    (if (pair? in)
		(let ((b (make-bindings)))
		  (if (pat-match-item repeating (car in) b)
		      (loop (cons b blist) (cdr in))
		      #f))
		#f)))))

(define (null-ddd-results vars b)
  (let loop ((i 0))
    (if (< i (vector-length vars))
	(and (merge-binding b (vector-ref vars i) '#())
	     (loop (+ i 1))))))

(define (merge-ddd-results blist b)
  ;(format #t "merge-ddd-results: ~s\n" b)
  (let* ((vv (vector-map (lambda (b)
			   (dequeue-state b))
			 (list->vector blist)))
	 ((v0 <vector>) (vector-ref vv 0)))
    (let loop ((i 0))
      (if (< i (vector-length v0))
	  (let ((lst (let ((j (add1 i)))
		       (vector-map (lambda (v)
				     (vector-ref v j))
				   vv))))
	    (and (merge-binding b (vector-ref v0 i) lst)
		 (loop (+ i 2))))
	  #t))))

