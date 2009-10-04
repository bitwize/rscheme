
;;

(define gentmprule
  (let ((n -1))
    (lambda ()
      (set! n (+ n 1))
      (symbol-append "T_" n))))
;;

(define-class <production> (<object>)
  (non-terminal type: <symbol> :sealed)
  (hash-value type: <fixnum> init-function: random :sealed)
  (right-hand-side type: <list>)
  (items type: <vector> init-value: '#())
  (action type: <function> :sealed))

(define-class <item> (<object>)
  (hash-value type: <fixnum> init-value: 0 :sealed)
  (production type: <production> :sealed)
  (position type: <fixnum> :sealed)
  (rest-of-rhs type: <list> :sealed))

(define-method initialize ((self <item>))
  (set-hash-value! self (bitwise-xor (integer->hash (position self))
				     (hash-value (production self)))))

(define-method initialize ((self <production>))
  (let* ((rhs (right-hand-side self))
	 (n (length (right-hand-side self)))
	 (v (make-vector (+ n 1))))
    (set-items! self v)
    (for-each (lambda (i)
		(vector-set! v
			     i
			     (make <item>
				   production: self
				   position: i
				   rest-of-rhs: (list-tail rhs i))))
	      (range (+ n 1)))))
  
(define (add-production! envt (p <production>))
  (let ((n (non-terminal p)))
    (table-insert! envt n (cons p (or (table-lookup envt n) '())))
    p))

(define (pass-envt proc)
  (lambda args
    (let ((val (apply proc args)))
      (lambda (envt)
	(values envt val)))))

(define (eval-rule rule envt)
  (let ((nt (cadr rule))
	(action-args (cadr (list-ref rule 3)))
	(action-body (cddr (list-ref rule 3))))
    (add-production!
     envt 
     (make <production>
	   non-terminal: nt
	   action: (eval `(lambda ,action-args
			    (let ((meaning (begin ,@action-body)))
			      #|(format #t "~s means => ~s\n"
				      ',nt
				      meaning)|#
			      (lambda (ct-envt)
				(values ct-envt meaning)))))
	   right-hand-side: (eval-rhs (caddr rule) envt)))))

(define (eval-seq items envt)
  (apply append (map (rcurry eval-rhs envt) items)))

;;;
;;;  terminals are denoted by procedures which recognize them
;;;  and return their meaning if found, or #f if not found

(define (make-string-terminal expr)
  (let ((f (lambda (token)
	     (if (string? token)
		 token
		 #f))))
    f))

(define (make-stag-terminal expr)
  (let ((f (lambda (token)
	     (if (and (pair? token)
		      (eq? (car token) 'stag)
		      (eq? (cadr token) (cadr expr)))
		 token
		 #f))))
    (set-template! f (clone (template f)))
    (set-function-descr! (template f) `((function-scope ,(cadr expr) stag)))
    f))

(define (make-etag-terminal expr)
  (let ((f (lambda (token)
	     (if (and (pair? token)
		      (eq? (car token) 'etag)
		      (eq? (cadr token) (cadr expr)))
		 token
		 #f))))
    (set-template! f (clone (template f)))
    (set-function-descr! (template f) `((function-scope ,(cadr expr) etag)))
    f))

;(define-method write-object ((self <closure>) port)
;  (format port "[~a]" (name self)))

;;;

(define (eval-rhs expr envt)
  (if (pair? expr)
      (case (car expr)
	;; terminals
	((string)
	 (list (make-string-terminal expr)))
	((stag)
	 (list (make-stag-terminal expr)))
	((etag)
	 (list (make-etag-terminal expr)))
	;; compositions
	((sequence)
	 (eval-seq (cdr expr) envt))
	((or)
	 (gen-or-node expr envt))
	((repeat)
	 (gen-repeat-node expr envt))
	((optional)
	 (gen-optional-node expr envt))
	(else (error "bad metagrammar: ~s" expr)))
      (list expr)))

(define (gen-or-node rpt envt)
  (let ((t (gentmprule)))
    (for-each
     (lambda (alt)
       (add-production! envt (make <production>
				   non-terminal: t
				   action: (pass-envt list)
				   right-hand-side: (eval-rhs alt envt))))
     (cdr rpt))
    (list t)))

(define (gen-repeat-node rpt envt)
  (let ((t (gentmprule))
	(x (gentmprule)))
    (add-production! envt (make <production>
				non-terminal: t
				action: (pass-envt (lambda () '()))
				right-hand-side: '()))
    (add-production! envt (make <production>
				non-terminal: t
				action: (pass-envt cons)
				right-hand-side: `(,x ,t)))
    (add-production! envt (make <production>
				non-terminal: x
				action: (pass-envt list)
				right-hand-side: (eval-seq (cdr rpt) envt)))
    (list t)))


(define (gen-optional-node opt envt)
  (let ((t (gentmprule)))
    (add-production! envt (make <production>
				non-terminal: t
				action: (pass-envt (lambda () #f))
				right-hand-side: '()))
    (add-production! envt (make <production>
				non-terminal: t
				action: (pass-envt list)
				right-hand-side: (eval-seq (cdr opt) envt)))
    (list t)))

(define (eval-gram G)
  (let ((t (make-symbol-table)))
    (for-each (rcurry eval-rule t) G)
    t))

(define (lookup envt nt)
  (table-lookup envt nt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <parser-state> (<object>)
  (label type: <fixnum> init-value: 0) ;; for debugging
  (hash-value :sealed type: <fixnum> init-value: 777) ;; for performance
  (input-stream type: <list>)
  (scan-tuples      :sealed type: <list> init-value: '())
  (active-tuples    :sealed type: <list> init-value: '())
  (complete-tuples  :sealed type: <list> init-value: '()))

(define-class <tuple> (<object>)
  (item type: <item> :sealed)
  (parent-state :sealed #| type: (union #f <parser-state>)|#)
  (envt :sealed)
  (accum type: <list> init-value: '()))

(define-method hash-value ((self <tuple>))
  (bitwise-and
   #xFFFFFF
   (* (hash-value (item self)) 
      (hash-value (parent-state self)))))

(define (matches-nt? (self <tuple>) nt)
  (let ((rrhs (rest-of-rhs (item self))))
    (and (not (null? rrhs))
	 (eq? (car rrhs) nt))))

(define (complete-tuple? (self <tuple>))
  (null? (rest-of-rhs (item self))))

(define (scannable-tuple? (self <tuple>))
  (terminal? (car (rest-of-rhs (item self)))))

;;;

(define (terminal-matches? term tok)
  (if (string? term)
      (string=? term tok)
      (case term
	((string) #t))))
      
(define (terminal? x)
  (or (string? x)
      (eq? x 'string)))

;;;

(define (matches-input? (self <tuple>) tok)
  (let ((rrhs (rest-of-rhs (item self))))
    (if (and (pair? rrhs)
	     (terminal? (car rrhs)))
	(and (terminal-matches? (car rrhs) tok) #t)
	#f)))

(define (run-completer (self <tuple>) visit)
  (let* ((prod (production (item self)))
	 (nt (non-terminal prod))
	 ;; this `means' is a function mapping an advancing tuple's
	 ;; environment to a new environment and accumuland (two values)
	 (means (apply (action prod) (reverse (accum self)))))
    (let loop ((a (active-tuples (parent-state self))))
      (if (null? a)
	  (values)
	  (let (((t <tuple>) (car a)))
	    (if (eq? (car (rest-of-rhs (item t))) nt)
		(visit (advance-tuple t means)))
	    (loop (cdr a)))))))

(define *next-token* #f)

(define (run-predictor (self <tuple>) in-state visit)
  (let* ((in-envt (envt self))
	 (next (car (rest-of-rhs (item self)))))
    (let ((productions (lookup in-envt next)))
      (if productions
	  (for-each (lambda ((p <production>))
		      (visit (make <tuple>
				   item: (vector-ref (items p) 0)
				   envt: in-envt
				   parent-state: in-state)))
		    productions)
	  (error "~s: not defined" next)))))

;;;

(define (tuple=? (a <tuple>) (b <tuple>))
  (and (eq? (item a) (item b))
       (eq? (parent-state a) (parent-state b))
       (eq? (envt a) (envt b))))

;;;

(define *num-live-tuples-scan* 0)
(define *num-live-tuples-active* 0)
(define *num-live-tuples-complete* 0)

(define-syntax (inc! n)
  (set! n (+ n 1)))

(define *parser-states* '())

(define (curtab) "  >")

(define (make-parser-state (initial <list>) strm label)
  (if (null? initial)
      (error "can't proceed at: ~#*@50s" strm))
  (let (((ps <parser-state>) (make <parser-state>
				   label: label
				   hash-value: (random)
				   input-stream: strm))
	(seen (make-table tuple=? hash-value))
	(completion-q '())
	(prediction-q '()))
    (set! *parser-states* (cons ps *parser-states*))
    ; [also] in one special case (bootstrap), we have to update the
    ; initial tuple's parent to point to the new state
    (if (and (pair? initial)
	     (not (parent-state (car initial))))
	(set-parent-state! (car initial) ps))
    ;
    (if (pair? strm)
	(set! *next-token* (car strm)))
    ;
    (define (visit (tup <tuple>))
      ;(format #t "visit: ~s -- ~s\n" tup (table-lookup seen tup))
      (if (and (table-key-present? seen tup)
	       (not (complete-tuple? tup)))
	  (values)
	  ;; could prune here according to the PEEK in the strm, right?
	  ;; (that would save the useless closure of unmatchable rules...
	  ;; on the other hand, there IS no closure from anything that
	  ;; we won't match anyway, since those are leaves in the closure
	  ;; computation)
	  (begin
	    (table-insert! seen tup tup)
	    ;(format #t "  visiting ~s... ~s\n" tup (table-lookup seen tup))
	    ;(print seen)
	    (cond
	     ((complete-tuple? tup)
	      ;(format #t "~acomplete: ~s\n" (curtab) tup)
	      (inc! *num-live-tuples-complete*)
	      ;; this info is never needed...
	      ;;except when we're done and back-filling the parse
	      (set-complete-tuples! ps (cons tup (complete-tuples ps)))
	      (run-completer tup visit))
	     ((scannable-tuple? tup)
	      ;(format #t "~ascannable: ~s\n" (curtab) tup)
	      (inc! *num-live-tuples-scan*)
	      (set-scan-tuples! ps (cons tup (scan-tuples ps)))
	      ;; don't queue it
	      )
	     (else
	      ;(format #t "~aactive: ~s\n" (curtab) tup)
	      (inc! *num-live-tuples-active*)
	      (set-active-tuples! ps (cons tup (active-tuples ps)))
	      (run-predictor tup ps visit))))))
    ;
    (for-each visit initial)
    ps))

;;;

(define (next-item (self <item>))
  (let ((p (production self)))
    (vector-ref (items p) (+ (position self) 1))))

(define (advance-tuple (self <tuple>) proc)
  (bind ((new-envt val (proc (envt self))))
   (make <tuple>
	 item: (next-item (item self))
	 parent-state: (parent-state self)
	 envt: new-envt
	 accum: (cons val (accum self)))))

(define (advance-input (self <parser-state>))
  (let* ((tok (car (input-stream self))))
    (make-parser-state
     (map (rcurry advance-tuple (lambda (e) (values e tok)))
	  (select (lambda (t)
		    (matches-input? t tok))
		  (scan-tuples self)))
     (cdr (input-stream self))
     (+ (label self) 1))))

;-----------------------------------------------------------------------

#|
(define-method write-object ((self <tuple>) port)
  (format port "#~d-~a-@~d" 
	  (hash-value self)
	  (item self) 
	  (label (parent-state self))))

(define-method display-object ((self <item>) port)
  (format port "[ ~a ->" (non-terminal (production self)))
  (let loop ((i 0)
	     (r (right-hand-side (production self))))
    (if (eq? i (position self))
	(format port " ."))
    (if (null? r)
	(format port " ]")
	(begin
	  (if (pair? (car r))
	      (format port " ~s" (cdar r))
	      (format port " ~s" (car r)))
	  (loop (+ i 1) (cdr r))))))
	

(define-method print ((self <parser-state>))
  (format #t "--- PARSER STATE ~d ---\n" (label self))
  (let ((k 0))
    (define (print-tup t)
      (format #t "  ~d.  ~a  (from ~a)\n" 
	      k
	      (item t)
	      (label (parent-state t)))
      (set! k (+ k 1)))
    ;
    (for-each print-tup (scan-tuples self))
    (for-each print-tup (complete-tuples self))
    (for-each print-tup (active-tuples self))
    self))
|#

;-----------------------------------------------------------------------

(define (parse-tree (self <parser-state>))
  (let* ((s (car (lookup *grammar* 'start)))
	 (i (vector-ref (items s) (- (vector-length (items s)) 1)))
	 (finals (select (lambda (t)
			   (and (eq? (item t) i)
				(eq? (label (parent-state t)) 0)))
			 (complete-tuples self))))
    (case (length finals)
      ;; note that this returns a list of the meanings of the `start'
      ;; rule, so if there is only one elem in the RHS, you'll need
      ;; a `car', too
      ((1) (accum (car finals)))
      ((0) (values))
      (else 'ambiguous))))

(define *grammar*
  (eval-gram
   '((rule start expr
	   (lambda (e)
	     e))
     (rule expr (sequence string "=" string)
	   (lambda (s eq v)
	     (list '= (string->symbol s) v)))
     (rule expr (sequence expr "and" expr)
	   (lambda (a and b)
	     (list 'and a b)))
     (rule expr (sequence expr "or" expr)
	   (lambda (a or b)
	     (list 'or a b)))
     (rule expr (sequence "not" expr)
	   (lambda (n x)
	     (list 'not x)))
     (rule expr (sequence string "in" "(" (repeat string) ")")
	   (lambda (slot $1 < lst >)
	     (list 'member (string->symbol slot) (map car lst))))
     ;; a predicate, like `in-snapshot?'
     (rule expr string
	   (lambda (p)
	     (list '= (string->symbol p) "true")))
     (rule expr (sequence "(" expr ")")
	   (lambda (< x >)
	     x)))))

(define (parse-where-clause clause)
  (let* (((start <production>) (car (lookup *grammar* 'start)))
	 (t (make <tuple>
		  item: (vector-ref (items start) 0)
		  parent-state: #f
		  envt: *grammar*)))
    (let loop ((pstate (make-parser-state (list t) clause 0)))
      ;(print pstate)
      (if (null? (input-stream pstate))
	  (let ((r (parse-tree pstate)))
	    (if r
		(car r)
		#f))
	  (begin
	    ;(format #t "consuming: ~s\n" (car (input-stream pstate)))
	    (loop (advance-input pstate)))))))

;;;

#|
,(use debugger)
,(trace tuple=?)
(define (t)
  (parse-where-clause '("user" "=" "donovan")))
|#
