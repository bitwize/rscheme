
;;

(define gentmprule
  (let ((n -1))
    (lambda ()
      (set! n (+ n 1))
      (symbol-append "T_" n))))
;;

(define *next-hash-code* 1)

(define-constant (assign-hash-code)
  (let ((n *next-hash-code*))
    (set! *next-hash-code* (+ *next-hash-code* 1))
    n))

(define-class <production> (<object>)
  (non-terminal type: <symbol> :sealed)
  (hash-code type: <fixnum> init-function: assign-hash-code :sealed)
  (right-hand-side type: <list>)
  (items type: <vector> init-value: '#())
  (action type: <function> :sealed))

(define-class <item> (<object>)
  (hash-code type: <fixnum> init-value: 0 :sealed)
  (production type: <production> :sealed)
  (position type: <fixnum> :sealed)
  (rest-of-rhs type: <list> :sealed))

(define-method initialize ((self <item>))
  (set-hash-code! self (bitwise-xor (integer->hash (position self))
				    (hash-code (production self)))))

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

(define-method hash-code-rec ((self <object>))
  (hash-code self))

(define-method hash-code-rec ((self <vector>))
  (hash-code-rec (vector->list self)))

(define-method hash-code-rec ((self <pair>))
  (bitwise-and (* (hash-code-rec (car self))
		  (hash-code (hash-code-rec (cdr self))))
	       #x1FFFFFFF))

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

(define (rename-procedure proc name)
  ;; disabled for now -- rs.lang doesn't export what we need
  proc)

(define (make-stag-terminal expr)
  (rename-procedure
   (lambda (token)
     (if (and (pair? token)
	      (eq? (car token) 'stag)
	      (eq? (cadr token) (cadr expr)))
	 token
	 #f))
   (list (cadr expr) 'stag)))

(define (make-etag-terminal expr)
  (rename-procedure
   (lambda (token)
     (if (and (pair? token)
	      (eq? (car token) 'etag)
	      (eq? (cadr token) (cadr expr)))
	 token
	 #f))
      (list (cadr expr) 'etag)))

(define-method write-object ((self <function>) port)
  (format port "[~a]" (name self)))

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
  (hash-code :sealed type: <fixnum> init-value: 777) ;; for performance
  (input-stream type: <list>)
  (scan-tuples      :sealed type: <list> init-value: '())
  (active-tuples    :sealed type: <list> init-value: '())
  (complete-tuples  :sealed type: <list> init-value: '()))

(define-class <tuple> (<object>)
  (item type: <item> :sealed)
  (parent-state :sealed #| type: (union #f <parser-state>)|#)
  (envt :sealed)
  (accum type: <list> init-value: '()))

(define-method hash-code ((self <tuple>))
  (* (hash-code (item self)) (hash-code (parent-state self))))

(define (matches-nt? (self <tuple>) nt)
  (let ((rrhs (rest-of-rhs (item self))))
    (and (not (null? rrhs))
	 (eq? (car rrhs) nt))))

(define (complete-tuple? (self <tuple>))
  (null? (rest-of-rhs (item self))))

(define (scannable-tuple? (self <tuple>))
  (terminal? (car (rest-of-rhs (item self)))))

(define (terminal? x)
  (procedure? x))

(define (matches-input? (self <tuple>) tok)
  (let ((rrhs (rest-of-rhs (item self))))
    (if (and (pair? rrhs)
	     (terminal? (car rrhs)))
	(and ((car rrhs) tok) #t)
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

;...

(define-thread-var *tablen* 1)

(define (curtab)
  (string-append (make-string *tablen* #\space) "> "))

(define-syntax (tab expr)
  (thread-let ((*tablen* (+ *tablen* 2)))
    expr))

;...

(define (make-parser-state (initial <list>) strm label)
  (if (null? initial)
      (error "can't proceed at: ~#*@50s" strm))
  (let (((ps <parser-state>) (make <parser-state>
				   label: label
				   hash-code: (assign-hash-code)
				   input-stream: strm))
	(seen (make-table tuple=?))
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
      (if (and (table-key-present? seen tup)
	       (not (complete-tuple? tup)))
	  (begin
	    ;(format #t "~aalready seen: ~s\n" (curtab) tup)
	    (values))
	  ;; could prune here according to the PEEK in the strm, right?
	  ;; (that would save the useless closure of unmatchable rules...
	  ;; on the other hand, there IS no closure from anything that
	  ;; we won't match anyway, since those are leaves in the closure
	  ;; computation)
	  (begin
	    (table-insert! seen tup tup)
	    (cond
	     ((complete-tuple? tup)
	      ;(format #t "~acomplete: ~s\n" (curtab) tup)
	      (inc! *num-live-tuples-complete*)
	      ;; this info is never needed...
	      ;;except when we're done and back-filling the parse
	      (set-complete-tuples! ps (cons tup (complete-tuples ps)))
	      (tab
	       (run-completer tup visit)))
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
	      (tab
	       (run-predictor tup ps visit)))))))
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


(define-method write-object ((self <tuple>) port)
  (format port "#-~a-@~d" (item self) (label (parent-state self))))

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

;;;

(define (parse-using-grammar input grammar)
  (let* (((start <production>) (car (lookup grammar 'start)))
	 (t (make <tuple>
		  item: (vector-ref (items start) 0)
		  parent-state: #f
		  envt: grammar)))
    (let loop ((pstate (make-parser-state (list t) input 0)))
      ;(print pstate)
      (if (null? (input-stream pstate))
	  (car (parse-tree pstate))
	  (begin
	    ;(format #t "consuming: ~s\n" (car (input-stream pstate)))
	    (loop (advance-input pstate)))))))
