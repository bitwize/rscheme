
;;

(define-syntax (gentmprule) (gensym))

;;

(define-class <grammar> (<object>)
  (table type: <symbol-table> :sealed))

(define-syntax (lookup envt nt)
  (table-lookup (table envt) nt))

;;;

(define-class <production> (<object>)
  (non-terminal type: <symbol> :sealed)
  (hash-code type: <fixnum> :sealed)
  (right-hand-side type: <list>)
  (items type: <vector> init-value: '#())
  (action type: <function> :sealed))

(define-class <item> (<object>)
  (hash-code type: <fixnum> init-value: 0 :sealed)
  (production type: <production> :sealed)
  (position type: <fixnum> :sealed)
  (rest-of-rhs type: <list> :sealed))

(define-method initialize ((self <item>))
  (set-hash-code! self (tuple->hash (position self)
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
   (tuple->hash (hash-code-rec (car self))
                (hash-code-rec (cdr self))))

(define (null-action)
  (lambda (ct-envt)
    (values ct-envt '())))

(define (cons-action first rest)
  (lambda (ct-envt)
    (values ct-envt (cons first rest))))

(define (multi-list-action . args)
  (lambda (ct-envt)
    (values ct-envt (list args))))

(define (list-action . args)
  (lambda (ct-envt)
    (values ct-envt args)))


(define (unit-action x)
  (lambda (ct-envt)
    (values ct-envt x)))

(define (multi-action . args)
  (lambda (ct-envt)
    (values ct-envt args)))

(define (make-literal-action value)
  (lambda args
    (lambda (ct-envt)
      (values ct-envt value))))

(define (make-callback-action (cb <function>))
  (lambda args
    (bind ((result new-envt (apply cb args)))
      (if new-envt
          (lambda (ct-envt)
            (values new-envt result))
          (lambda (ct-envt)
            (values ct-envt result))))))

(define (eval-rule rule envt pure-parse-tree?)
  (let ((nt (car rule)))
    (add-production!
     envt 
     (make <production>
           non-terminal: nt
           hash-code: (string->hash (string-append
                                     (symbol->string nt)
                                     " "
                                     (machine-bits->string rule)))
           action: (if pure-parse-tree?
                       (make-callback-action
                        (lambda args
                          (cons nt args)))
                       (if (pair? (cddr rule))
                           ;; it's a rule with an action
                           (if (procedure? (caddr rule))
                               (make-callback-action (caddr rule))
                               (make-literal-action (caddr rule)))
                           (case (length (cadr rule))
                             ((0) null-action)
                             ((1) unit-action)
                             (else multi-action))))
           right-hand-side: (eval-seq (cadr rule) envt)))))
    
(define (eval-seq items envt)
  (apply append (map (lambda (elem)
                       (eval-rhs elem envt))
                     items)))

;;;
;;;  terminals are denoted by procedures which recognize them
;;;  and return their meaning if found, or #f if not found

(define (make-terminal term)
  (if (procedure? term)
      term
      (lambda (token)
        (if (equal? token term)
            token
            #f))))

(define (eval-rhs expr envt)
  (cond
   ((pair? expr)
    (case (car expr)
      ;; terminals
      ((terminal)
       (list (make-terminal (cadr expr))))
      ;; compositions
      ((sequence)
       (eval-seq (cdr expr) envt))
      ((or)
       (gen-or-node expr envt))
      ((+ repeat+)
       (gen-repeat+-node expr envt))
      ((* repeat)
       (gen-repeat-node expr envt))
      ((? optional)
       (gen-optional-node expr envt))
      (else (error "bad metagrammar: ~s" expr))))
   ((char? expr)
    (list (make-terminal expr)))
   ((symbol? expr)
    (list expr))
   ((null? expr)
    '())
   (else
    (error "bad metagrammar: ~s" expr))))

(define (gen-or-node rpt envt)
  (let ((t (gentmprule)))
    (for-each
     (lambda (alt)
       (let ((rhs (eval-rhs alt envt)))
         (add-production! envt (make <production>
                                     hash-code: (string->hash 
                                                 (string-append
                                                  "\\or "
                                                  (machine-bits->string rpt)))
                                     non-terminal: t
                                     action: (case (length rhs)
                                               ((0) null-action)
                                               ((1) unit-action)
                                               (else multi-action))
                                     right-hand-side: rhs))))
     (cdr rpt))
    (list t)))

(define (gen-repeat-node rpt envt)
  (let ((t (gentmprule))
	(x (gentmprule))
        (body (eval-seq (cdr rpt) envt)))
   (add-production! envt (make <production>
                                hash-code: (string->hash 
                                            (string-append
                                             "\\*0 "
                                             (machine-bits->string rpt)))
				non-terminal: t
				action: null-action
				right-hand-side: '()))
    (add-production! envt (make <production>
                                hash-code: (string->hash 
                                            (string-append
                                             "\\*1 "
                                             (machine-bits->string rpt)))
				non-terminal: t
				action: cons-action
				right-hand-side: `(,x ,t)))
    (add-production! envt (make <production>
                                hash-code: (string->hash 
                                            (string-append
                                             "\\*2 "
                                             (machine-bits->string rpt)))
				non-terminal: x
				action: (case (length body)
                                          ((1) unit-action)
                                          (else multi-action))
				right-hand-side: body))
    (list t)))

(define (gen-repeat+-node rpt envt)
  (let ((t (gentmprule))
	(x (gentmprule))
        (body (eval-seq (cdr rpt) envt)))
    (add-production! envt (make <production>
                                hash-code: (string->hash 
                                            (string-append
                                             "\\+0 "
                                             (machine-bits->string rpt)))
				non-terminal: t
				action: (case (length body)
                                          ((1) list-action)
                                          (else multi-list-action))
				right-hand-side: `(,x)))
    (add-production! envt (make <production>
                                hash-code: (string->hash 
                                            (string-append
                                             "\\+1 "
                                             (machine-bits->string rpt)))
				non-terminal: t
				action: cons-action
				right-hand-side: `(,x ,t)))
    (add-production! envt (make <production>
                                hash-code: (string->hash 
                                            (string-append
                                             "\\+2 "
                                             (machine-bits->string rpt)))
				non-terminal: x
				action: (case (length body)
                                          ((1) unit-action)
                                          (else multi-action))
				right-hand-side: body))
    (list t)))


(define (gen-optional-node opt envt)
  (let ((t (gentmprule)))
    (add-production! envt (make <production>
                                hash-code: (string->hash 
                                            (string-append
                                             "\\?0 "
                                             (machine-bits->string opt)))
				non-terminal: t
				action: (pass-envt (lambda () #f))
				right-hand-side: '()))
    (add-production! envt (make <production>
                                hash-code: (string->hash 
                                            (string-append
                                             "\\?1 "
                                             (machine-bits->string opt)))
				non-terminal: t
				action: (pass-envt list)
				right-hand-side: (eval-seq (cdr opt) envt)))
    (list t)))

;;;
;;;  <ruleset> ::= ( <rule> ... )
;;;  <rule> ::= ( <NON-TERMINAL> <rhs> )
;;;          |  ( <NON-TERMINAL> <rhs> <action> )
;;;
;;;  <rhs> ::= ( <item> ... )
;;;  <item> ::= ( terminal <PROCEDURE> )        ; match by calling PROCEDURE
;;;          |  ( terminal <OBJECT> )           ; match using `equal?'
;;;          |  ( sequence <item> ... )         ; value is list (???)
;;;          |  ( or <item> ... )               ; value is the matched <item>
;;;          |  ( + <item> )                    ; value is list of <item> vals
;;;          |  ( * <item> )                    ; value is list of <item> vals
;;;          |  ( + <item> ... )                ; value is list of lists
;;;          |  ( * <item> ... )                ; value is list of lists
;;;          |  ( ? <item> ... )                ; value is #f, or list (???)
;;;          |  <CHAR>                          ; same as (terminal <CHAR>)
;;;          |  <NON-TERMINAL>
;;;
;;;  <action> ::= <PROCEDURE>           ; rule calls <PROCEDURE> (*) when fired
;;;            |  <OBJECT>              ; rule evaluates to <OBJECT>
;;;
;;; (*) PROCEDURE is called with the values of the <rhs> items, and
;;;     returns two values: (1) the result value, and (2) the new
;;;     parse environment

(define (make-grammar ruleset #optional pure-parse-tree?)
  (let ((g (make <grammar>
                 table: (make-symbol-table))))
    (for-each (lambda (r)
                (eval-rule r (table g) pure-parse-tree?))
              ruleset)
    g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <parser-state> (<object>)
  (in-grammar type: <grammar> :sealed)
  (label type: <fixnum> init-value: 0) ;; for debugging
  (hash-code :sealed type: <fixnum> init-value: 777) ;; for performance
  (current-token :sealed)
  (input-stream type: <function> :sealed)
  (scan-tuples      :sealed type: <list> init-value: '())
  (active-tuples    :sealed type: <list> init-value: '())
  (complete-tuples  :sealed type: <list> init-value: '()))

(define-class <tuple> (<object>)
  (item type: <item> :sealed)
  (parent-state :sealed #| type: (union #f <parser-state>)|#)
  (envt :sealed)
  (accum type: <list> init-value: '()))

(define-method hash-code ((self <tuple>))
  (check-fixnum
   (bitwise-and
    (* (hash-code (item self)) (hash-code (parent-state self)))
    #x1FFFFFFF)))

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

(define (scan-meaning (self <tuple>) token)
  (let ((rrhs (rest-of-rhs (item self))))
    (if (and (pair? rrhs)
	     (terminal? (car rrhs)))
	((car rrhs) token)
	#f)))

#|
(define (matches-input? (self <tuple>) tok)
  (let ((rrhs (rest-of-rhs (item self))))
    (if (and (pair? rrhs)
	     (terminal? (car rrhs)))
	(and ((car rrhs) tok) #t)
	#f)))
|#

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

;(define *parser-states* '())

;...

(define-thread-var *tablen* 1)

(define (curtab)
  (string-append (make-string *tablen* #\space) "> "))

(define-syntax (tab expr)
  (thread-let ((*tablen* (+ *tablen* 2)))
    expr))

;...

(define (make-parser-state g (initial <list>) token (strm <function>) label)
  (if (null? initial)
      (error "can't proceed at: ~#*@50s" token))
  (let (((ps <parser-state>) (make <parser-state>
                                   in-grammar: g
				   label: label
				   hash-code: (tuple->hash #x5555 label)
                                   current-token: token
				   input-stream: strm))
	(seen (make-table tuple=?))
	(completion-q '())
	(prediction-q '()))
    ;(set! *parser-states* (cons ps *parser-states*))
    ; [also] in one special case (bootstrap), we have to update the
    ; initial tuple's parent to point to the new state
    (if (and (pair? initial)
	     (not (parent-state (car initial))))
	(set-parent-state! (car initial) ps))
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

(define-class <parse-error> (<condition>)
  previous-state
  next-token)

(define-method parse-error-at-token ((self <parse-error>))
  (next-token self))

(define-method terminal-name ((self <object>))
  name)

(define-method parse-error-expecting ((self <parse-error>))
  (map (lambda ((t <tuple>))
         (terminal-name (car (rest-of-rhs (item t)))))
       (scan-tuples (previous-state self))))
  
(define (advance-input (self <parser-state>) token)
  (let loop ((next '())
             (src (scan-tuples self)))
    (if (null? src)
        (if (null? next)
            (signal (make <parse-error>
                          previous-state: self
                          next-token: token))
            (make-parser-state (in-grammar self)
                               (reverse! next)
                               token
                               (input-stream self)
                               (+ (label self) 1)))
        (bind ((m new-envt (scan-meaning (car src) token)))
          (loop (if m
                    (cons (advance-tuple (car src)
                                         (if new-envt
                                             (lambda (e)
                                               (values new-envt m))
                                             (lambda (e)
                                               (values e m))))
                          next)
                    next)
                (cdr src))))))

;-----------------------------------------------------------------------


(define-method write-object ((self <tuple>) port)
  (__format port "#-~a-@~d" (item self) (label (parent-state self))))

(define-method display-object ((self <item>) port)
  (__format port "[ ~a ->" (non-terminal (production self)))
  (let loop ((i 0)
	     (r (right-hand-side (production self))))
    (if (eq? i (position self))
	(__format port " ."))
    (if (null? r)
	(__format port " ]")
	(begin
	  (if (pair? (car r))
	      (__format port " ~s" (cdar r))
	      (__format port " ~s" (car r)))
	  (loop (+ i 1) (cdr r))))))
	

(define-method print ((self <parser-state>))
  (__format #t "--- PARSER STATE ~d ---\n" (label self))
  (let ((k 0))
    (define (print-tup t)
      (__format #t "  ~d.  ~a  (from ~a)\n" 
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
  (let* ((s (car (lookup (in-grammar self) 'start)))
	 (i (vector-ref (items s) (- (vector-length (items s)) 1)))
	 (finals (select (lambda (t)
			   (and (eq? (item t) i)
				(eq? (label (parent-state t)) 0)))
			 (complete-tuples self)))
         (a (map accum finals)))
    (case (length finals)
      ;; note that this returns a list of the meanings of the `start'
      ;; rule, so if there is only one elem in the RHS, you'll need
      ;; a `car', too
      ((1) (values (car a) 'success))
      ((0) (values #f 'failed '()))
      (else (values (car a) 'ambiguous a)))))

;;;

(define-syntax (eof-token? token)
  (not token))


(define (parse-using-grammar* (input <function>) (grammar <grammar>))
  (let* (((start <production>) (car (lookup grammar 'start)))
	 (t (make <tuple>
		  item: (vector-ref (items start) 0)
		  parent-state: #f
		  envt: grammar))
         (first (input)))
    (let loop ((pstate (make-parser-state grammar (list t) first input 0))
               (token first))
      ;(print pstate)
      (if (eof-token? token)
          (parse-tree pstate)
          (loop (advance-input pstate token)
                (input))))))

(define-method parse-using-grammar ((self <function>) (grammar <grammar>))
  (parse-using-grammar* self grammar))

#|
(define-method parse-using-grammar ((self <string>) (grammar <grammar>))
  (parse-using-grammar (open-input-string self) grammar))

(define-method parse-using-grammar ((self <input-port>) (grammar <grammar>))
  (parse-using-grammar
   (lambda ()
     (let ((ch (read-char self)))
       (if (eof-object? ch)
           #f
           ch)))
   grammar))

|#

(define-method parse-using-grammar ((self <list>) (grammar <grammar>))
  (parse-using-grammar* 
   (lambda ()
     (if (null? self)
         #f
         (let ((n (car self)))
           (set! self (cdr self))
           n)))
   grammar))


