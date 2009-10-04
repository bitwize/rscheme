#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/codegen/genaml.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.34
 | File mod date:    2003-11-04 15:47:07
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  codegen
 |
 | Purpose:          Generate abstract machine language (AML) 
 `------------------------------------------------------------------------|#

;; Generate abstract machine language
;;
;; AML is defined so that both the C back end
;; and the byte-coded back end can take it as input
;;
;; Hence, in order to allow the back ends to optimize
;; nested expressions, AML expressions are TYPED
;; (this is implemeted as an AML expression being
;;  a pair whose car is the symbol for the resulting
;;  type and whose cdr is the untyped AML expression)
;;
;; for example:
;;
;;     (set! (reg 0 foo) (<fixnum> primop #[<primop> fixnum+]
;;			           (<fixnum> ref (reg 0 bar))
;;			           (<fixnum> int 1)))
;;
;; The type system is restricted to primtypes, named by symbols
;;
;;
;;  The `mode' argument to gen-aml is how the receiver object is situated
;;  in the program; essentially, what it should do with it's results
;;
;;  Valid modes are:
;;
;;      value
;;      effect
;;      tail
;;	(bind min rest?)
;;	(reg-bind min rest? first-reg)
;;
;; The meaning of the "bind" modes is that the code
;; is to have its value or values be the initializers
;; of the given binding (either on the heap or in registers,
;; according as whether it is (bind ...) or (reg-bind ...))

(define-generic-function gen-aml*)

(define-syntax (gen-aml . args) (gen-aml* . args))

#|
(define (gen-aml (self <icode>) ct-envt in-regs mode)
  ;; kill any unreferenced bindings from the registers
  (let ((vr (vars-referenced self)))
    (gen-aml* self 
	      ct-envt
	      (map (lambda (r)
		     (if (or (eq? r #f)    ; placeholder?
			     (eq? r 'free) ; already available?
			     (memq r vr))
			 r
			 'free))
		   in-regs)
	      mode)))
|#

(define $nil '(<immob> immob ()))
(define $true '(<boolean> immob #t))
(define $false '(<boolean> immob #f))

;;
;; returns the number of registers in use after a reg-bind
;; is done.
;; for (reg-bind min rest? first)
;; this is: first + min + 1 if rest?
;; e.g., if (reg-bind 3 #f 1)
;;
;; then REG1, REG2, and REG3 will be loaded, so 
;; 4 registers will be in use
;;

(define (num-reg-bind-regs (mode <pair>))
  (+ (cadddr mode) (num-targets mode)))

;;
;;  returns the number of target cells indicated by the given binding
;;  mode, which is exactly `min' unless `rest?', in which case it's `min+1'
;;

(define (num-targets (mode <pair>))
  (if (caddr mode)
      (+ 1 (cadr mode))
      (cadr mode)))

; If the mode is 'value then a stmt/expr tuple is returned
; otherwise, only a stmt is returned

(define-syntax (make-se s e)	; make a stmt/expr tuple
  (%make <vector> s e))

(define-syntax (se-stmt code)	; extract the stmt part
  (gvec-ref code 0))

(define-syntax (se-expr code)	; extract the expr part
  (gvec-ref code 1))

(define (reg-ref-reg aml)       ; extract the reg # from a (<obj> ref (reg n))
  (car (cdaddr aml)))

(define (cons-expr head-expr tail-expr)
  (list '<pair> 
	'primop 
	(well-known 'cons) 
	(obj-expr head-expr) 
	(obj-expr tail-expr)))

(define (int-const fx)
  ;;  (assert (fixnum? fx))
  (list '<raw-int> 'int fx))

(define (make-list-expr exprs)
  (if (null? exprs)
      $nil
      (cons-expr (car exprs) (make-list-expr (cdr exprs)))))

(define-method gen-aml* ((self <ic-cast>) ct-envt in-regs mode)
  (let ((aml (gen-aml* (expr self) ct-envt in-regs mode)))
    ;(format #t "casting: ~s\n" aml)
    (if (eq? mode 'value)
	(make-se (se-stmt aml)
		 (cons (ct-type->prim-type-name (car (return-types self)))
		       (cdr (se-expr aml))))
	aml)))

(define-method gen-aml* ((self <ic-const>) ct-envt in-regs mode)
  (let ((v (value self)))
    (gen-simple-aml
     '()
     (cond
      ((eq? v #t) $true)
      ((eq? v #f) $false)
      ((null? v) $nil)
      ((fixnum? v) (int-const v))
      ;; a hack to support bytecodes in the immediate term...
      ;; (because we can't have float consts inline in BCs)
      ;((real? v) (list '<double-float> 'float v))
      ((real? v) (list '<double-float> 'literal (alloc-literal v) v))
      ((ascii-char? v) (list '<ascii-char> 'immob v))
      ((unicode-char? v) (list '<unicode-char> 'immob v))
      ((symbol? v) (list '<symbol> 'literal (alloc-literal v) v))
      ((memq v '(#none #undef #unbound #uninit #key #rest))
       (list '<immob> 'immob v))
      (#t
       (list
	(if (instance? (actual-value v) <<target-class>>)
	    '<<class>>
	    (native-class->prim-type-name (object-class (actual-value v))))
	'literal
	(alloc-literal v)
	v)))
     mode)))

(define (gen-simple-aml stmt expr mode)
  (case mode
    ((value)
     (make-se stmt expr))
    ((effect)
     stmt)
    ((tail)
     (append stmt
	     `((set! (reg 0 #f) ,(obj-expr expr))
	       (return 1))))
    (else
     (append stmt (make-binder (list expr) mode)))))




; code context management...

(define-syntax (make-code-ctx info)
  (vector '() 0 info))

(define-syntax (code-ctx-literals* code-ctx)
  (vector-ref code-ctx 0))

(define (code-ctx-literals code-ctx)
  (reverse (code-ctx-literals* code-ctx)))

(define-syntax (code-ctx-properties code-ctx)
  (vector-ref code-ctx 2))

(define (code-ctx-add-property! (code-ctx <vector>) key value)
  (vector-set! code-ctx
	       2
	       (cons (cons key value)
		     (vector-ref code-ctx 2))))

;; some properties have list-like values.
;; this function adds an element to the beginning of such a list,
;; creating a new list if the property wasn't previously present

(define (code-ctx-extend-property! (code-ctx <vector>) key value)
  (let* ((a (assq key (vector-ref code-ctx 2))))
    (if a
	(begin
	  (set-cdr! a (cons value (cdr a)))
	  (values))
	(code-ctx-add-property! code-ctx key (cons value '())))))

; Normally, the dynamic state holds the current code context
; (this is so that it doesn't have to be passed so deeply in 
;  the call chain)

(define-thread-var *code-context*)

(define-syntax (current-code-context)
  *code-context*)

(define-syntax (alloc-label)
  (let* ((code-ctx (current-code-context))
	 (i (vector-ref code-ctx 1)))
    (vector-set! code-ctx 1 (add1 i))
    i))

(define (coalesce-literal thing)
  (let ((current-literals (code-ctx-literals* (current-code-context))))
    (if (eq? (object-class thing) <top-level-var>)
	(memq thing current-literals)
	(if (eq? (object-class thing) <template>)
	    #f
	    (member thing current-literals)))))


(define (alloc-literal thing)
  (let ((already (coalesce-literal thing)))
    (if already
	(sub1 (length already))
	(let ((code-ctx (current-code-context)))
	  (vector-set! code-ctx 0
		       (cons thing
			     (code-ctx-literals* code-ctx)))
	  (sub1 (length (code-ctx-literals* code-ctx)))))))


;;========================================================================

; SIMPLE? returns #t if the value-compilation of the given
; icode results in side-effect free and inlinable code
; It is used to notice when the remaining arguments in a lazy-args
; aren't going to affect an expr

(define (simple? icode)
  (let ((c (object-class icode)))
    (or (eq? c <ic-lex-ref>)
	(eq? c <ic-tl-ref>)
	(eq? c <ic-const>)
	(and (eq? c <ic-call-prim>)
	     (not (primop-has-side-effect? (actual-bdg (function icode))))
	     (every? simple? (arg-list (args icode)))))))

; LEX-VAR constructs an effective address (ea) for a lexical
; variable.  The type depends on whether or not the variable
; is found in the registers or not.

(define (lex-var ct-envt in-regs var)
  (let ((r (memq var in-regs)))
    (if r
	`(reg ,(sub1 (length r)) ,(name var))
	(let ((lex-addr (find-lex-addr ct-envt var)))
	  (if lex-addr
	      `(lex-var ,(car lex-addr) ,(cdr lex-addr) ,(name var))
	      ; if not found, return #f as a flag [cr 617]
	      #f)))))

; TL-VAR constructs an eff.addr. for a top-level variable.

(define (tl-var var)
  (list (if (and (instance? var <top-level-var>)
		 (eq? (value var) '#unbound))
	    'tl-var
	    'tl-var/b)
	(alloc-literal var)
	(name var)))

; NORMALIZE-BIND-EXPRS takes a list of expressions and a
; desired structure specification (min+rest?) and massages
; the exprs into the desired form.  It will pad the exprs
; with #f's if necessary, and drop expressions if needed,
; and construct a list as needed for rest?.  The returned
; list of expressions always has length (+ min (if rest? 1 0))
; and the primtypes are subtypes of <obj>

(define (normalize-bind-exprs exprs min rest?)
  (let loop ((min min) (exprs exprs))
    (if (eq? min 0)
	(if rest?
	    (list (make-list-expr exprs))
	    '())
	(if (null? exprs)
	    (cons $false
		  (loop (sub1 min) '()))
	    (cons (obj-expr (car exprs))
		  (loop (sub1 min) (cdr exprs)))))))

; MAKE-BINDER takes a list of expressions and a binding mode (ie
; '(bind ...) or '(reg-bind ...)) and constructs the appropriate
; binder, filling out the expressions or truncating them to fit.
; Currently, there aren't any assurances that the expressions 
; will be evaluated in any particular order, but that will probably
; change (because if we generate explicit calls to gvec_write to 
; build up the new envt, we can assure the order just as easily as
; for the reg-bind)

(define (make-binder t-exprs mode)
  (let ((norm (normalize-bind-exprs t-exprs (cadr mode) (caddr mode))))
    (if (eq? (car mode) 'bind)
	(list (cons 'bind norm))
	(let loop ((result '()) (src norm) (dst (cadddr mode)))
	  (if (null? src)
	      (reverse result)
	      (loop (cons `(set! (reg ,dst #f) ,(car src))
			  result)
		    (cdr src)
		    (add1 dst)))))))

(define-syntax (gen-aml-ref rvalue mode type)
  (gen-simple-aml '() (list type 'ref rvalue) mode))

(define (gen-aml-set lvalue rvalue mode in-regs)
  (case mode
    ((value)	
     (let* ((temp `(reg ,(length in-regs) #f))
	    (rhs-aml (obj-expr (se-expr rvalue))))
       (make-se (append (se-stmt rvalue)
			`((set! ,temp ,rhs-aml)
			  (set! ,lvalue (,(car rhs-aml) ref ,temp))))
		`(,(car rhs-aml) ref ,temp))))
    ;;
    ((effect)	
     (append (se-stmt rvalue)
	     `((set! ,lvalue ,(obj-expr (se-expr rvalue))))))
    ((tail)		
     (append (se-stmt rvalue) 
	     `((set! (reg 0 #f) ,(obj-expr (se-expr rvalue)))
	       (set! ,lvalue (<obj> ref (reg 0 #f)))
	       (return 1))))
    (else 		
     (let ((temp `(reg ,(length in-regs) #f)))
       (append (se-stmt rvalue)
	       `((set! ,temp ,(obj-expr (se-expr rvalue)))
		 (set! ,lvalue (<obj> ref ,temp))
		 ,@(make-binder `((<obj> ref ,temp)) mode)))))))

(define *num-anonymous* 0)

(define (new-anonymous)
  (set! *num-anonymous* (add1 *num-anonymous*))
  (list 'anonymous *num-anonymous*))

(define-method gen-aml* ((self <ic-lambda>) ct-envt in-regs mode)
  (let* ((code (proc self))
	 (code-context (make-code-ctx (code-properties self)))
	 (name (if (name code) 
		   (name code) 
		   (new-anonymous)))
	 (asm (procedure->aml code ct-envt code-context))
	 (t (aml->template asm code-context)))
    (gen-simple-aml '() 
		    (list '<function> 'closure (alloc-literal t))
		    mode)))

(define-method gen-aml* ((self <ic-lex-ref>) ct-envt in-regs mode)
  (let ((t (ct-type->prim-type-name (actual-value (first-return-type self)))))
    ;(format #t "LEX REF ~s => ~s\n" (first-return-type self) t)
    (let ((ea (lex-var ct-envt in-regs (var self))))
      (assert ea) ; if we are a reference, the var better exist
      (gen-aml-ref ea mode t))))

(define-method gen-aml* ((self <ic-lex-set>) ct-envt in-regs mode)
  (let ((ea (lex-var ct-envt in-regs (var self))))
    (if ea
	(gen-aml-set ea
		     (gen-aml (rhs self) ct-envt in-regs 'value)
		     mode 
		     in-regs)
	;; generate the RHS in the appropriate mode
	(gen-aml (rhs self) ct-envt in-regs mode))))

(define-method gen-aml* ((self <ic-tl-ref>) ct-envt in-regs mode)
  (gen-aml-ref (tl-var (var self)) 
	       mode
	       (ct-type->prim-type-name
		(actual-value
		 (first-return-type self)))))

(define-method gen-aml* ((self <ic-tl-set>) ct-envt in-regs mode)
  (gen-aml-set (tl-var (var self))
	       (gen-aml (rhs self) ct-envt in-regs 'value)
	       mode 
	       in-regs))

(define-method gen-aml* ((self <ic-seq>) ct-envt in-regs mode)
  (let loop ((result '()) (stmts (stmt-list self)))
    (if (null? stmts)
	(begin
	  (warning "<ic-seq> with no statements")
	  (gen-simple-aml '() $false mode))
	(let ((code (gen-aml (car stmts) 
			     ct-envt
			     in-regs
			     (if (null? (cdr stmts))
				 mode
				 'effect))))
	  (if (null? (cdr stmts))
	      (if (eq? mode 'value)
		  (make-se (append result (se-stmt code)) (se-expr code))
		  (append result code))
	      (loop (append result code) (cdr stmts)))))))


(define (use-regs in-regs num)
  (if (eq? num 0)
      in-regs
      (cons #f (use-regs in-regs (sub1 num)))))

(define (shift-regs num-regs first-src-reg first-dst-reg)
  (if (eq? first-src-reg first-dst-reg)
      '()
      (let loop ((s first-src-reg) 
		 (d first-dst-reg) 
		 (n num-regs) 
		 (r '()))
	(if (eq? n 0)
	    (if (< first-src-reg first-dst-reg)
		r
		(reverse r))
	    (loop (add1 s)
		  (add1 d)
		  (sub1 n)
		  (cons `(set! (reg ,d #f) (<obj> ref (reg ,s #f)))
			r))))))

;; pop enough environments to get from 'from-envt' to 'to-envt'

(define (pop-envts from-envt to-envt)
  (let loop ((f from-envt) (r '()))
    (if (eq? f to-envt)
	r
	(loop (cdr f) (cons '(unbind) r)))))

;; the functionality of GEN-AML-TAIL-COMBO is
;; extended to support the possibility that in-regs != '()
;; (because we may still reg-allocate if the only combos
;;  in a body are in tail position)
;; this simply involves reg-binding to regs N... where N=(length in-regs)
;; and installing an appropriate argument shifter
;; (generates the function part to reg N+M, also) --
;; the shift occurs JUST BEFORE the apply

(define (apply-opcode (fn-expr <expr-icode>))
  (let ((type (actual-value (ct-unprim-type (first-return-type fn-expr)))))
    (cond
     ((target-subclass? type (value
			      (actual-bdg 
			       (well-known '<single-dispatch-gf>))))
      'applyg)
     ((target-subclass? type (value
			      (actual-bdg
			       (well-known '<function>))))
      'applyf)
     (else
      'apply))))

(define (gen-aml-tail-combo (combo <ic-call>) ct-envt in-regs)
  (let ((opcode (apply-opcode (function combo))))
    (if (simple? (function combo))
	(bind ((arg-exprs (arg-list (args combo)))
	       (n (length arg-exprs))
	       (stmts exprs in-r (gen-rearranged-aml arg-exprs
						     ct-envt
						     in-regs
						     n
						     #f))
	       (fse (gen-aml (function combo) ct-envt in-r 'value)))
	  (assert (null? (se-stmt fse)))
	  ;; may still need to save the fn-expr elsewhere, if it involves
	  ;; a register which is clobbered by the assmts
	  (let* ((u (regs-used (se-expr fse)))
		 (fne (obj-expr (se-expr fse)))
		 (need-room (max (length in-r) n)))
	    (if (and (pair? u)
		     (< (apply min u) need-room))
		(let ((tmp-reg need-room))
		  (append stmts
			  `((set! (reg ,tmp-reg $fn) ,fne))
			  (gen-parallel-assmt exprs
					      (range n) 
					      (range need-room))
			  `((,opcode ,n (,(car fne) ref (reg ,tmp-reg $fn))))))
		(append stmts
			(gen-parallel-assmt exprs (range n) in-r)
			`((,opcode ,n ,fne))))))
	;;
	;; the function argument is not simple; arrange for it's evaluation
	;; just like the other arguments
	;;
	(bind ((arg-exprs (append (arg-list (args combo)) 
				  (list (function combo))))
	       (n (length arg-exprs))
	       (stmts exprs in-r (gen-rearranged-aml arg-exprs
						     ct-envt
						     in-regs
						     n
						     #f))
	       (fn-expr (last exprs)))
	  (append stmts
		  (gen-parallel-assmt exprs (range n) in-r)
		  `((,opcode ,(- n 1)
			     (,(car fn-expr) ref (reg ,(- n 1) fn)))))))))

(define (gen-aml-combo code ct-envt in-regs before-restore)
  (gen-aml-combo* code ct-envt in-regs before-restore (length in-regs)))

(define (gen-aml-combo* code ct-envt in-regs before-restore num-to-save)
  (let ((l (alloc-label)))
    (append `((save ,num-to-save ,l))
	    (gen-aml-tail-combo code ct-envt in-regs)
	    `((label ,l))
	    before-restore
	    `((restore ,num-to-save)))))

(define (make-runtime-bind mode fn-args?)
  ;
  (define (make-shift-regs)
    (let loop ((num (if (caddr mode)
			(add1 (cadr mode))
			(cadr mode)))
	       (src 0)
	       (dst (cadddr mode))
	       (result '()))
      (if (eq? num 0)
	  result
	  (loop (sub1 num)
		(add1 src)
		(add1 dst)
		(cons `(set! (reg ,dst #f) 
			     (<obj> ref (reg ,src #f)))
		      result)))))
  ;
  (define (make-heap-bind)
    (let loop ((num (if (caddr mode)
			(add1 (cadr mode))
			(cadr mode)))
	       (src 0)
	       (result '()))
      (if (eq? num 0)
	  (list (cons 'bind (reverse result)))
	  (loop (sub1 num)
		(add1 src)
		(cons `(<obj> ref (reg ,src #f))
		      result)))))
  ;
  (append
   (if fn-args?
       (if (caddr mode)
	   `((check>= ,(cadr mode)))
	   `((check= ,(cadr mode))))
       `((set-false< ,(cadr mode))))
   (if (caddr mode)
       `((collect> ,(cadr mode)))
       '())
   (if (eq? (car mode) 'reg-bind)
       (make-shift-regs)
       (make-heap-bind))))

(define-method gen-aml* ((self <ic-call>) ct-envt in-regs mode)
  (case mode
    ((tail) 
     (gen-aml-tail-combo self ct-envt in-regs))
    ((value) 
     (let ((r (list 'reg (length in-regs) #f))
	   (t (ct-type->prim-type-name (first-return-type self))))
       (make-se (gen-aml-combo self 
			       ct-envt 
			       in-regs 
			       `((set! ,r (,t ref (reg 0 #f)))))
		`(,t ref ,r))))
    ((effect)
     (gen-aml-combo self ct-envt in-regs `()))
    (else
     (if (eq? (car mode) 'reg-bind)
	 ;; any registers we're being asked to bind are surely
	 ;; dead by the time the call completes
	 (gen-aml-combo* self
			 ct-envt
			 in-regs
			 (make-runtime-bind mode #f)
			 (min (length in-regs) (cadddr mode)))
	 (gen-aml-combo self 
			ct-envt 
			in-regs 
			(make-runtime-bind mode #f))))))

;; suppress the (RT) creation of a binding environment
;; when there is nothing to bind.  Note that, if it is a
;; procedure, we go ahead and emit the check

(define-method gen-aml* ((self <ic-bind>) ct-envt in-regs mode)
  (if (null? (bindings (envt self)))
      (if (inits self)
	  (gen-aml (body self) ct-envt in-regs mode)
	  (append (make-runtime-bind '(reg-bind 0 #f 0) #t)
		  (gen-aml (body self) ct-envt in-regs mode)))
      (gen-aml-bind self ct-envt in-regs mode #f)))

;;;

(define-method gen-aml* ((self <ic-multi>) ct-envt in-regs mode)
  (case mode
    ((value) 
     (if (null? (arg-list self))
	 (make-se '() $false)
	 (let ((code (gen-aml-lazy-args (arg-list self)
					ct-envt 
					in-regs)))
	   (make-se (se-stmt code) (car (se-expr code))))))
    ;;
    ((effect) 
     (if (null? (arg-list self))
	 '()
	 (let ((code (gen-aml-lazy-args (arg-list self)
					ct-envt 
					in-regs)))
	   (se-stmt code))))
    ;;
    ((tail)
     (if (null? (arg-list self))
	 `((return 0))
	 (bind ((n (length (arg-list self)))
		(stmts exprs in-r (gen-rearranged-aml (arg-list self)
						      ct-envt
						      in-regs
						      n
						      #f)))
	   (append stmts
		   (gen-exprs-to-reg-seq exprs 
					 0
					 (range (max n (length in-regs))))
		   `((return ,n))))))
    ;;
    (else
     (bind ((stmts exprs in-r (gen-rearranged-aml (arg-list self) 
						  ct-envt
						  in-regs
						  (cadr mode)
						  (caddr mode))))
       (if (eq? (car mode) 'bind)
	   (append stmts `((bind ,@(map obj-expr exprs))))
	   (append stmts (gen-exprs-to-reg-seq exprs 
					       (cadddr mode) 
					       in-r)))))))

(define (gen-exprs-to-reg-seq (exprs <list>) to-first-reg in-regs)
  (gen-parallel-assmt exprs 
		      (map (lambda (i)
			     (+ i to-first-reg))
			   (range (length exprs)))
		      in-regs))

(define-method gen-aml* ((self <ic-if>) ct-envt in-regs mode)
  ;;
  (define (before-label stmts0)
    (let loop ((stmts stmts0) (result '()))
      (if (null? stmts)
	  stmts0
	  (if (and (pair? (car stmts))
		   (eq? (caar stmts) 'label))
	      (reverse result)
	      (loop (cdr stmts) (cons (car stmts) result))))))
  ;;
  (define (after-label stmts)
    (if (null? stmts)
	'()
	(if (and (pair? (car stmts))
		 (eq? (caar stmts) 'label))
	    stmts
	    (after-label (cdr stmts)))))
  ;;
  (let* ((n (length in-regs))
	 (test (gen-aml (condition self) ct-envt in-regs 'value))
	 (xpr (raw-bool-expr (se-expr test)))
	 (l (alloc-label))
	 (mk-jump (if (or (eq? xpr $raw-true)
			  (eq? xpr $raw-false))
		      ;; it's a const test -- don't allocate or use a lable
		      (lambda (n)
			'())
		      (lambda (n)
			`((jump ,n ,l)))))
	 (gen-br (case mode
		   ((effect) (lambda (ic)
			       (append (gen-aml ic 
						ct-envt 
						in-regs 
						'effect)
				       (mk-jump n))))
		   ((tail) (lambda (ic)
			     (gen-aml ic 
				      ct-envt 
				      in-regs 
				      'tail)))
		   ((value) (lambda (ic)
			      (let ((c (gen-aml ic 
						ct-envt 
						in-regs 
						'value)))
				(append (se-stmt c)
					`((set! (reg ,n #f) 
						,(obj-expr (se-expr c))))
					(mk-jump (add1 n))))))
		   (else (lambda (ic)
			   (append (gen-aml ic
					    ct-envt
					    in-regs
					    mode)
				   (mk-jump (if (eq? (car mode) 'reg-bind)
						(num-reg-bind-regs mode)
						n)))))))
	 (true-br (gen-br (if-true self)))
	 (false-br (gen-br (if-false self)))
	 (result (cond
		  ((eq? xpr $raw-true)
		   (append (se-stmt test) true-br))
		  ((eq? xpr $raw-false)
		   (append (se-stmt test) false-br))
		  (else
		   (append (se-stmt test)
			   `((if ,xpr
				 (seq ,@(before-label true-br))
				 (seq ,@(before-label false-br))))
			   (after-label true-br)
			   (after-label false-br)
			   (if (eq? mode 'tail)
			       '()
			       `((label ,l))))))))
    (if (eq? mode 'value)
	(make-se result `(<obj> ref (reg ,n #f)))
	result)))

;;;

(define-method gen-aml* ((self <ic-call-prim>) ct-envt in-regs mode)
  ;;
  (let* ((the-primop (actual-bdg (function self)))
	 (args (gen-aml-lazy-args (arg-list (args self))
				  ct-envt 
				  in-regs))
	 (the-call (cons 'primop (cons (function self)
				       (coerce-aml-list
					(se-expr args)
					(arg-types the-primop)
					(rest-type the-primop))))))
    ;;
    (if (not (result-type the-primop))
	; calling a procedure...
	(case mode
	  ((effect) (append (se-stmt args) (list the-call)))
	  (else
	   (warning "~a used in value-expecting position (~a)"
		    the-primop
		    mode)
	   (if (eq? mode 'tail)
	       (append (se-stmt args)
		       (list the-call)
		       '((return 0)))
	       (make-se (append (se-stmt args) (list the-call))
			$false))))
	; calling a function...
	(let ((call-t-expr (cons (result-type the-primop) the-call)))
	  (case mode
	    ((value) (make-se (se-stmt args) call-t-expr))
	    ((tail) (append
		     (se-stmt args)
		     `((set! (reg 0 #f) ,(obj-expr call-t-expr))
		       (return 1))))
	    ((effect) (append (se-stmt args)
			      `((do (<ignore> ,@the-call)))))
	    (else
	     (append (se-stmt args)
		     (make-binder (list call-t-expr) mode))))))))

(define (gen-aml-lazy-args arg-list ct-envt in-regs)
  (let loop ((result-exprs '()) 
	     (result-stmts '())
	     (regs in-regs)
	     (args arg-list)
	     (simplicity (map simple? arg-list)))
    (if (null? args)
	(make-se result-stmts (reverse result-exprs))
	(if (or (eq? (object-class (car args)) <ic-const>)
		(every? identity (cdr simplicity)))
	    ; don't put it in a register
	    (let ((c (gen-aml (car args) ct-envt regs 'value)))
	      (loop (cons (se-expr c) result-exprs)
		    (append result-stmts (se-stmt c))
		    regs
		    (cdr args)
		    (cdr simplicity)))
	    ; put it in a register
	    (bind ((r `(reg ,(length regs) #f))
		   (c (gen-aml (car args) ct-envt regs 'value))
		   (put-it get-it (flush-to-register (se-expr c) r)))
	      (loop (cons get-it result-exprs)
		    (append result-stmts (se-stmt c) put-it)
		    (cons #f regs)
		    (cdr args)
		    (cdr simplicity)))))))

;;;
;;; arrange for an AML expression `xpr' to be saved in a register `r'
;;;
;;; the only tricky part is when `xpr' has a primitive type not
;;; inheriting from <obj>, in which case it has to be converted to an
;;; obj-type and back again.
;;;
;;; NOTE: The "back again" part is not done here; instead, the effective
;;; expression has a type compatible with the original type, and later
;;; coercions will restore the appropriate primtype with guaranteed success.
;;;

(define (flush-to-register xpr r)
  (let* ((xpr-type (lookup-prim-type (car xpr)))
	 (<obj> (lookup-prim-type '<obj>)))
    (if (prim-subtype? xpr-type <obj>)
	;; easy case, just save the value and get it back
	(values `((set! ,r ,xpr))
		`(,(car xpr) ref ,r))
	;; harder case; convert to an obj-type
	(let* ((medium-primtype (preferred-class xpr-type))
	       (obj-type-expr (coerce-aml xpr medium-primtype)))
	  (values `((set! ,r ,obj-type-expr))
		  `(,medium-primtype ref ,r))))))

;; loop optimization...

(define-method gen-aml* ((self <ic-loop>) ct-envt in-regs mode)
  (gen-aml-loop self ct-envt in-regs mode))

(define-method gen-aml* ((self <ic-jump>) ct-envt in-regs mode)
  (gen-aml-jump self ct-envt in-regs mode))


; PROCEDURE->AML is the external entry point into this
; code generation mechanism
;
; the <template> which results from generating code for the AML 
; returned by this function should wind up in a closure
; whose environment is a runtime environment corresponding to
; the ct-envt passed here.  that closure should be called with
; the number of arguments indicated by the <ic-procedure>'s body
; (which should be an <ic-bind>)

(define *save-captured-envt-info* #f) ;; by default, dont keep this info

(define (flatten-ct-envt ct-envt)
  (if (null? ct-envt)
      '()
      (cons (map (lambda (v)
		   (cons (name v)
			 (type v)))
		 (car ct-envt))
	    (flatten-ct-envt (cdr ct-envt)))))

(define (procedure->aml (self <ic-procedure>) ct-envt code-ctx)
  (thread-let ((*code-context* code-ctx))
    (let ((aml (gen-aml (body self) ct-envt '() 'tail)))
      (if *save-captured-envt-info*
	  ;; remember the captured ct-envt in the code context
	  (code-ctx-add-property! code-ctx
				  'captured-envt-info
				  (flatten-ct-envt ct-envt)))
      ;;
      (if (null? ct-envt)
	  aml
	  (cons '(use-function-envt) aml)))))

; EXPR->AML is another entry point, and is used for
; "procedureless" expressions (ie, top-level expressions)
;
; the <template> which results from generating code for
; the AML returned by this function should be closed in
; an empty environment and called with no arguments.
; (it will ignore any arguments)

(define (expr->aml (self <icode>) code-ctx)
  (thread-let ((*code-context* code-ctx))
    (if *save-captured-envt-info*
	(code-ctx-add-property! code-ctx
				'captured-envt-info
				'()))
    (cons '(use-empty-envt)
	  (gen-aml self '() '() 'tail))))

;;
;;  determine which registers an AML expression uses
;;

(define (regs-used aml-expr)
  (case (cadr aml-expr)
    ((ref)
     (if (eq? (caaddr aml-expr) 'reg)
	 (list (cadr (caddr aml-expr)))
	 '()))
    ((seq)
     (apply append (map regs-used (cddr aml-expr))))
    ((primop)
     (apply append (map regs-used (cdddr aml-expr))))
    (else '())))

;;
;;

;;
;; create AML for an <ic-bind> node, where the final result is to
;; be stored in registers (reg-bind mode) and the body of the
;; <ic-bind> calls for register allocation of the bound values
;;
;; this is a "double-bind" condition
;;
;; the strategy for the initializers is to bind their values into
;; the same registers that the body will use.  This works because
;; the body, when it finally returns multiple values, will shift
;; the values around to suit its purpose
;;
;; for example, if there is already one register in use (say, 
;; for a lexical variable X), then in-regs ==> (X)
;;
;; if we are being asked to bind 3 values into registers
;; 1, 2, and 3, then mode ==> (reg-bind 3 #f 1)
;; 
;; if we are binding 2 variables, say A and B, and we're assigning
;; them to registers, then we'll put them in registers 1 and 2
;; and call the initializer `multi' with (reg-bind 2 #f 1)
;; 


(define (gen-aml-dbl-bind self ct-envt in-regs mode body-label)
  (if (not (inits self))
      (error/internal "gen-aml-dbl-bind didn't expect no initializers"))
  (let* ((bindings (bindings (envt self)))
	 (body (gen-aml (body self)
			ct-envt  ;; reg-alloc'ing, no change in ct envt
			(append (reverse bindings) in-regs) ; but add our bindings on here
			mode))
	 (mode-for-multi (list 'reg-bind 
			       (num-args self) 
			       (rest? self)
			       (length in-regs)))
	 (bind-code (gen-aml (inits self)
			     ct-envt
			     in-regs 
			     mode-for-multi)))
    (append 
     bind-code
     (if body-label
	 `((jump ,(+ (length bindings) 
		     (length in-regs))
		 ,body-label)
	   (label ,body-label))
	 '())
     body)))


;;
;; generate fill code for a reg-bind operation
;; the result AML code just fills the registers
;; indicated by a reg-bind with #f values
;;
;; this is sometimes necessary: when a continuation is
;; going to be saved, but the registers haven't
;; been initialized yet
;;

(define (gen-fill-code mode)
  (let loop ((r '())
	     (which (cadddr mode))
	     (left (+ (cadr mode)
		      (if (caddr mode) 1 0))))
    (if (eq? left 0)
	r
	(loop (cons `(set! (reg ,which fill) ,$false) r)
	      (+ which 1)
	      (- left 1)))))

;;
;; gen-aml-bind-to-heap
;;
;; create AML from an <ic-bind> node, where we have decided
;; to allocate our own bindings on the heap (a real binding envt),
;; as may happen if our body captures the environment
;;

(define (gen-aml-bind-to-heap self ct-envt in-regs mode body-label)
  (let* ((bindings (bindings (envt self)))
	 (body-mode (if (and (pair? mode)
			     (eq? (car mode) 'bind))
			(list 'reg-bind
			      (cadr mode) 
			      (caddr mode)
			      (length in-regs))
			mode))
	 (body (gen-aml (body self)
			(cons bindings ct-envt)
			in-regs
			body-mode))
	 (bind-mode (list 'bind (num-args self) (rest? self)))
	 (binder  (if (inits self)
		      (let ((b (gen-aml (inits self)
					ct-envt
					in-regs
					bind-mode)))
			(if body-label
			    (append b 
				    `((jump ,(+ (length bindings) 
						(length in-regs))
					    ,body-label)
				      (label ,body-label)))
			    b))
		      (make-runtime-bind bind-mode #t))))
    (case (if (pair? mode)
	      (car mode)
	      mode)
      ((value)
       (let ((save-reg `(reg ,(length in-regs) #f))
	     (aml-expr (obj-expr (se-expr body))))
	 (make-se (append binder 
			  (se-stmt body)
			  `((set! ,save-reg ,aml-expr))
			  '((unbind)))
		  `(,(car aml-expr) ref ,save-reg))))
      ((tail)
       (append binder body))
      ((bind)
       (append binder
	       body
	       `((unbind)
		 (bind ,@(map (let ((first-temp (cadddr body-mode)))
				(lambda (i)
				  `(<obj> ref (reg ,(+ i first-temp) #f))))
			      (range (num-targets mode)))))))
      (else
       (append binder body '((unbind)))))))


;;
;; gen-aml-bind-to-regs
;;
;; create AML from an <ic-bind> node with a mode that is NOT
;; a (reg-bind) mode, but we have decided to register allocate
;; our own bindings
;;


(define (gen-aml-bind-to-regs self ct-envt in-regs mode body-label)
  (let* ((bindings (bindings (envt self)))
	 (body (gen-aml (body self)
			ct-envt
			(append (reverse bindings) in-regs)
			mode))
	 (bind-mode   (list 'reg-bind 
			    (num-args self) 
			    (rest? self) 
			    (length in-regs)))
	 (binder  (if (inits self)
		      (let ((b (gen-aml (inits self)
					ct-envt
					in-regs
					bind-mode)))
			(if body-label
			    (append b 
				    `((jump ,(+ (length bindings)
						(length in-regs))
					    ,body-label)
				      (label ,body-label)))
			    b))
		      (make-runtime-bind bind-mode #t))))
    (if (eq? mode 'value)
	(let ((save-reg `(reg ,(length in-regs) #f))
	      (aml-expr (obj-expr (se-expr body))))
	  ;(format #t "gen-aml-bind-to-regs: aml-expr: ~s\n" aml-expr)
	  (make-se (append binder 
			   (se-stmt body)
			   `((set! ,save-reg ,aml-expr)))
		   `(,(car aml-expr) ref ,save-reg)))
	(append binder body))))

;;;
;;;  algorithm for determining when to bind on the heap:
;;;   (1) if the body does a SAVE *and* any of the variables
;;;       being bound are side-effected
;;;   (2) or, if the body does a LAMBDA *and* any of the variables
;;;       being bound are captured
;;;  note that the current implementation can't support putting some
;;;  variables from a binding construct on the heap and other in regs
;;;

(define *num-frames-now-reg-alloced* 0)
(define *num-vars-now-reg-alloced* 0)

(define-syntax inc! 
  (syntax-form (v)
    (set! v (+ v 1)))
  (syntax-form (v n)
    (set! v (+ v n))))

(define (bind-to-heap? body (vars <list>))
  (if (or (and (does-save? body)
	       (any? ever-set? vars))
	  (and (does-lambda? body)
	       (any? (lambda (captured)
		       (memq captured vars))
		     (vars-captured body))))
      (if (or (does-save? body)
	      (does-lambda? body))
	  ;; these cases would have been heap-alloced before
	  (begin
	    (inc! *num-frames-now-reg-alloced*)
	    (inc! *num-vars-now-reg-alloced* (length vars))
	    #t)
	  ;; these would have already been reg-alloced
	  #t)
      #f))

(define (gen-aml-bind self ct-envt in-regs mode body-label)
  (let* ((reg-alloc? (not (bind-to-heap? (body self) 
					 (bindings (envt self)))))
	 (dbl-bind? (and reg-alloc? 
			 (pair? mode) 
			 (eq? (car mode) 'reg-bind))))
    (if reg-alloc?
	(if dbl-bind?
	    (gen-aml-dbl-bind self ct-envt in-regs mode body-label)
	    (gen-aml-bind-to-regs self ct-envt in-regs mode body-label))
	(gen-aml-bind-to-heap self ct-envt in-regs mode body-label))))
