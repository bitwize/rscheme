#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/codegen/aml2bc.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.13
 | File mod date:    2003-07-17 11:42:35
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  codegen
 |
 | Purpose:          compile abstract machine language into bytecodes
 `------------------------------------------------------------------------|#

;; analagous to assem.scm...

(define (aml-expr-is-obj? aml-expr)
  (prim-subtype? (lookup-prim-type (car aml-expr))
		 (lookup-prim-type '<obj>)))

(define (aml-expr-is-raw-bool? aml-expr)
  (prim-subtype? (lookup-prim-type (car aml-expr))
		 (lookup-prim-type '<raw-bool>)))

(define (compile-aml-stmt insn)
  (if *debug-byte-codes*
      (format #t "aml-stmt: ~s\n" insn))
    (case (car insn)
	((apply) (aml-apply insn))
	((applyg) (aml-applyg insn))
	((applyf) (aml-applyf insn))
	((set!) (aml-set! insn))
	((save) (aml-save insn))
	((restore) (aml-restore insn))
	((label) (def-label (cadr insn)))
	((return) (aml-return insn))
	((bind) (aml-bind insn))
	((unbind) (emit-unbind))
	((jump) (emit-jump (cadr insn) (caddr insn)))
	((bjump) (emit-bjump (cadr insn) (caddr insn)))
	((if) (aml-if insn))
	((seq) (for-each compile-aml-stmt (cdr insn)))
	((do) (compile-aml-expr (cadr insn))
	      (emit-pop))
	((check=) (emit-check= (cadr insn)))
	((check>=) (emit-check>= (cadr insn)))
	((collect>) (emit-collect> (cadr insn)))
	((set-false<) (emit-set-false< (cadr insn)))
	((use-empty-envt) (emit-use-empty-envt))
	((use-function-envt) (emit-use-function-envt))
	;;
	;; a primop statement is generated from a primcall
	;; that returns no values
	;;
	((primop)
	 (gen-primop-expr insn))
	(else
	    (error "compile-aml-stmt: Unrecognized AML stmt: ~s" insn))))

(define alloc-temp-label
  (let ((i 10000))
    (lambda ()
      (set! i (+ i 1))
      i)))

;; note that if's are at the end of a monotone too,
;; so we don't need to have our own JOIN label

(define (aml-if insn)
  (let ((l (alloc-temp-label)))
    (assert (aml-expr-is-raw-bool? (cadr insn)))
    (compile-aml-expr (cadr insn))
    (emit-branch-if-false l)
    (compile-aml-stmt (caddr insn))
    (def-label l)
    (compile-aml-stmt (cadddr insn))))

(define (reg-ref? aml-expr)
  (let ((aml-expr (cdr aml-expr)))
    (and (eq? (car aml-expr) 'ref)
	 (eq? (caadr aml-expr) 'reg))))

(define (aml-bind insn)
  (assert (every? aml-expr-is-obj? (cdr insn)))
  ;; special case  (bind (ref (reg 0)) (ref (reg 1)) ... (ref (reg N-1)))
  (if (and (every? reg-ref? (cdr insn))
	   (equal? (map cadr (map caddr (cdr insn))) 
		   (range (length (cdr insn)))))
      ;;
      ;; it's a bind of registers from 0 to N-1
      ;;
      (emit-bind-first-regs (length (cdr insn)))
      ;; it's a bind not all of registers, or not the particular
      ;; registers in the right order
       (begin
	 (for-each compile-aml-expr (cdr insn))
	 (emit-bind (length (cdr insn))))))

(define (aml-save insn)
    (emit-save (cadr insn) (caddr insn)))
    
(define (aml-restore insn)
    (emit-restore (cadr insn)))

(define (aml-return insn)
    (emit-return (cadr insn)))
    
(define (aml-apply insn)
  (compile-aml-expr (caddr insn))
  (emit-apply (cadr insn)))

(define (aml-applyg insn)
  (compile-aml-expr (caddr insn))
  (emit-applyg (cadr insn)))

(define (aml-applyf insn)
  (compile-aml-expr (caddr insn))
  (emit-applyf (cadr insn)))

(define (aml-set! insn)
  (assert (aml-expr-is-obj? (caddr insn)))
  (let ((ea (cadr insn))
	(rhs (caddr insn)))
    ;(format #t "aml-set: ea = ~s, rhs = ~s\n" ea rhs)
    ;;
    ;; do a peep-hole optimization
    ;;  throw away things of the form: (set! (reg ,x) (ref (reg ,x)))
    ;;
    (if (and (eq? (car ea) 'reg) (reg-ref? rhs))
	(let ((to (cadr ea))
	      (from (reg-ref-reg rhs)))
	  (if (eq? from to)
	      #t ;; instant NOP
	      (emit-reg-xfer from to)))
	;;
	(begin
	  ;; get the RHS onto the eval stack
	  (compile-aml-expr rhs)
	  (case (car ea)
	    ((reg)  (emit-reg-set (cadr ea)))
	    ((lex-var) (emit-lex-set (cadr ea) (caddr ea)))
	    ((tl-var tl-var/b) (emit-tl-set (cadr ea)))
	    (else   (error/internal "Bad EA in set!: ~s" ea)))))))


(define (compile-aml-expr expr)
  (if *debug-byte-codes*
      (format #t "compile-aml-expr: ~s\n" expr))
    ;; the car of an expr is it's type
    (set! expr (cdr expr))
    (if (pair? expr)
	(case (car expr)
	    ((literal)
		(emit-literal (cadr expr)))
	    ((closure)
	     (emit-closure (cadr expr)))
	    ((this-function)
	     (emit-this-function))
	    ((seq)
		(assert (pair? (cdr expr)))
		(let loop ((i (cdr expr)))
		    (if (null? (cdr i))
			(compile-aml-expr (car i))
			(begin
			    (compile-aml-expr (car i))
			    (emit-pop)))))
	    ((primop)
	     (gen-primop-expr expr))
	    ((ref)
		(let ((ea (cadr expr)))
		    (case (car ea)
			((reg) (emit-reg-ref (cadr ea)))
			((lex-var) (emit-lex-ref (cadr ea) (caddr ea)))
			((tl-var) (emit-tl-ref (cadr ea)))
			((tl-var/b) (emit-tl-ref/bound (cadr ea)))
			(else (error/internal "Bad EA in ref: ~s" ea)))))
	    ((immob)
	    	(emit-immob (cadr expr)))
	    ((int)
	        (emit-raw-int (cadr expr)))
	    ((raw-bool)
	        (emit-raw-bool (cadr expr)))
	    (else
	     (error/internal "Bad EXPR: ~s" expr)))
	; plain immobs no longer supported
	;(emit-immob expr)
	(error/internal "Bad EXPR: ~s" expr)))

;;;

;;; peephole-optimize certain primops
;;;  ie, for gvec-ref and gvec-set of small constant offsets,
;;;      use gvec-load and gvec-store
;;;

(define (gen-primop-expr-no-opt primop args)
  (for-each compile-aml-expr args)
  (emit-primop primop (length args)))

(define (gen-primop-expr expr)
  (let ((primop (cadr expr))
	(args (cddr expr)))
    ;;
    ;; check for some special cases...
    ;;
    (case (name primop)
      ((raw-int->fixnum)
       (if (eq? (cadar args) 'int)
	   ;; it's the raw-int->fixnum primop and the arg is a raw-int const
	   (emit-fixnum (caddar args))
	   (gen-primop-expr-no-opt primop args)))
      ((gvec-ref)
       (let ((ix (small-aml-int-const (cadr args))))
	 (if ix
	     (begin
	       (compile-aml-expr (car args))
	       (emit-gvec-load ix))
	     (gen-primop-expr-no-opt primop args))))
      ((gvec-set!)
       (let ((ix (small-aml-int-const (cadr args))))
	 (if ix
	     (begin
	       (compile-aml-expr (car args))   ;; lhs
	       (compile-aml-expr (caddr args)) ;; rhs
	       (emit-gvec-store ix))
	     (gen-primop-expr-no-opt primop args))))
      (else
       (gen-primop-expr-no-opt primop args)))))

;;; check if an aml expression denotes a small constant (0..255) integer
;;; in raw-int-bytes units

(define (small-aml-int-const aml-expr)
  (and (eq? (car aml-expr) '<raw-int-bytes>)
       (eq? (cadr aml-expr) 'primop)
       (eq? (name (actual-bdg (caddr aml-expr))) 'raw-int-words->raw-int-bytes)
       (let ((arg (cadddr aml-expr)))
	 (and (eq? (cadr arg) 'int)
	      (>= (caddr arg) 0)
	      (< (caddr arg) 256)
	      (caddr arg)))))
