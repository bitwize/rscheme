#|------------------------------------------------------------*-Scheme-*--|
 | File:    rs/backend/c/assem.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.2
 | File mod date:    2005-02-18 15:58:16
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#


(define (write-multi-expr multi-items port)
  (write-char #\( port)
  (if (pair? multi-items)
      (begin
	(write-expr port (car multi-items))
	(let loop ((i (cdr multi-items)))
	  (if (pair? i)
	      (begin
		(write-char #\, port)
		(write-expr port (car i))
		(loop (cdr i)))))))
  (write-char #\) port))

(define (write-reg ea port)
  (if (< (cadr ea) 10)
      (asm* port "REG~d~n" (cadr ea) (caddr ea))
      (asm* port "REG(~d)~n" (cadr ea) (caddr ea))))

(define (write-primop-expr port op args)
  (if (symbol? op)
      ;; check for special-cased ops
      (case op
	((make)
	 ;; subtract one from the number of args because the first
	 ;; arg is the class, which doesn't count in the makeN
	 (let ((objsize (- (length args) 1)))
	   (if (< objsize 10)
	       (begin
		 (format port "make~d" objsize)
		 (write-multi-expr args port))
	       (begin
		 (format port "maken( ")
		 (write-expr port (car args))
		 (format port ", ~d" objsize)
		 (for-each (lambda (a)
			     (format port ", ")
			     (write-expr port a))
			   (cdr args))
		 (format port " )")))))
	(else
	 (error "unrecognized `special' primop: ~s" op)))
      (begin
	;; usual case, op is a <string>
	(display op port)
	(write-multi-expr args port))))

(define (asm-primop tab insn)
  (asm "~>" tab)
  (let* ((port (current-output-port))
	 (primop (cadr insn))
	 (b (assq 'ccode (translations (actual-bdg primop)))))
    (if b
	(let ((op (cdr b)))
	  (display op port)
	  (write-multi-expr (cddr insn) port))
	(error "primop ~s not implemented for strategy ccode"
	       primop)))
  (asm ";\n"))

(define (asm-set! tab insn)
  (asm "~>" tab)
  (let ((ea (cadr insn))
	(expr (caddr insn))
	(port (current-output-port)))
    (case (car ea)
      ((reg)
       (if (reg-ref? expr)
	   (let ((to (cadr ea))
		 (from (reg-ref-reg expr)))
	     (if (eq? to from)
		 (format port "/* NOP: REG~d = REG~d; */\n" to from)
		 (begin
		   (write-reg ea port)
		   (asm* port " = ~e;\n" expr))))
	   (begin
	     (write-reg ea port)
	     (asm* port " = ~e;\n" expr))))
      ((lex-var) (if (< (cadr ea) 10)
		     (asm* port "LEXSET~d(~d~n,~e);\n"
			   (cadr ea)
			   (caddr ea)
			   (cadddr ea)
			   expr)
		     (asm* port "LEXSET(~d,~d~n,~e);\n"
			   (cadr ea)
			   (caddr ea)
			   (cadddr ea)
			   expr)))
      ((tl-var tl-var/b) 
       (asm* port "TLSET(~d~n,~e);\n" 
	     (cadr ea)
	     (caddr ea)
	     expr))
      ((root) (asm* port "~a = ~e;\n" (cadr ea) expr))
      (else   (error "Bad EA in set!")))))

(define (asm-jump tab insn)
  (asm "~>JUMP(~d,~l);\n" tab (cadr insn) (caddr insn)))

; should probably just look at the labels and SEE whether or not
; it is a backward jump...

(define (asm-bjump tab insn)
  (asm "~>BJUMP(~d,~l);\n" tab (cadr insn) (caddr insn)))

(define (asm-apply tab insn)
  (if (eq? (caaddr insn) '<function>)
      (asm "~>APPLYF(~d,~e);\n" tab (cadr insn) (caddr insn))
      (asm "~>APPLY(~d,~e);\n" tab (cadr insn) (caddr insn))))

(define (asm-applyg tab insn)
  (asm "~>APPLYG(~d,~e);\n" tab (cadr insn) (caddr insn)))

(define (asm-applyf tab insn)
  (asm "~>APPLYF(~d,~e);\n" tab (cadr insn) (caddr insn)))

(define (asm-return tab insn)
  (let ((n (cadr insn)))
    (if (or (eq? n 0) (eq? n 1))
	(asm "~>RETURN~d();\n" tab (cadr insn))
	(asm "~>RETURN(~d);\n" tab (cadr insn)))))

(define (asm-seq tab insn)
  (asm "~>{\n" (sub1 tab))
  (asm-stmts tab (cdr insn))
  (asm "~>}\n" (sub1 tab)))

(define (asm-do tab insn)
  (asm "~>~e;\n" tab (cadr insn)))

(define (asm-if tab insn)
  (asm "~>if (~e)\n" tab (cadr insn))
  (asm-stmt (add1 tab) (caddr insn))
  (asm "~>else\n" tab)
  (asm-stmt (add1 tab) (cadddr insn)))

(define (asm-check= tab insn)
  (asm "~>COUNT_ARGS(~d);\n" tab (cadr insn)))

(define (asm-check>= tab insn)
  (if (> (cadr insn) 0)
      (asm "~>COUNT_ARGS_AT_LEAST(~d);\n" tab (cadr insn))))

(define (asm-use-empty-envt tab insn)
  (asm "~>USE_EMPTY_ENVT();\n" tab))

(define (asm-use-function-envt tab insn)
  (asm "~>USE_FUNCTION_ENVT();\n" tab))

(define (asm-collect> tab insn)
  (if (< (cadr insn) 10)
      (asm "~>COLLECT~d();\n" tab (cadr insn))
      (asm "~>COLLECT(~d);\n" tab (cadr insn))))

(define (asm-set-false< tab insn)
  (if (< (cadr insn) 5)
      (asm "~>PAD_WITH_FALSE~d();\n" tab (cadr insn))
      (asm "~>PAD_WITH_FALSE(~d);\n" tab (cadr insn))))

(define (asm-bind tab insn)
  (if (< (length (cdr insn)) 10)
      (asm "~>BEGIN_BIND~d()\n" tab (length (cdr insn)))
      (asm "~>BEGIN_BIND(~d)\n" tab (length (cdr insn))))
  (let loop ((i 0) (e (cdr insn)))
    (if (pair? e)
	(begin
	  (asm "~>BIND_ARG(~d,~e);\n" (add1 tab) i (car e))
	  (loop (add1 i) (cdr e)))))
  (asm "~>END_BIND\n" tab))

(define (asm-unbind tab insn)
  (asm "~>POPENVT();\n" tab))

(define (asm-save tab insn)
  (if (< (cadr insn) 10)
      (asm "~>SAVE_CONT~d(~l);\n" tab (cadr insn) (caddr insn))
      (asm "~>SAVE_CONT(~d,~l);\n" tab (cadr insn) (caddr insn))))

(define (asm-restore tab insn)
  (if (< (cadr insn) 10)
      (asm "~>RESTORE_CONT~d();\n" tab (cadr insn))
      (asm "~>RESTORE_CONT(~d);\n" tab (cadr insn))))

(define *asm-stmts*
  (let ((tbl (make-symbol-table)))
    (for-each
     (lambda (b)
       (table-insert! tbl (car b) (cdr b)))
     (list (cons 'jump asm-jump)
	   (cons 'apply asm-apply)
	   (cons 'applyg asm-applyg)
	   (cons 'applyf asm-applyf)
	   (cons 'return asm-return)
	   (cons 'seq asm-seq)
	   (cons 'do asm-do)
	   (cons 'if asm-if)
	   (cons 'bind asm-bind)
	   (cons 'save asm-save)
	   (cons 'restore asm-restore)
	   (cons 'set! asm-set!)
	   (cons 'use-empty-envt asm-use-empty-envt)
	   (cons 'use-function-envt asm-use-function-envt)
	   (cons 'check= asm-check=)
	   (cons 'unbind asm-unbind)
	   (cons 'check>= asm-check>=)
	   (cons 'collect> asm-collect>)
	   (cons 'set-false< asm-set-false<)
	   (cons 'primop asm-primop)
	   (cons 'bjump asm-bjump)))
    tbl))

(define (asm-stmt tab insn)
  ((table-lookup *asm-stmts* (car insn)) tab insn))

(define (asm-stmts tab insns)
  (for-each
   (lambda (i)
     (asm-stmt tab i))
   insns))

;;;  write out some asm stmts
;;;
;;;  the `new-names' maps labels in the vinsns to valid C labels
;;;  (it's a hash table, whose keys are (union <fixnum> <symbol>))

(define (write-asm-stmts new-names vinsns)
  (thread-let ((*relabel-table* new-names))
    (newline)
    (asm-stmts 1 vinsns)))

;;;
;;;  a procedure to find all the primops used by a piece of code
;;;  (initially used to make sure we don't assign ccode strategy
;;;  to a bunch of code that contains a reference to a bytecode-only
;;;  primop like `fd-close')
;;;

(define (collect-primop-refs aml)
  (let ((tbl (make-symbol-table)))
    (define (check thing)
      (cond
       ((pair? thing)
	(check (car thing))
	(check (cdr thing)))
       ((instance? thing <primop>)
	(table-insert! tbl (name thing) thing))))
    ;;
    (check aml)
    (value-sequence tbl)))

