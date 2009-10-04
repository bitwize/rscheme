#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/cgen/assem.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.12
 | File mod date:    1999-02-12 09:30:34
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#



(define-syntax (asm format-str . arg-list)
  (asm* (current-output-port) format-str . arg-list))

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

(define (write-name port item)
    (if item
	(let ((str (object->string item)))
	    (if (not (or (string-search str "*/")
	    	         (string-search str "/*")))
		(asm* port " /* ~a */" str)))))

(define (write-label port item)
    (let ((new-label (table-lookup (fluid-ref *relabel-table*) item)))
	(display new-label port)))

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
	 (error/internal "unrecognized `special' primop: ~s" op)))
      (begin
	;; usual case, op is a <string>
	(display op port)
	(write-multi-expr args port))))

(define (write-expr port expr)
  (let ((item (cdr expr)))  ;; strip off leading type
    (case (car item)
      ((seq) (write-multi-expr (cdr item) port))
      ((primop)
       (let* ((primop (cadr item))
	      (b (assq 'ccode (translations (actual-bdg primop)))))
	 (if b
	     (write-primop-expr port (cdr b) (cddr item))
	     (error/internal "primop ~s not implemented for strategy ccode"
			     primop))))
      ((ref) (let ((ea (cadr item)))
	       (case (caadr item)
		 ((reg) (write-reg ea port))
		 ((lex-var)
		  (if (< (cadr ea) 10)
		      (asm* port "LEXREF~d(~d)~n"
			    (cadr ea)
			    (caddr ea)
			    (cadddr ea))
		      (asm* port "LEXREF(~d,~d)~n"
			    (cadr ea)
			    (caddr ea)
			    (cadddr ea))))
		 ((tl-var)
		  (asm* port "TLREF(~d)~n" 
			(cadr ea) 
			(caddr ea)))
		 ((tl-var/b)
		  (asm* port "TLREFB(~d)~n" 
			(cadr ea) 
			(caddr ea)))
		 ((root) (display (cadr ea) port))
		 (else (error/internal "Bad EA in ref")))))
      ((literal) 
       (asm* port "LITERAL(~d)~n" 
	     (cadr item) 
	     (caddr item)))
      ((int)
       (asm* port "~d" (cadr item)))
      ((immob)
       (let ((b (assq (cadr item)
		      '((#f "FALSE_OBJ")
			(#t "TRUE_OBJ")
			(() "NIL_OBJ")
			(#none "NOVALUE_OBJ")
			(#undef "UNDEFINED_OBJ")
			(#uninit "UNINITIALIZED_OBJ")
			(#unbound "UNBOUND_OBJ")
			(#key "KEY_OBJ")
			(#rest "REST_OBJ")))))
	 (if b
	     (display (cadr b) port)
	     (if (char? (cadr item))
		 (asm* port "MAKE_ASCII_CHAR(~d)" (char->integer (cadr item)))
		 (error/internal "Bad immob: ~s" item)))))
      ((if)
       (asm* port "(~e ? ~e : ~e)"
	     (cadr item)
	     (caddr item)
	     (cadddr item)))
      ((closure)
       (asm* port "CLOSURE(~d)" (cadr item)))
      ((this-function)
       (asm* port "THIS_FUNCTION()"))
      (else (error/internal "Bad expr: ~a" item)))))

(define (asm-primop tab insn)
  (asm "~>" tab)
  (let* ((port (current-output-port))
	 (primop (cadr insn))
	 (b (assq 'ccode (translations (actual-bdg primop)))))
    (if b
	(let ((op (cdr b)))
	  (display op port)
	  (write-multi-expr (cddr insn) port))
	(error/internal "primop ~s not implemented for strategy ccode"
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
      (else   (error/internal "Bad EA in set!")))))
    
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

(define *asm-stmts* #f)

(define (asm-stmt tab insn)
    (if (not *asm-stmts*)
	(set! *asm-stmts*
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
		    (cons 'bjump asm-bjump))))
    ((cdr (assq (car insn) *asm-stmts*)) tab insn))
    
(define (asm-stmts tab insns)
    (for-each 
	(lambda (i) 
	    (asm-stmt tab i)) 
	insns))

(define (write-asm-stmts new-names vinsns)
  (fluid-let ((*relabel-table* new-names))
    (newline)
    (asm-stmts 1 vinsns)))

(define (symbol+integer->hash x)
    (if (symbol? x) 
	(symbol->hash x)
	(integer->hash x)))

(define (relabel-monotones base-name monotones label-table)
    (let loop ((i 0) (m monotones) (r '()))
	(if (null? m)
	    (reverse r)
	    (let ((new-label (format #f "~a_~d" base-name i)))
		(table-insert! label-table (caar m) new-label)
		(loop (add1 i)
		      (cdr m)
		      (cons (cons new-label (cdar m)) r))))))
		      
(define (vinsns->monotones* name vinsns)
    (let loop ((accum '()) (i vinsns))
	(if (null? i)
	    (list (cons name (reverse accum)))
	    (if (eq? (caar i) 'label)
		(cons (cons name (reverse accum))
		      (vinsns->monotones* (cadar i) (cdr i)))
		(loop (cons (car i) accum) (cdr i))))))


(define (write-tab port tab-stop)
  (let loop ((i tab-stop))
    (if (> i 1)
	(begin
	  (output-port-write-char port #\tab)
	  (loop (sub1 (sub1 i))))
	(write-string port "    "))))

(if-implements
 (version >= 0 7 3 1)
 (define (fmt-proc ch proc)
   (cons ch
	 (lambda (info)
	   (values 1 proc))))
 (define (fmt-proc ch proc)
   (cons* ch
	  (lambda (port info arg)
	    (proc port arg))
	  1)))

(define asm*
  (make-formatter (list (fmt-proc #\> write-tab)
			(fmt-proc #\e write-expr)
			(fmt-proc #\n write-name)
			(fmt-proc #\l write-label))))
