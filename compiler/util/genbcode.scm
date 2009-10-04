#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/util/genbcode.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.16
 | File mod date:    2003-08-13 21:20:22
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#

;;
;; this is designed to work AFTER (create-distribution)
;; (obviouslly, because it installs it's work products into
;;  the distribution)
;;

(define *primop-name* #f)

(define (include-defs-file f)
  (let* ((rel-f (if *relative-file-paths*
		    (string->file f)
		    (append-path (current-directory) (string->file f))))
	 (port (open-input-file (pathname->os-path rel-f))))
    (within-directory
     (or (file-directory rel-f)
	 (string->dir "."))
     (lambda ()
       (let loop ()
	 (let ((datum (read port)))
	   (if (eof-object? datum)
	       (close-input-port port)
	       (begin
		 (process-declaration datum)
		 (loop)))))))))

(define (process-defs-file src)
    (set! *num-opcodes* 0)
    (set! *num-primops* 0)
    (set! *primop-table* '())
    (set! *opcode-table* '())
    (call-with-output-path
	(append-path *dist-path* (string->file "bci/bcfrags.ci"))
      (lambda (f1)
	(set! *frags* f1)
	(call-with-output-path
	    (install-resource-path "compiler/bytecode/bcgen.scm")
	  (lambda (f2)
	    (set! *code-generator* f2)
	    (include-defs-file src)
	    (generate-primop-frag)))))
    (call-with-output-path
	(install-resource-path "compiler/bytecode/potable.dat")
      (lambda (p)
	(write *primop-table* p)))
    (call-with-output-path
	(install-resource-path "compiler/bytecode/bctable.dat")
      (lambda (p)
	(write *opcode-table* p)))

    ; write out byte-code jump tables for compilers that
    ; support computed gotos.

    (let ((write-byte-code-jump-table
	   (lambda (file-name prefix num-codes max-codes last-string)
	       (call-with-output-path 
	           (append-path *dist-path* (string->file file-name))
		 (lambda (p)
		   (letrec
		       ((last-code (- max-codes 1))
			(loop (lambda (bc)
				(cond
				 ((< bc num-codes)
				  (begin
				    (format p "  &&~a__~a~a\n" 
					    prefix bc 
					    (if (= bc last-code) "" ","))
				    (loop (+ bc 1))))
				 ((< bc max-codes)
				  (begin
				    (format p "  &&~a__error~a\n" 
					    prefix 
					    (if (= bc last-code) "" ","))
				    (loop (+ bc 1))))))))
		     (loop 0)
		     (format p last-string)))))))
      (write-byte-code-jump-table 
       "bci/bcojump.ci" "bco" *num-opcodes* 254 ", &&bco__254, &&bco__255\n")
      (write-byte-code-jump-table 
       "bci/bcpjump.ci" "bcp" *num-primops* 256 "")))

    
(define *num-opcodes* 0)
(define *opcode-table* '())
(define *primop-table* '())
(define *num-primops* 0)

(define (escape-format-specifiers item)
  (if (string? item)
      (string-join "%%" (string-split item #\%))
      (let ((o (open-output-string)))
	(write item o)
	(escape-format-specifiers (close-output-port o)))))

;; primop defn:
;;
;; <vector>
;; [0] name
;; [1] args
;; [2] result
;; [3] c-expr
;; [4] primopcode

;; write out the interpreter/compiler fragment that handles the primops

(define (generate-primop-frag)
  (format *frags* "\nBCI_CASE(255)\n")
  (format *frags* "    debug_bytecode_printf(( \"primop: \" ));\n" );
  (format *frags* "    BCI_PRIMOP_SWITCH(*pc++) {\n")
  (for-each
   (lambda (prim)
     (let ((xlations (vector-ref prim 3)))
       (format *frags* "BCI_PRIMOP_CASE(~d) /* ~s */\n{\n"
	       (cdr (assq 'bytecode xlations))
	       (vector-ref prim 0))
       (let ((args (make-seq))
	     (depth (length (vector-ref prim 1)))
	     (result (vector-ref prim 2)))
	 (for-each
	  (lambda (a)
	    (set! depth (- depth 1))
	    (seq-add! args (format #f "TOPT(~a,~d)"
				   (eval-ftype-for a)
				   depth)))
	  (vector-ref prim 1))
	 
	 (if result
	     (format *frags* "~a result;\n" (c-type-for result)))

	 (format *frags* "   debug_bytecode_printf(( \"~a (~d args)\\n\" ));\n"
		 (escape-format-specifiers (vector-ref prim 0))
		 (length (vector-ref prim 1)))
	 (format *frags* "~a~a(~a);\n"
		 (if result 
		     (if (eq? result '<raw-bool>)
			 "result = (rs_bool)"
			 "result = ")
		     "")
		 (cdr (assq 'ccode xlations))
		 (comma-sep (seq->list args)))
	 (format *frags* "NPOP(~d);\n" (length (vector-ref prim 1)))
	 (if result
	     (format *frags* "PUSHT(~a,result);\n" (eval-ftype-for result)))
	 (format *frags* "BCI_BREAK;\n")
	 (format *frags* "\n}\n"))))
   (reverse *primop-table*))
  (format *frags* "\n}\n"))


;; returns the stem name for the eval stack union member that
;; contains these kinds of primtypes

(define (eval-ftype-for t)
  (eval-stack-union-member-stem (lookup-prim-type t)))

(define (c-type-for t)
  (c-type-name (lookup-prim-type t)))

(define (assign-opcode num-extension-bytes vinsn args)
  (let* ((i *num-opcodes*)
	 (descrip (list i vinsn args num-extension-bytes)))
    ;(format #t "assigned opcode ~d: ~s\n" i (cdr descrip))
    (set! *opcode-table* (cons descrip *opcode-table*))
    (set! *num-opcodes* (+ i 1))
    i))

(define (item->str h)
  (if (symbol? h)
      (symbol->string h)
      (if (string? h)
	  h
	  (object->string h))))

(define (comma-sep lst)
    (if (null? lst)
	""
	(if (null? (cdr lst))
	    (item->str (car lst))
	    (string-append (item->str (car lst))
	    	           ","
			   (comma-sep (cdr lst))))))

(define *code-generator* (current-output-port))
(define *frags* (current-output-port))

;; bcd is a bytecode declaration we read from the defs file

(define (process-declaration d)
  (case (car d)
    ((bytecode)
     (process-bytecode-declaration d))
    ((define)
     (write d *code-generator*)
     (newline *code-generator*))
    ((include)
     (include-defs-file (cadr d)))
    ((define-primop)
     (process-primop-declaration d))
    (else
     (abort 'process-declaration
	    "Invalid decl: ~s" d))))

;;
;; primop declarations look like
;;
#|
   (define-primop NAME ( ARGDECL ... [=> RETURNDECL]) IMPLEMENTATION)

   where:
      ARGDECL ::= PRIMTYPE | ( NAME PRIMTYPE )
      RETURNDECL ::= PRIMTYPE
      IMPLEMENTATION ::= STRING
      NAME ::= SYMBOL
      PRIMTYPE ::= SYMBOL which denotes a primtype

  note this syntax is analgous to that of define-method
  there may be the possibility of polymorphic primops later on...
|#

(define (process-primop-declaration primop-def)
  (let ((name (cadr primop-def))
	(args (caddr primop-def))
	(result #f)
	(c-expr (cadddr primop-def)))
    ;;
    ;; parse the result type...
    ;;
    (if (memq '=> args)
	(begin
	  (set! result (cadr (memq '=> args)))
	  ;; slow but easy to write...
	  (set! args (reverse (cddr (reverse args))))))
    ;;
    ;; strip off names, which are for documentation only...
    ;;
    (set! args (map (lambda (arg)
		      (if (pair? arg)
			  (cadr arg)
			  arg))
		    args))
    ;;
    ;; validate given types
    ;;
    (for-each lookup-prim-type args)
    (if result
	(lookup-prim-type result))
    ;;
    ;; add the primop
    ;;
    (set! *primop-table* 
	  (cons (vector name
			args
			result
			(list (cons 'ccode c-expr)
			      (cons 'bytecode *num-primops*)))
		*primop-table*))
    (set! *num-primops* (+ 1 *num-primops*))))
  
(define (process-bytecode-declaration bcd)
  (let ((name (caadr bcd))
	(args (cdadr bcd))
	(body (cddr bcd)))
    (fluid-let ((*primop-name* name))
      (format *code-generator* "\n;; bytecode ~s\n" name)
      (let ((emitter-name (string->symbol 
			   (string-append "emit-"
					  (symbol->string name)))))
	(write `(define (,emitter-name ,@args)
		  ,@(map assign-byte-codes body))
	       *code-generator*))
      (newline *code-generator*))))

(define (parse-decls decls)
    (let loop ((src decls) (dst '()))
	(if (null? src)
	    (reverse dst)
	    (let ((d (car src)))
		(loop (cdr src)
		      (cons
			 (cond
			    ((symbol? d)
				(vector d d '<uint-8>))
			    ((pair? d)
				(if (pair? (cddr d))
				    (vector (car d) (cadr d) (caddr d))
				    (vector (car d) (cadr d) '<uint-8>)))
			    (else
				(abort 'parse-decls
					"Bad decl: ~s" d)))
			  dst))))))

(define (compute-ext-bytes-len vars)
  (apply + (map (lambda (v)
		  (case (vector-ref v 2)
		    ((<uint-8>) 1)
		    ((<uint-16> <int-16>) 2)
		    ((<int-32>) 4)))
		vars)))

(define (gen-arg-extractor* p v)
  (let ((n (vector-ref v 0))) ; arg name
    (format p "      ~a = " n)
    (case (vector-ref v 2)
      ((<uint-8>)
       (format p "pc[0];\n" n)
       (values 1 "%u"))
      ((<uint-16>)
       (format p "(pc[0] << 8) + pc[1];\n" n)
       (values 2 "%u"))
      ((<int-16>)
       (format p "(pc[0] << 8) + pc[1];\n" n)
       (values 2 "%d"))
      ((<int-32>)
       (format p "(pc[0]<<24) + (pc[1]<<16) + (pc[2] << 8) + pc[3];\n")
       (values 4 "%ld")))))

(define (gen-arg-extractor port v)
  (bind ((len fmt (gen-arg-extractor* port v))
	 (n (vector-ref v 0)))
    ;; increment the bytecode code pointer
    (format port "      pc += ~d;\n" len)
    ;; arrange to print the actual bytecode args at runtime (if enabled)
    (format port "      debug_bytecode_printf(( \" ~a = ~a\", ~a ));\n"
	    n
	    fmt
	    n)))

(define (assign-new-byte-code vinsn ext-byte-decls body)
  (let* ((vars (parse-decls ext-byte-decls))
	 (opcode (assign-opcode (compute-ext-bytes-len vars) vinsn vars)))
    ;; generate the bytecode interpreter fragment for
    ;; this bytecode
    (format *frags* "BCI_CASE(~d)\n" opcode)
    (format *frags* "    debug_bytecode_printf(( \"~a: \" ));\n" 
	    (escape-format-specifiers (format #f "~j" vinsn)))
    (if (not (null? ext-byte-decls))
	(begin
	  (format *frags* "     {\n")
	  (for-each
	   (lambda (v)
	     (format *frags* "      ~a ~a;\n"
		     (case (vector-ref v 2)
		       ((<uint-8>) "UINT_8")
		       ((<uint-16>) "UINT_16")
		       ((<int-16>) "INT_16")
		       ((<int-32>) "INT_32")
		       (else (error/internal 
			      "bad typ: ~s" 
			      (vector-ref v 2))))
		     (vector-ref v 0)))
	   vars)
	  (for-each
	   (lambda (v)
	     (gen-arg-extractor *frags* v))
	   vars)))
    (format *frags* "    debug_bytecode_printf(( \"\\n\" ));\n")
    (for-each
     (lambda (line)
       (format *frags* "\t~a\n" line))
     body)
    (if (not (null? vars))
	(format *frags* "    }\n"))
    (format *frags* "    BCI_BREAK;\n")
    
    ;; return a peice of scheme (source) code that causes this
    ;; bytecode to be emitted
    (if (null? vars)
	`(emit-byte-code ,opcode)
	`(begin (emit-byte-code ,opcode)
		,@(map (lambda (v)
			 (case (vector-ref v 2)
			   ((<uint-8>)
			    `(emit-byte-code-check 
			      ,(vector-ref v 1)))
			   ((<uint-16>)
			    `(emit-byte-code-16-check 
			      ,(vector-ref v 1)))
			   ((<int-16>)
			    `(emit-byte-code-s16-check 
			      ,(vector-ref v 1)))
			   ((<int-32>)
			    `(emit-byte-code-s32 
			      ,(vector-ref v 1)))
			   (else
			    (abort 'assign-new-byte-code 
				   "bum type: ~s" 
				   (vector-ref v 2)))))
		       vars)))))

(define (translate/assign-code expr)
  (cond
   ((or (not (list? expr))
	(< (length expr) 3))
    (abort 'assign-byte-codes "Bad assign-code form: ~s\n" expr))
   ;
   ((or (null? (cadr expr))
	(not (eq? (caadr expr) 'quote)))
    (let ((use-name (cons (fluid-ref *primop-name*) (cadr expr))))
      (translate/assign-code (cons* 'assign-code
				    (list 'quote use-name)
				    (cdr expr)))))
   ;
   (else
    (let* ((vinsn (cadr (cadr expr)))
	   (args (caddr expr))
	   (body (cdddr expr))
	   (descrip (list vinsn args)))
      (assign-new-byte-code vinsn args body)))))

(define (assign-byte-codes expr)
  (cond
   ((and (pair? expr) (eq? (car expr) 'assign-code))
    (translate/assign-code expr))
   ((pair? expr)
    (cons (assign-byte-codes (car expr))
	  (assign-byte-codes (cdr expr))))
   (else
    expr)))

(define (create-primop-module name)
  (let* ((pos *primop-table*)
	 (envt (make-top-level-contour))
	 (ibs (make-table eq? symbol->hash))
	 (scheme-xbt #f)
	 (lcmd #f)
	 (im #f)
	 (m (make <module>
		  name: 'primops
		  top-level-envt: envt
		  module-exports: (table envt))))
    (for-each
     (lambda (prim)
       (let* ((prim-name (vector-ref prim 0))
	      (full-bdg #f))
	 (table-insert!
	  (table envt)
	  prim-name
	  (make <primop>
		name: prim-name
		arg-types: (vector-ref prim 1)
		result-type: (vector-ref prim 2)
		full-procedure-bdg: full-bdg
		primop-has-side-effect?: (if (vector-ref prim 2)
					     ;; by default, assume that if it
					     ;; returns a value, it is
					     ;; side-effect-free
					     #f
					     #t)
		translations: (vector-ref prim 3)))))
       pos)
    (let ((p (install-resource-path "modules/primops.mif")))
      (ensure-directory (file-directory p))
      (compiler-save-module (pathname->string p) m))
    ;
    (write-mx-file (install-resource-path "modules/primops.mx")
		   name
		   "primops"
		   '() ; unit names
		   '() ; bytecode extns
		   '() ; prereqs
		   '()); other libs
    ;;
    ;(format #t "primops:\n~a\n" mx)
    m))
