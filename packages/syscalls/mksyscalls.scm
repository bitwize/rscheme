#!nrs -script

(define *bc-extension-name* "sysio")
(define *bc-extension-number* 11)   ;; BCI_OS_EXTN
(define *bc-extension-defn-source* "sysio.scm")

(define *target-dest-dir* #f)

(define (prepend-dest-dir f)
  (if *target-dest-dir*
      (string-append *target-dest-dir* "/" f)
      f))

(define (main args)
  (if (and (> (string-length (car args)) 18)
           (string=? (substring (car args) 0 18) "--target-dest-dir="))
      (begin
        (set! *target-dest-dir* (substring (car args) 18))
        (main (cdr args)))
      (begin
        (set! *bc-extension-number* (string->number (car args)))
        (set! *bc-extension-name* (cadr args))
        (set! *bc-extension-defn-source* (caddr args))
        (do-file))))


;;
;;  process all `define-syscall' definitions
;;

;;
;;  this is called for each `define-syscall'
;;

(define (handle-syscall-defn form)
  (let ((name (cadr form))
	(args (caddr form))
	(results (if (memq '=> form)
		    (cadr (memq '=> form))
		    '()))
	(body (if (memq '=> form)
		  (caddr (memq '=> form))
		  (cadddr form))))
    (format #t "name: ~s\n" name)
    (format #t "args: ~s\n" args)
    (format #t "results: ~s\n" results)
    (format #t "body: ~s\n" body)
    (list name args results body)))

;;
;;  really handle it
;;

(define (gen-syscall-operation name args results body)
  ;;
  ;;  emit preamble
  ;;
  ;;  the preamble extracts the arguments from the BCI
  ;;  eval stack, binding them to C locals of the appropriate
  ;;  type
  ;;
  ;;  the preamble also contains a definition for the VALUES
  ;;  macro that assigns the return values into the BCI
  ;;  eval stack
  ;;
  (syscall-preamble name args results)
  ;;
  (write body)
  ;;
  ;;  the postamble cleans up, by undef'ing VALUES
  ;;
  (syscall-postamble name))

(define (construct-arg-views descriptor rhs arg-name)
  (let ((v (views-for-descriptor descriptor)))
    (if (pair? v)
	(map (lambda (view)
	       (cons (construct-view-decl view arg-name)
		     (string-append (construct-view-loader view rhs)
				    "."
				    (bc-datum-type descriptor))))
	     v)
	(list (cons (string-append "obj " (symbol->string arg-name))
		    (string-append rhs ".obj_val"))))))


(define (syscall-preamble name args results)
  ;;
  (display "{\n" )
  (for-each (lambda (i arg)
	      (let ((arg-name (car arg))
		    (arg-type (cadr arg)))
		(for-each
		 (lambda (var-init-pair)
		   (let ((var (car var-init-pair))
			 (init (cdr var-init-pair)))
		     (format #t "~a = ~a;\n" var init)))
		 (construct-arg-views
		  (descriptor-for-type arg-type)
		  (format #f "arg_p[~d]" (- -1 i))
		  arg-name))))
	    (reverse (range (length args)))
	    args)
  ;;
  (let ((rtn-names (map (lambda (num)
			  (format #f "v_~d" num))
			(range (length results)))))
    (format #t "#define VALUES(~a) do { \\\n" 
	    (string-join #\, rtn-names))
    (format #t "\t\targ_p -= ~d;\\\n" (- (length args)
					   (length results)))
    (for-each (lambda (i r t)
		(format #t "\t\targ_p[~d].~a = (~a);\\\n"
			(- i (length results))
			(bc-datum-type (descriptor-for-type t))
			r))
	      (range (length results))
	      rtn-names
	      results)
    (format #t "\t\tgoto ~a_done; } while (0)\n" (c-ify name))))

(define (syscall-postamble name)
  (format #t "\n#undef VALUES\n")
  (format #t "~a_done: break;\n" (c-ify name))
  (display "}\n" ))

  
  
;;
;;  this is called to finish up
;; 

(define (emit-extn-preamble)
  (format #t "UINT_8 *bc_~a_extension( UINT_8 *pc, RS_bc_datum **arg_lim )\n"
	  *bc-extension-name*)
  (format #t "{ RS_bc_datum *arg_p = *arg_lim;\n\n")
  (format #t "    switch (*pc++) {\n" ))

(define (emit-extn-postamble)
  (format #t "    }\n")
  (format #t "    *arg_lim = arg_p;\n")
  (format #t "    return pc;\n")
  (format #t "}\n"))

(define (emit-syscall-code forms)
  (format #t "~d syscalls\n" (length forms))
  (let ((p (open-output-file (prepend-dest-dir
                              (string-append *bc-extension-name* ".bcx")))))
    (with-output-to-file
	(prepend-dest-dir (string-append *bc-extension-name* ".ci"))
      (lambda ()
	(emit-extn-preamble)
	(emit-extn-body forms p)
	(emit-extn-postamble)))
    (close-output-port p)))
      
(define (emit-extn-body forms primops-port)
  (for-each 
   (lambda (i op)
     (let ((n (quotient (- 68 (string-length
			       (symbol->string (car op))))
			2)))
       (format #t "/~a ~a ~a\n" 
	       (make-string n #\*)
	       (car op)
	       (make-string n #\*)))
     (format #t " *  BCI Opcode: ~d\n" i)
     (format #t " *        Name: ~s\n" (car op))
     (if (null? (cadr op))
	 (format #t " *   Arguments: NONE\n")
	 (for-each 
	  (lambda (a)
	    (format #t " *    Argument: ~s\n" a))
	  (cadr op)))
     (format #t " *     Results: ~s\n" (caddr op))
     (format #t " */\n")
     (format #t "case ~d:\n" i)
     (apply gen-syscall-operation op)
     (apply gen-primop primops-port i op)
     (newline primops-port))
   (range (length forms))
   forms))

(define (gen-primop port opcode name args results body)
  (if (null? results)
      (write `(define-primop (,name ,@(map cadr args))
		(bytecode ,*bc-extension-number* ,opcode))
	     port)
      (if (null? (cdr results))
	  (write `(define-primop (,name ,@(map cadr args))
		    => ,(car results)
		    (bytecode ,*bc-extension-number* ,opcode))
		 port)
	  (write `(define-primop (,name ,@(map cadr args))
		    => ,results
		    (bytecode ,*bc-extension-number* ,opcode))
		 port)))
  (newline port))

(define (do-file)
  (call-with-input-file
      *bc-extension-defn-source*
    (lambda (port)
      (let ((r '()))
	(with-objects-from-port
	 port
	 (lambda (item)
	   (if (and (pair? item)
		    (eq? (car item) 'define-syscall))
	       (set! r (cons (handle-syscall-defn item) r)))))
	(emit-syscall-code (reverse r))))))


;;
;;  currently supported types and views
;;
;;  table ::= ((type-name recognition-style [view ...]) ...)
;;
;;  view ::= (var-decl-fmt var-init-fmt [local-name-fmt])
;;

;; note:  this is taken from corelib/safeglue.scm,
;;        which has the same descriptor format (on purpose)
;;
;; EXCEPT, these conversions are relative to the bc_datum types, instead
;; of being relative to the <obj>'s in REG(n)'s
;;

(define $auto-primop-types
    '((<raw-int> (primitive "OBJ_ISA_FIXNUM" <fixnum>)
		 bc-datum: "raw_int"
		 ("int ~a" "~a"))
      (<fixnum> (primitive "OBJ_ISA_FIXNUM" <fixnum>))
      (<string> (primitive "STRING_P" <string>))
      (<ascii-char> (primitive "OBJ_ISA_ASCII_CHAR" <ascii-char>))
      (<raw-ascii-char> (primitive "OBJ_ISA_ASCII_CHAR" <ascii-char>)
			bc-datum: "raw_int"
			("UINT_8 ~a" "~a"))
      (<raw-string> (primitive "STRING_P" <string>)
		    bc-datum: "raw_str"
		    ("char *~a" "~a"))))


(define (descriptor-for-type type)
  (let ((a (assq type $auto-primop-types)))
    (if a
	a
	'(<obj> ()))))

(define (views-for-descriptor d)
  (if (and (pair? (cddr d))
	   (eq? (caddr d) 'bc-datum:))
      (cddddr d)
      (cddr d)))


(define (view-name view base-name)
  (if (> (length view) 2)
      (format #f (caddr view) base-name)
      base-name))
  
(define (construct-view-decl view arg-base-name)
  (format #f (car view) (view-name view arg-base-name)))

(define (construct-view-loader view src-expr)
  (format #f (cadr view) src-expr))

(define (bc-datum-type type-info)
  (string-append (if (memq 'bc-datum: type-info)
		     (cadr (memq 'bc-datum: type-info))
		     "obj")
		 "_val"))

(define (c-ify sym)
  (list->string (map (lambda (ch)
		       (if (eq? ch #\-)
			   #\_
			   (if (or (char-alphabetic? ch)
				   (char-numeric? ch))
			       ch
			       #\_)))
		     (string->list (symbol->string sym)))))

  

