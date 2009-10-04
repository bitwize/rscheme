(define-thread-var *relabel-table*)

(define (write-tab port tab-stop)
  (let loop ((i tab-stop))
    (if (> i 1)
	(begin
	  (output-port-write-char port #\tab)
	  (loop (sub1 (sub1 i))))
	(write-string port "    "))))

(define (write-name port item)
  (if item
      (let ((str (format #f "~s" item)))
	(if (not (or (string-search str "*/")
		     (string-search str "/*")))
	    (asm* port " /* ~a */" str)))))

(define (write-label port item)
  (let ((new-label (table-lookup *relabel-table* item)))
    (display new-label port)))

(define (write-expr port expr)
  (let ((item (cdr expr)))  ;; strip off leading type
    (case (car item)
      ((seq) (write-multi-expr (cdr item) port))
      ((primop)
       (let* ((primop (cadr item))
	      (b (assq 'ccode (translations (actual-bdg primop)))))
	 (if b
	     (write-primop-expr port (cdr b) (cddr item))
	     (error "primop ~s not implemented for strategy ccode"
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
		 (else (error "Bad EA in ref")))))
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
		 (error "Bad immob: ~s" item)))))
      ((if)
       (asm* port "(~e ? ~e : ~e)"
	     (cadr item)
	     (caddr item)
	     (cadddr item)))
      ((closure)
       (asm* port "CLOSURE(~d)" (cadr item)))
      ((this-function)
       (asm* port "THIS_FUNCTION()"))
      (else (error "Bad expr: ~a" item)))))

(define (fmt-proc ch proc)
  (cons* ch
	 (lambda (port info arg)
	   (proc port arg))
	 1))


(define asm*
  (make-formatter (list (fmt-proc #\> write-tab)
			(fmt-proc #\e write-expr)
			(fmt-proc #\n write-name)
			(fmt-proc #\l write-label))))

(define-syntax (asm format-str . arg-list)
  (asm* (current-output-port) format-str . arg-list))

;;;

(define (center-* port format-str . format-args)
  (let* ((line (string-join
		"*_/"
		(string-split (apply format #f format-str format-args)
			      "*/")))
	 (stars (make-string (max 1 (quotient (- 72 (string-length line)) 2))
			     #\*)))
    (format port "/~a ~a ~a/\n" stars line stars)))
