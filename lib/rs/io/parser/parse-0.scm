(define-class <parse-node> (<object>) :abstract
  start-token
  end-token)


;;;

(define (parse:parse-vector scanner first last start-line)
  (bind ((class item elem-line (parse:parse-object* scanner)))
    (if (eof-object? item)
	(parse:error scanner 
		    start-line
		    "<open-vector> missing <close-paren>")
	(if (eq? class 'end)
	    (case item
	      ((<close-paren>) (values 'result 
				       (list->vector first)
				       start-line))
	      ((<dot>)
	       (parse:error scanner
			   elem-line
			   "<dot> not understood in vector datum"))
	      (else (parse:internal-error 1 item)))
	    (let ((i (cons item '())))
	      (if (null? first)
		  (parse:parse-vector scanner i i start-line)
		  (begin
		    (set-cdr! last i)
		    (parse:parse-vector scanner first i start-line))))))))

(define (parse:parse-list scanner first last start-line)
  (bind ((class item elem-line (parse:parse-object* scanner)))
    (if (eof-object? item)
	(parse:error scanner start-line "<open-paren> missing <close-paren>")
	(if (eq? class 'end)
	    (case item
	      ((<close-paren>) (values 'result
				       first
				       start-line))
	      ((<dot>)
	       (if (null? first)
		   (parse:error scanner elem-line "<dot> before any items")
		   (bind ((class item tail-line (parse:parse-object* scanner)))
		     (if (eq? class 'result)
			 (if (eof-object? item)
			     (parse:error scanner
					 elem-line
					 "unexpected <eof> after <dot>")
			     (begin
			       (set-cdr! last item)
			       (bind ((class i l (parse:parse-object* scanner)))
				 (if (and (eq? class 'end)
					  (eq? i '<close-paren>))
				     (values 'result
					     first
					     start-line)
				     (parse:error
				      scanner
				      l
				      "unexpected ~s inside at end of ~s"
				      i 
				      first)))))
			 (parse:error scanner
				     tail-line
				     "unexpected ~s after <dot>" 
				     item)))))
	      (else (parse:internal-error 2 item)))
	    (let ((i (cons item '())))
	      (if (null? first)
		  (parse:parse-list scanner i i start-line)
		  (begin
		    (set-cdr! last i)
		    (parse:parse-list scanner first i start-line))))))))

(define (parse:parse-object* scanner)
  (bind ((token-type token-data token-line (scanner)))
    (case token-type
      ((<number> <symbol> <literal>) 
       (values 'result token-data token-line))
      ((<open-paren>) 
       (parse:parse-list scanner '() '() token-line))
      ((<close-paren> <dot>)
       (values 'end token-type token-line))
      ((<open-vector>)
       (parse:parse-vector scanner '() '() token-line))
      ((<curly-braced>)
       (values 'result
	       (make <curly-braced>
		     text: token-data)
	       token-line))
      ((quote unquote unquote-splicing quasiquote)
       (bind ((meta datum line (parse:parse-object* scanner)))
	 (if (eq? meta 'result)
	     (if (eof-object? datum)
		 (parse:error scanner
			     token-line
			     "unexpected <eof> after ~s" token-type)
		 (values 'result (list token-type datum) token-line))
	     (parse:error scanner 
			 token-line
			 "unexpected ~s after ~s" datum token-type))))
      (else
       (if (eof-object? token-type)
	   (values 'result token-type token-line)
	   (parse:error scanner
		       token-line
		       "Strange token in input: ~s ~s" 
		       token-type
		       token-data))))))

(define (parse:parse-object scanner)
  (bind ((type result line (parse:parse-object* scanner)))
    (if (eq? type 'end)
	(case result
	  ((<close-paren>) 
	   (parse:error scanner line "Unmatched <close-paren>"))
	  ((<dot>)
	   (parse:error scanner line "Misplaced <dot>"))
	  (else
	   (parse:internal-error 3 result)))
	(values result line))))

;;
;; we use closure introspection to avoid having a flag or
;; extra argument.  Specifically, we look at the environment
;; of the "scanner" closure to find the port that's being read
;; from.
;;

(define (scanner->port scanner)
  (gvec-ref (gvec-ref scanner 0) 2))

(define (scanner-for-port port)
  (let ((meth (find-method input-port-scan-token (list port)))
	(p port)) ;; copy of port for scanner->port to use
    (lambda ()
      (meth p))))

(define-method input-port-read ((self <input-port>))
  (parse:parse-object (scanner-for-port self)))

