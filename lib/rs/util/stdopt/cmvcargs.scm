;;
;;  parse a cmvc-style argument list
;;
;;  which is:
;;    (1) exactly one ACTION flag
;;    (2) zero or more ATTRIBUTE and FLAG arguments, with their
;;        associated values (flag values are #t or #f)
;;
;;  the environment determines:
;;    1. the set of actions
;;    2. the set of recognized attributes
;;    3. the set of recognized flags 

;;  the action determines:
;;    1. the set of valid attributes, their domains, and their default values
;;    2. the set of valid flags and their default values
;;

;; the structure of the ENVT list is a table mapping arguments
;; to a <arg-descriptor>

;;
;;  parse-cmvc-style always returns two values, which are the
;;  action object selected and a (possibly empty) list 
;;  of arguments for that procedure.
;;
;;  if the arguments do not constitute a valid invocation, an error is
;;  signalled
;;
;;  this function returns the action and args so that the caller
;;  can fix things up (last minute default values, etc) before calling
;;  the action procedure
;;
;;  an alternative, simpler interface using a thunk to fix things up, 
;;  is the parse-std-args function in cmdlib.scm

(define (parse-cmvc-style envt args)
  (letrec ((found-items '())
	   (found-action #f)
	   (action-args '())
	   ;;
	   (done (lambda ()
		   (if found-action
		       ;;
		       ;; validate everything
		       ;;
		       (values found-action action-args)
		       (error "no action flag specified.  Use one of: ~j"
			      (sort 
			       (uniq
				(map name
				     (select (lambda (i)
					       (instance? i <action-descriptor>))
					     (table-values->list envt))))
			       string<?)))))
	   (parse-item
	    (lambda (a)
	      (if (null? a)
		  (done)
		  (let ((b (table-lookup envt (car a))))
		    ;(format #t "~s => ~s\n" (car a) b)
		    (if b
			(if (instance? b <action-descriptor>)
			    (if found-action
				(error "action ~s specified with ~s"
				       (car a) 
				       (name found-action))
				(begin
				  (set! found-action b)
				  (if (instance? b <action-attrib>)
				      (if (singled-valued? b)
					  (if (pair? (cdr a))
					      (begin
						(set! action-args
						      (list (string->arg b (cadr a))))
						(parse-item (cddr a)))
					      (error "action ~s missing a value" (name b)))
					  (parse-action-values (cdr a) b))
				      (parse-item (cdr a)))))
			    (if (memq b found-items)
				(error "parameter ~s given twice" (car a))
				(begin
				  (set! found-items (cons b found-items))
				  (if (instance? b <flag-descriptor>)
				      (begin
					(set-value! (flag-binding b) (use-value b))
					(parse-item (cdr a)))
				      (if (single-valued? b)
					  (if (pair? (cdr a))
					      (begin
						(set-value! (attrib-binding b) (string->arg b (cadr a)))
						(parse-item (cddr a)))
					      (error "attribute ~s missing a value" (car a)))
					  (parse-attrib-values (cdr a) b))))))
			(error "expected an attribute or flag argument, saw ~s" (car a)))))))
	   ;;
	   (parse-action-values
	    (lambda (a b)
	      (bind ((vals rest (parse-attrib-values* envt b a)))
		(if (null? vals)
		    (error "expected a list of values for action ~s" (name b))
		    (begin
		      (set! action-args (list vals))
		      (parse-item rest))))))
	   ;;
	   (parse-attrib-values 
	    (lambda (a b)
	      (bind ((vals rest (parse-attrib-values* envt b a)))
		(if (null? vals)
		    (error "expected a list of parameter values for ~s" (name b))
		    (begin
		      (set-value! (attrib-binding b) vals)
		      (set! found-items (cons b found-items))
		      (parse-item rest)))))))
    (parse-item args)))

(define (parse-attrib-values* envt b a)
  (let loop ((a a) (r '()))
    (if (null? a)
	(values (reverse r) '())
	(let (((str <string>) (car a)))
	  (if (and (> (string-length str) 1)
		   (eq? (string-ref str 0) #\-))
	      (begin
		(if (not (table-lookup envt str))
		    (error "option `~a' is invalid or ambiguous" str))
		(values (reverse r) a))
	      (loop (cdr a)
		    (cons (string->arg b (car a)) r)))))))

(define (show-usage-for-action process action)
  (letrec ((s0 (format #f "~a ~a~a~a~a" 
		       process 
		       (if (default? action)
			   (format #f "[~a]" (name action))
			   (name action))
		       (if (instance? action <action-attrib>)
			   " "
			   "")
		       (if (instance? action <action-attrib>)
			   (value-description action)
			   "")
		       (if (and (instance? action <action-attrib>)
				(not (singled-valued? action)))
			   "..."
			   "")))
	   (indent-len (+ (string-length s0) 1))
	   (indenter (make-string indent-len #\space)))
    (display s0)
    (let loop ((i (string-length s0))
	       (opts (append (required-items action)
			     (optional-items action))))
      (if (pair? opts)
	  (let* ((opt (car opts))
		 (s (if (instance? opt <flag-descriptor>)
			(name opt)
			(format #f "~a ~a~a" (name opt) (value-description opt) (if (single-valued? opt) "" "..."))))
		 (s2 (if (memq opt (optional-items action))
			 (string-append "[" s "]")
			 s)))
	    (if (>= (+ i (string-length s2) 1) 72)
		(begin
		  (newline)
		  (display indenter)
		  (set! i indent-len))
		(begin
		  (display " ")
		  (set! i (+ i 1))))
	    (display s2)
	    (loop (+ i (string-length s2)) (cdr opts)))
	  (newline)))))


(define (show-usage process envt)
  ;;
  ;; find all the actions
  ;;
  (let ((list '()))
    (table-for-each envt
		    (lambda (h k v)
		      (if (instance? v <action-descriptor>)
			  ;;
			  ;; don't count any abbreviations
			  ;;
			  (if (string=? k (name v))
			      (set! list (cons v list))))))
    ;;
    ;; construct a usage message from the argument envt
    ;;
    (for-each (lambda (a)
		(show-usage-for-action process a)
		(newline))
	      (sort list (lambda (a b)
			   (string<? (name a) (name b)))))))

(define (uniq lst)
  (let loop ((d '())
	     (s lst))
    (if (null? s)
	d
	(loop (if (memq (car s) d)
		  d
		  (cons (car s) d))
	      (cdr s)))))
