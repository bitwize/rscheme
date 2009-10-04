#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/objsys/newfm.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    1997-11-29 23:10:38
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose:          new find-method for multimethods (NOT IMPLEMENTED YET)
 `------------------------------------------------------------------------|#

(define-class <singleton> (<object>)
  singleton-value)

;; note that this CANT be a generic function,
;; and cant USE generic functions...
;;
;; so we build in the special case...


(define-syntax std-generic-function-methods (syntax-form (gf)
  (gvec-ref gf 2)))

(define-syntax method-function-specializers (syntax-form (m)
  (gvec-ref m 2)))

(define (nfind-method (gf <generic-function>) (args <pair>))
  (let loop ((method-list (std-generic-function-methods gf)))
    (if (pair? method-list)
	(if (method-accepts? (car method-list) args)
	    (car method-list)
	    (loop (cdr method-list)))
	#f)))

;;
;; this function is used to determine where in a list of
;; methods to insert a new methods
;;
;; returns #f if spec-1 is not compatible with spec-2
;;   (that is, something that matches spec-1 will not match spec-2)
;; returns sub if the things spec-1 accepts is a subset
;;        of the things spec-2 accepts (that is, spec-1 is MORE SPECIALIZED
;;        than spec-2)
;; returns equal if spec-1 and spec-2 accept exactly the same args
;;

(define (make-type-iterator spec)
  (lambda ()
    (if (pair? spec)
	(let ((type (car spec)))
	  (set! spec (cdr spec))
	  (values type #t))
	(if (null? spec)
	    (values #f #f)
	    (values spec #f)))))

;; returns = if spec-1 is equivalent to spec-2
;;         < if spec-1 is MORE SPECIFIC than spec-2
;;         > if spec-1 is LESS SPECIFIC than spec-2
;;        #f if spec-1 is incommensurate with spec-2
;;
;; examples:
;;    spec-1 
;;    (<number> <integer>)  (<integer> <number>)    ==> #f
;;    (<pair> <integer>)    (<list> <number>)       ==> <
;;    (<pair> <number>)     (<pair> <integer>)      ==> >
;;    (<pair> <number>)     (<pair> <number>)       ==> =
;;    (<pair> . <pair>)     (<pair> . <list>)       ==> <
;;    (<pair> . <list>)     <pair>                  ==> >
;;    (<pair> . <list>)     <list>                  ==> <


(define (compare-type a b)
  (cond
   ((eq? a b) '=)
   ((subclass? a b) '<)
   ((subclass? b a) '>)
   (else #f)))

(define (compare-specializer spec-1 spec-2)
  (let ((orientation '=)
	(A (make-type-iterator spec-1))
	(B (make-type-iterator spec-2)))
    (let loop ()
      (bind ((a more-a? (A))
	     (b more-b? (B)))
	(format #t "comparing ~s v. ~s\n" a b)
	(if (or more-a? more-b?)
	    (if a
		(if b
		    (case (compare-type a b)
		      ((<)
		       ;; a more specific than b
		       (if (eq? orientation '<)
			   (loop)
			   (if (eq? orientation '=)
			       (begin
				 (set! orientation '<)
				 (loop))
			       #f)))
		      ((>)
		       ;; b more specific than a
		       (if (eq? orientation '>)
			   (loop)
			   (if (eq? orientation '=)
			       (begin
				 (set! orientation '>)
				 (loop))
			       #f)))
		      ((=)
		       ;; same
		       (loop))
		      ((#f)
		       #f))
		    ;; no more items in b
		    (if more-a?
			#f
			orientation))
		;; out of a
		(if more-b?
		    #f
		    orientation))
	    ;; compare the #rest type
	    (if (eq? orientation '=)
		(compare-type a b)
		(case (compare-type a b)
		  ((<)
		   (if (eq? orientation '<)
		       '<
		       #f))
		  ((=)
		   orientation)
		  ((>)
		   (if (eq? orientation '>)
		       '>
		       #f))
		  ((#f) #f))))))))

(define (specializer-includes? spec datum)
  (if (instance? spec <singleton>)
      (eq? (singleton-value spec) datum)
      (subclass? (object-class datum) spec)))

(define (method-accepts? (m <method>) (args <list>))
  (let loop ((spec (method-function-specializers m))
	     (given args))
    (if (pair? spec)
	(if (pair? given)
	    (if (specializer-includes? (car spec) (car given))
		(loop (cdr spec) (cdr given))
		#f)
	    #f)
	(if (null? spec)
	    ;;
	    ;; no #rest accepted
	    ;;
	    (null? given)
	    ;;
	    ;; otherwise, rest accepted...
	    ;; special case #rest x :: <object>
	    ;;
	    (if (eq? spec <object>)
		#t
		;;
		;; otherwise, ensure that all the remaining given args
		;; match the #rest specialization
		;;
		(let rest-loop ((given given))
		  (if (pair? given)
		      (if (specializer-includes? spec (car given))
			  (rest-loop (cdr given))
			  #f)
		      #t)))))))


;;
;; slow GFs
;;

(define-class <slow-generic-function> (<generic-function>))

(define slow-gf-proc 
  (let ((owner-gf #f))
    (lambda args
      (let ((m (nfind-method owner-gf args)))
	(if m
	    (apply* args m)
	    (does-not-understand owner-gf args))))))
      
(define slow-generic-function-dispatch (template slow-gf-proc))

(define (make-slow-gf name)
  (let* ((v (make-gvec <binding-envt> '() #f))
	 (gf (make <slow-generic-function>
		   template: slow-generic-function-dispatch
		   environment: v
		   generic-function-methods: '()
		   function-specializers: <object>
		   generic-function-name: name)))
    (gvec-set! v 1 gf)
    gf))

(define (add-method (gf <generic-function>)
		    (method <method>))
  ;;
  (let ((methods (generic-function-methods gf)))
    ;; 
    ;; examine the function's specializers to see where to add it
    ;;
    (let-syntax ((insert-before (syntax-form (prev rest)
				  (if prev
				      (set-cdr! prev (cons method rest))
				      (set-generic-function-methods! gf
						       (cons method rest)))
				  method))
		 (method-key-class (syntax-form (m)
				      (car (function-specializers m)))))
      (let loop ((i methods)
		 (prev #f))
	(if (null? i)
	    (insert-before prev '())
	    (let ((cmp (compare-specializer (function-specializers method)
					    (function-specializers (car i)))))
	      (if (eq? cmp '=)
		  ; this is a REPLACEMENT of an existing method
		  (set-car! i method)
		  (if (eq? cmp '<)
		      (insert-before prev i)
		      (loop (cdr i) i)))))))))

(define (make-method spec fn) 
  (make <method> 
	template: (template fn) 
	environment: (environment fn) 
	function-specializers: spec))

(define binary++ (make-slow-gf 'binary++))
(add-method binary++ 
	    (make-method (list <fixnum> <fixnum>)
			 (lambda ((a <fixnum>) (b <fixnum>))
			   (fixnum+ a b))))
(add-method binary++
	    (make-method (list <double-float> <double-float>)
			 (lambda ((a <double-float>) (b <double-float>))
			   (+ a b))))

(add-method binary++
	    (make-method (list <string> <string>)
			 (lambda (a b)
			   (string-append a b))))

(add-method binary++
	    (make-method (list <string> <number>)
			 (lambda (a b)
			   (string-append a (number->string b)))))

(add-method binary++
	    (make-method (list <number> <string>)
			 (lambda (a b)
			   (string-append (number->string a) b))))

(define-syntax x++
  (syntax-form ()
    0)
  (syntax-form (a)
    a)
  (syntax-form (a b . more)
    (binary++ a (x++ b . more))))
