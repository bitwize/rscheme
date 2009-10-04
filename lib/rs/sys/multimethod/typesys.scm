;;;
;;;   1. Type System
;;;

(define-class <type> (<object>) :abstract)

(define-class <singleton> (<type>)
  singleton-value)

(define (singleton? x)
  (instance? x <singleton>))

(define (make-singleton val)
  (make <singleton>
	singleton-value: val))

(define-macro (singleton val)
  (list 'quote (make-singleton val)))

(define (specializer-includes? spec datum)
  (if (instance? spec <singleton>)
      (eq? (singleton-value spec) datum)
      (subclass? (object-class datum) spec)))

(define (type=? t1 t2)
  (cond
   ((and (class? t1) (class? t2))
    (eq? t1 t2))
   ((and (instance? t1 <singleton>) (instance? t2 <singleton>))
    (eq? (singleton-value t1) (singleton-value t2)))
   (else
    #f)))

(define (type<=? t1 t2)
  (cond
   ((and (class? t1) (class? t2))
    (subclass? t1 t2))
   ((and (singleton? t1) (singleton? t2))
    (eq? (singleton-value t1) (singleton-value t2)))
   ((and (singleton? t1) (class? t2))
    (instance? (singleton-value t1) t2))
   (else
    #f)))

(define (method-accepts? (m <method>) (args <list>))
  (let loop ((spec (function-specializers m))
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

