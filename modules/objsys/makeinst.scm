#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/objsys/makeinst.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.19
 | File mod date:    1998-05-24 19:41:34
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  objsys
 |
 | Purpose:          support for creating instances using "make"
 `------------------------------------------------------------------------|#

;;

(define-class <initializer-error> (<condition>)
  target-class
  slot-descriptor)

(define-class <initializer-type-error> (<initializer-error>)
  given-value)

(define-class <initializer-missing> (<initializer-error>))

(define-method display-object ((self <initializer-missing>) port)
  (__format port "make ~s: missing initializer for slot ~a\n"
	    (class-name (target-class self))
	    (name (slot-descriptor self))))

(define-method display-object ((self <initializer-type-error>) port)
  (__format port "make ~s: incorrect type of initial value for ~a~a\n"
	    (class-name (target-class self))
	    (if (slot-descriptor self)
		"slot "
		"allocation area")
	    (if (slot-descriptor self)
		(name (slot-descriptor self))
		""))
  (__format port ">> given: ~#*@60s\n" (given-value self))
  (__format port ">> required type: ~a\n"
	    (if (slot-descriptor self)
		(type-restriction (slot-descriptor self))
		<allocation-area>)))


;;;

(%early-once-only
 (define $alloc-area-kwd (symbol->keyword '%alloc-area)))

(define (get-allocation-area c v)
  (using-keyword-value 
   $alloc-area-kwd
   v
   (lambda (a)
     (if (instance? a <allocation-area>)
	 a
	 (signal
	  (make <initializer-type-error>
		target-class: c
		slot-descriptor: #f
		given-value: a))))
   (lambda ()
     *default-allocation-area*)))

;;

(%strategy ccode
(define-method default-slot-value ((self <slot-descriptor>)
				   (target <object>)
				   (inits <vector>))
  (case (initialization-mode self)
    ((optional prohibited)
     (init-value self))
    ((function)
     ((init-value self)))
    ((required)
     (signal
      (make <initializer-missing>
	    target-class: (object-class target)
	    slot-descriptor: self)))))

(define-method initialize-slot! ((self <slot-descriptor>)
				 (target <object>)
				 (inits <vector>))
  (if (init-keyword self)
      (using-keyword-value
       (init-keyword self)
       inits
       (lambda (val)
	 (if (instance? val (type-restriction self))
	     (begin
	       (gvec-set! target (index self) val)
	       (values))
	     (signal
	      (make <initializer-type-error>
			 target-class: (object-class target)
			 slot-descriptor: self
			 given-value: val))))
       (lambda ()
	 (begin
	   (gvec-set! target 
		      (index self)
		      (default-slot-value self target inits))
	   (values))))
      (begin
	(gvec-set! target 
		   (index self)
		   (default-slot-value self target inits))
	(values))))

(define finish-initialization
  (lambda 'finish-initialization (instance more-inits)
    (if (null? more-inits)
	(initialize instance)
	(apply* instance more-inits initialize))
    ;; return the instance independent of what `initialize'
    ;; returns -- per Dylan spec [DIRM 94 p.80]
    instance))

;; 
;;  make-instance
;;
;;  support for creating of instances
;;  for which the class is known only at compile time
;;

(define (make-instance (class <<class>>) . inits)
  (if (not (or (eq? (heap-type class) 0)
	       (eq? (heap-type class) 4)))
      (if (eq? (heap-type class) 3)
	  (error "class ~s is abstract; instantiation is not permitted" class)
	  (error "cannot instantiate class ~s; heap type = ~d"
		 class (heap-type class))))
  (let* ((slots (class-compute-slots class))
	 ((v <vector>) (keyword-value-list->vector inits))
	 (instance (gvec-alloc-in-area (get-allocation-area class v)
				       class
				       (class-instance-size class)
				       #f)))
    ;;
    ;; fill in the initial values
    ;;
    (let loop ((slots slots))
      (if (null? slots)
	  ;; done processing all slots; finish the initialization
	  ;; (which involves invoking the `initialize' procedure)
	  (finish-initialization instance (remainder->list v))
	  (begin
	    (initialize-slot! (car slots) instance v)
	    (loop (cdr slots)))))))
)

(define (set-finish-initialization-proc! (proc <function>))
  (set! finish-initialization proc))

(define (slot-index slot)
  (gvec-ref slot 4))

(define (slot-init-kwd slot)
  (gvec-ref slot 5))
