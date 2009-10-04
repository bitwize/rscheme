#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/repl/parsslot.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    2003-12-15 09:25:50
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  repl
 |
 `------------------------------------------------------------------------|#

(define (target-procedure? x)
  (instance? x <target-function>))

;;;
;;;  note: this code is also used by the cross compiler `rsc'
;;;

(define *slots-sealed-by-default?* #f)

(define (parse-slot-descriptor supers name options envt d-envt)
  (let ((init-mode 'required)
	(init-val #f)
	(type #f)
	(getter #f)
	(setter #f)
	(alloc #f)
	(init-kwd #f)
        (metaclass <slot-descriptor>)
        (metainits '())
	(sealed? *slots-sealed-by-default?*))
    ;;
    (for-each-keyword
     (lambda (keyword value)
       (case keyword
	 ((type:)
          (set! type (parse-type-expr value envt d-envt)))
	 ((getter:) 
          (set! getter value))
	 ((setter:) 
          (set! setter value))
         ((metaclass:)
          (set! metaclass (parse-const-expr value envt d-envt)))
	 ((allocation:) 
          (if (memq value '(constant internal))
              (set! alloc value)
              (error/syntax 
               "`allocation: ~s' is not supported" value)))
	 ((init-value:) 
          (if (eq? init-mode 'required)
              (set! init-mode 'optional))
          (set! init-val (parse-const-expr value envt d-envt)))
	 ((init-function:) 
	  (if (eq? init-mode 'required)
	      (set! init-mode 'function))
	  ;; this expression evaluates to a function
	  (let ((proc (target-expr-value value envt d-envt)))
	    (set! init-val proc)
	    (if (not (target-procedure? (actual-value proc)))
		(error "init-function: expr `~s' doesn't\nevaluate to a procedure (is a ~s, ~#*@30s)"
		       value
		       (name (object-class (actual-value proc)))
		       (actual-value proc)))))
	 ((init-keyword:)
          (if value
              (if (keyword? value)
                  (set! init-kwd value)
                  (error/syntax 
                   "init-keyword not a keyword: ~s"
                   value))
              (set! init-mode 'prohibited)))
	 ((:sealed) 
          (set! sealed? #t))
	 ((:open)
          (set! sealed? #f))
	 (else 
          (set! metainits (cons* (parse-const-expr value envt d-envt)
                                 keyword
                                 metainits))))
       #t)
     options)
    ;;
    (if setter
	(if alloc
	    (error/syntax 
	     "`allocation: ~s' is inconsistent with `setter: ~s'"
	     alloc
	     setter))
	(if (not alloc)
	    (set! setter
		  (string->symbol (string-append 
				   "set-" 
				   (symbol->string name)
				   "!")))))
    ;;
    (if (not getter)
	(set! getter name))
    ;;
    (if (eq? alloc 'internal)
	(begin
	  (set! setter #f)
	  (set! getter #f))
	(if (eq? alloc 'constant)
	    (set! setter #f)))
    ;;
    (apply make-instance
           metaclass
           name: name
           initialization-mode: init-mode
           init-value: init-val
           type-restriction: (or type
                                 (xform (well-known '<object>)
                                        'value))
           index: 0
           getter: getter
           setter: setter 
           properties: (if sealed?
                           '((sealed? . #t))
                           '())
           init-keyword: (or init-kwd (symbol->keyword name))
           (reverse metainits))))
