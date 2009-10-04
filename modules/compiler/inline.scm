#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/inline.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.8
 | File mod date:    1998-08-29 19:45:08
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 `------------------------------------------------------------------------|#


(define *do-inlining* #t)


(define (slot-sealed? (s <slot-descriptor>))
  (and (assq 'sealed? (properties s)) #t))

(define (slot-was-inlined? (s <slot-descriptor>))
  (and (assq 'slot-was-inlined (properties s)) #t))

(define (set-slot-was-inlined! (s <slot-descriptor>))
  (set-properties! s (cons '(slot-was-inlined) (properties s))))

(define (inline-setter (m <target-setter>) 
		       (rcvr <expr-icode>) 
		       (val <expr-icode>))
  (let ((s1 (make <ic-call-prim>
		  function: (well-known 'gvec-set!)
		  args: (multi-checked-coerce
			 (list rcvr 
			       (make-const (index m) 'value) 
			       val)
			 (list (first-return-type rcvr)
			       '<fixnum>
			       (type-restriction m))
			 #f)
		  return-types: '()))
	(s2 (make <ic-multi>
		  arg-list: '()
		  mode: 'value)))
    (set-slot-was-inlined! (slot-descriptor m))
    (make <ic-seq>
	  stmt-list: (list s1 s2))))
		    

(define (inline-getter (m <target-getter>) 
		       (rcvr <expr-icode>))
  ;(format #t "\tgetter (slot ~d)\n" (index m))
  (set-slot-was-inlined! (slot-descriptor m))
  (make <ic-cast>
	expr: (make <ic-call-prim>
		    function: (well-known 'gvec-ref)
		    args: (make <ic-multi>
				arg-list: (list rcvr
						(make-const (index m) 
							    'value))
				mode: 'value)
		    return-types: '(<obj>))
	return-types: (list (type-restriction m))))

(define (do-inlining (fn <expr-icode>) (args <expr-icode>))
  (let ((v (actual-value (compile-time-const-value fn)))
	(argtypes (map (lambda (rt)
			 (if (symbol? rt)
			     (actual-value (prim-type->class rt))
			     (actual-value rt)))
		       (return-types args))))
    ;(format #t "ILV: ~s~s\n"  (name v) (map class-name argtypes))
    (let ((m (find-method-by-class v (car argtypes)))
	  (argc (length argtypes)))
      (cond
       ((and (instance? m <target-getter>) 
	     (slot-sealed? (slot-descriptor m))
	     (eq? argc 1))
	(inline-getter m (car (arg-list args))))
       ((and (instance? m <target-setter>) 
	     (slot-sealed? (slot-descriptor m))
	     (eq? argc 2))
	(inline-setter m (car (arg-list args)) (cadr (arg-list args))))
       (else #f)))))

(define (inlined-version (fn <expr-icode>) (args <expr-icode>))
  (and *do-inlining*
       (compile-time-const? fn)
       (instance? (actual-value (compile-time-const-value fn)) <target-gf1>)
       (pair? (return-types args)) ;; cr621
       (do-inlining fn args)))

;;;
