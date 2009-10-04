#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/codegen/rearrnge.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.2
 | File mod date:    1997-11-29 23:10:33
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  codegen
 |
 | Purpose:          Much improved but still fairly simply `gen-lazy-args' facility
 `------------------------------------------------------------------------|#

;;;  returns a sequence of AML statements to be executed in order
;;;  plus a sequence of length (min+(if(rest?,1,0))) of AML expressions
;;;  which commute with each other,
;;;  plus the new set of `in-regs'

(define (gen-rearranged-aml (ic-exprs <list>) ct-envt in-regs min rest?)
  (if rest?
      (let ((n (length ic-exprs)))
	(if (<= n min)
	    (bind ((s e ir (gen-rearranged-aml ic-exprs ct-envt in-regs min #f)))
	      (values s (append e (list $nil)) ir))
	    (bind ((s e ir (gen-rearranged-aml ic-exprs ct-envt in-regs n #f))
		   (h t (list-brk e min)))
	      (values s (append h (list (make-list-expr t))) ir))))
      (bind ((temp1 (length in-regs))
	     (non-triv effects result (multi-bind-fixed temp1 min ic-exprs)))
	(let loop ((preamble non-triv)
		   (in-regs in-regs)
		   (reg-num temp1)
		   (r '())
		   (temp-types '()))
	  (if (pair? preamble)
	      (let* ((se (gen-aml (car preamble) ct-envt in-regs 'value))
		     (seo (obj-expr (se-expr se))))
		(loop (cdr preamble)
		      (cons #f in-regs)
		      (+ reg-num 1)
		      (cons* `((set! (reg ,reg-num #f) ,seo))
			     (se-stmt se)
			     r)
		      (cons (cons reg-num (car seo)) temp-types)))
	      (values (apply 
		       append
		       (apply append (reverse r))
		       (map (lambda (e)
			      (gen-aml e ct-envt in-regs 'effect))
			    effects))
		      (map (lambda (r)
			     (if (fixnum? r)
				 (let ((t (cdr (assq r temp-types))))
				   `(,t ref (reg ,r #f)))
				 (let ((se (gen-aml r ct-envt in-regs 'value)))
				   (assert (null? (se-stmt se)))
				   (se-expr se))))
			   result)
		      in-regs))))))
		   

;;;
;;;   do some analysis of the given expressions which are destined
;;;   to be bound to a set of variables (with no #rest variable)
;;;
;;;   INPUT:
;;;      first-temp-reg <fixnum>          first register available as temp
;;;      num-vars       <fixnum>          number of output variables
;;;      exprs          (list <icode>)    icode to be bound
;;;
;;;   OUTPUT:
;;;      non-triv   (list (pair <fixnum> <icode>))
;;;                                   register setups for some subset of exprs
;;;      effects    (list <icode>)    subset of exprs to compile for effect
;;;      result     (list {<icode>|<fixnum>})
;;;                                   final result exprs (all simple)
;;;
;;;  the result may contain icode for things that are `simple?' but
;;;  `trivial?' if effects contains only `simple?' things and there is
;;;  some tail of non-triv with only `simple?' things

(define (multi-bind-fixed (first-temp-reg <fixnum>)
			  (num-vars <fixnum>)
			  (exprs <list>))
  (bind ((for-val for-eff (pad-or-split-exprs exprs num-vars)))
    (let loop ((src for-val)
	       (simplicity (map simple? exprs))
	       (non-trivial-r '())
	       (result-r '())
	       (next-reg first-temp-reg))
      (if (null? src)
	  (values (reverse non-trivial-r)
		  for-eff
		  (reverse result-r))
	  (let ((ic (car src))
		(simplicity-of-rest (if (pair? simplicity)
					(cdr simplicity)
					'())))
	    (if (or (trivial? ic)
		    (every? identity simplicity))
		(loop (cdr src)
		      simplicity-of-rest
		      non-trivial-r
		      (cons ic result-r)
		      next-reg)
		(loop (cdr src)
		      simplicity-of-rest
		      (cons ic non-trivial-r)
		      (cons next-reg result-r)
		      (+ 1 next-reg))))))))

;;;
;;;  help out `multi-bind-fixed' by padding or splitting the list of
;;;  expressions according to the number of desired quantities
;;;
;;;  INPUT:
;;;     exprs      (list <icode>)
;;;     num-vars   <fixnum>
;;;  OUTPUT:
;;;     for-value  (list <icode>)    may be padded w/ic-const(#f)'s
;;;     for-effect (list <icode>)

(define (pad-or-split-exprs (exprs <list>) (num-vars <fixnum>))
  (let ((n (length exprs)))
    (cond
     ;; common case... the middle bear (not too many, not too few)
     ((eq? n num-vars)
      (values exprs '()))
     ;; underflow... pad with #f
     ((< n num-vars)
      (values (append exprs
		      (map (let ((f (make-const #f 'value)))
			     (lambda (i) f))
			   (range (- num-vars n))))
	      '()))
     ;; overflow... rest are for effect
     (else
      (values (reverse (list-tail (reverse exprs) (- n num-vars)))
	      (list-tail exprs num-vars))))))

(define (list-brk (src <list>) (brk <fixnum>))
  (let ((n (length src)))
    (cond
     ((eq? n brk)
      (values src '()))
     ((> brk n)
      (error "list-brk: break point ~d > list length ~d" brk n))
     (else
      (values (reverse (list-tail (reverse src) (- n brk)))
	      (list-tail src brk))))))

;;;  `trivial?' is a subcase of `simple?' which detects code that can
;;;  be arbitrarily reordered.  Note that this is even stronger than
;;;  `pure?', because a pure function can depend on the state of it's
;;;  argument which might change.
;;;
;;;  The complete hierarchy should be:
;;;      [A arbitrary
;;;        [F side-effect-free
;;;          [P pure
;;;             [T trivial]]]]
;;;
;;;  (a function that accesses a global variable is in [F]-[P])
;;;
;;;  The current code generator makes no use of [P]-[T], and only
;;;  aproximates [F]-[P] (the `simple?' predicate)

(define (trivial? (self <icode>))
  ;; the only trivial things we recognize (perhaps *are*) are constants
  (instance? self <ic-const>))
