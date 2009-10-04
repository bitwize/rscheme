#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/envtrefl.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.4
 | File mod date:    1997-11-29 23:10:35
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose:          Reflection on binding environments
 `------------------------------------------------------------------------|#

;;(file-version-string "%Z%%W% 1.4 %G% 23:10:35")

;;
;;  in the current implementation, the captured-envt-info code 
;;  property is a list of representations of environment frames,
;;  flattened (not the original <lexical-var> objects) to keep
;;  the source compiler from being sucked into the system image.
;;
;;  example:
;;    (let ((x 10)
;;          (y 3))
;;      (let ((z 5))
;;        (lambda ()
;;           (+ x y z))))
;;
;;  creates a procedure with the following element in the
;;  function-descr list:
;;     (captured-envt-info 
;;        ((z <obj>)) 
;;        ((x <obj>) (y <obj>)))
;;  note that the type is stored along with the name.  The type
;;  can be either a symbol (but only the <obj> in practice, I
;;  think, representing the <object> class) or an actual class.
;;
;;  a later version of this library will use the type information
;;  to prevent violating the type system, and besides, the information's
;;  there.
;;
;;  NOTE:  "free-variable" in this context refers to the scope
;;  before the lambda is entered.  The arguments to the lambda don't
;;  hide anything as far as these functions are concerned.
;;
;;  NOTE:  not every function will have this code property.  In particular,
;;  generic functions and those created specially (e.g., by
;;  call-with-current-continuation or unwind-protect, etc.) will likely
;;  not have this code-property.  introspect-on-free-variables signals
;;  an error if called with a function (or template) without the
;;  captured-envt-info code property.

;;
;; returns a flattened alist
;; mapping names to (frame . slot) pairs
;;

(define-method introspect-on-free-variables ((self <template>))
  (let ((b (assq 'captured-envt-info (function-descr self))))
    (if b
	;; flatten it
	(let ((r '()))
	  (let loop ((frame-num 0)
		     (p (cdr b)))
	    (if (null? p)
		(reverse r)
		(begin
		  (for-each (lambda (v slot-num)
			      (set! r (cons (cons (car v)
						  (cons frame-num slot-num))
					    r)))
			    (car p)
			    (range (length (car p))))
		  (loop (+ frame-num 1)
			(cdr p))))))
	(error "~s: sorry, not compiled with captured envt info" self))))

(define-method free-variable->lex-addr ((self <template>) (name <symbol>))
  (let ((b (assq name (introspect-on-free-variables self))))
    (if b
	(cdr b)
	#f)))

(define-method introspect-on-free-variables ((self <closure>))
  (introspect-on-free-variables (template self)))

(define-method free-variable->lex-addr ((self <closure>) (name <symbol>))
  (free-variable->lex-addr (template self) name))

(define-method reflect-on-captured-value ((self <closure>)
					  (lex-addr <pair>)
					  proc)
   (let loop ((frame (environment self))
	     (i (car lex-addr)))
    (if (eq? i 0)
	;; add 1 because slot[0] is used for scope link
	(proc frame (+ (cdr lex-addr) 1))
	(loop (enclosing frame)
	      (- i 1)))))

(define-method introspect-on-captured-value ((self <closure>)
					     (lex-addr <pair>))
  (reflect-on-captured-value self 
			     lex-addr 
			     (lambda (gvec slot)
			       (gvec-ref gvec slot))))


(define-method set-captured-value! ((self <closure>)
				    (lex-addr <pair>)
				    value)
  (reflect-on-captured-value self
			     lex-addr
			     (lambda (gvec slot)
			       (gvec-set! gvec slot value))))

#|
some test functions...

  (define p (let ((x 10)) (lambda () x)))
  (define q (let ((a 3) (b 5)) (let ((b 7)) (lambda (a) b))))

  (free-variable->lex-addr p 'x)  ; ==> (0 . 0)
  (free-variable->lex-addr q 'a)  ; ==> (1 . 0)
  (free-variable->lex-addr q 'b)  ; ==> (0 . 0)

|#
