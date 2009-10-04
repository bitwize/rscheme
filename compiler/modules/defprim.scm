#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/modules/defprim.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.4
 | File mod date:    1997-11-29 23:10:28
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#


;;
;;  define a primop
;; 

#|
    (define-primop (car <pair>) => <obj>
      (ccode "pair_car")
      (bytecode 0 12))

    (define-primop (%make <<class>> . <obj>) => <obj>
      (bytecode 'make)
      (ccode make))

    (define-primop (cons <obj> <obj>) => <obj>
      (bytecode 'cons)
      (ccode "cons"))
|#

(define (compile-primop-morphose prim-name envt d-envt morph)
  (cons 
   (car morph)
   (case (car morph)
     ((ccode)
      (if (not (or (symbol? (cadr morph))
		   (string? (cadr morph))))
	  (error/syntax "~s: Illegal form of c morphose: ~s" 
			prim-name
			morph)
	  (cadr morph)))
     ((bytecode)
      (let ((args (map
		   (lambda (x)
		     (let ((v (parse-const-expr x envt d-envt)))
		       (if (or (integer? v)
			       (symbol? v))
			   v
			   (error/syntax 
			    "~s: Illegal bytecode component expr: ~s (=~s)"
			    x 
			    v))))
		   (cdr morph))))
	 (if (not (memq (length args) '(1 2)))
	     (error/syntax "~s: Illegal form for bytecode morphose: ~s"
			   prim-name
			   morph)
	     (case (length args)
	       ((1)
		(car args))
	       ((2)
		;;
		;;  extension # specified...
		;;
		(cons (car args) (cadr args)))))))
     (else
      (warning "~s: Unrecognized morphose: ~s" prim-name morph)))))

(define (parse-primop-args-form form)
  (if (pair? form)
      (bind ((t r (parse-primop-args-form (cdr form))))
	(values (cons (car form) t) r))
      (if (symbol? form)
	  (values '() form)
	  (values '() #f))))

(define (compile-tl-define-primop tl-def tl-envt dyn-envt)
  (bind ((prim-name (caadr tl-def))
	 (arg-types rest-type (parse-primop-args-form (cdadr tl-def))))
    (ensure-new-tlb 
     prim-name
     tl-envt
     (make <primop>
	   name: prim-name
	   arg-types: arg-types
	   rest-type: rest-type
	   result-type: (if (eq? (caddr tl-def) '=>)
			    (cadddr tl-def)
			    #f)
	   full-procedure-bdg: #f
	   translations: (map (lambda (morph)
				(compile-primop-morphose prim-name 
							 tl-envt
							 dyn-envt
							 morph))
			      (if (eq? (caddr tl-def) '=>)
				  (cddddr tl-def)
				  (cddr tl-def)))))
    #f)) ;; #f => no initialization code
  
