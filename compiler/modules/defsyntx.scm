#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/modules/defsyntx.scm
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


(define (compile-tl-define-syntax tl-def tl-envt dyn-envt)
  (let ((lhs (cadr tl-def)))
    (if (pair? lhs)
	(begin
	  (if *tl-report*
	      (format #t "compiling top-level syntax ~s ~s\n"
		      (car lhs)
		      (cdr lhs)))
	  (submit-new-syntax 
	   (make <macro>
		 name: (car lhs) 
		 envt: tl-envt
		 else-bdg: #f
		 forms: (list (make <macro-form>
				    args: (cdr lhs)
				    body: (cddr tl-def))))
	   tl-envt))
	(begin
	  (if *tl-report*
	      (format #t "compiling top-level syntax ~s ...\n"
		      lhs))
	  (submit-new-syntax (compile-macro lhs (cddr tl-def) tl-envt)
			     tl-envt)))))

(define (submit-new-syntax (m <macro>) envt)
  (let ((bdg (lookup (the-top-level envt) (name m))))
    (if bdg
	(error/semantic "define-syntax: `~s' is already bound" (name m))
	(bind! (the-top-level envt) m))
    #f)) ;; #f => no initialization code


;;--- crummy macros...

(define (compile-tl-define-rewriter tl-def tl-envt dyn-envt)
  (let ((name (caadr tl-def))
	(args (cdadr tl-def))
	(body (cddr tl-def)))
    (ensure-new-tlb name
		    tl-envt
		    (make <rewriter>
			  name: name
			  rewriter-body: body
			  rewriter-args: args))
    #f)) ;; #f => no initialization code

(define-method compile-head ((self <rewriter>) 
			     orig form
			     lex-envt dyn-envt mode)
  (let ((proc (eval `(lambda ,(rewriter-args self) ,@(rewriter-body self)))))
    (compile (proc form) lex-envt dyn-envt mode)))
