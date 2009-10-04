#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/kwdlist.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    1997-11-29 23:10:35
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 | Purpose:          compile keyword lists
 |------------------------------------------------------------------------|
 | Notes:
 |      *** This file is included by the offline compiler ***
 `------------------------------------------------------------------------|#

;; returns a list of <icode> for		foo: bar baz: blech
;; that is the same as would result from
;; 						'foo: bar 'baz: blech
;;
;; (in this version, subst expansions done, because
;;  we might as well -- compile-make is called as from a macro)
;;
;; this function is currently only used for parsing the
;; arguments to `make' after the class expr
;;

(define (compile-keyword-list kw-list lex-envt dyn-envt)
  (cond
   ((null? kw-list)
    '())
   ((and (pair? kw-list)
	 (or (keyword? (car kw-list))
	     (unique-obj? (car kw-list)))
	 (pair? (cdr kw-list)))
    (cons (make <ic-const> 
		value: (car kw-list))
	  (cons (compile (cadr kw-list) lex-envt dyn-envt 'value)
		(compile-keyword-list (cddr kw-list)
				      lex-envt
				      dyn-envt))))
   ((symbol? kw-list)
    ;; it is something like (foo: bar baz: quux . more)
    (let ((bdg (lookup-aliased kw-list lex-envt dyn-envt)))
      (if (substitution? bdg)
	  (compile-keyword-list (expr bdg) (envt bdg) dyn-envt)
	  (error/syntax "at `~s', not a substitution"
			kw-list))))
   (else					      
    (error/syntax "at `~s', keyword/value list ill formed"
		  kw-list))))

;;
;;  for a list of the form
;;    (keyword: value keyword: value ...)
;;  calls the given procedure (proc) for each kwd/val pair

(define (for-each-keyword proc kv-list)
  (let loop ((i kv-list))
    (if (pair? i)
	(if (keyword? (car i))
	    (if (pair? (cdr i))
		(if (proc (car i) (cadr i))
		    (loop (cddr i))
		    (error/syntax "keyword not recognized: ~s" (car i)))
		(error/syntax "missing value after: ~s" (car i)))
	    (if (flag? (car i))
		(if (proc (car i) #t)
		    (loop (cdr i))
		    (error/syntax "flag not recognized: ~s" (car i)))
		(error/syntax "not a keyword or flag: ~s" (car i))))
	(if (not (null? i))
	    (error/syntax "keyword list malformed")))))
