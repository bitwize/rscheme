#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/aliases.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.6
 | File mod date:    2001-06-23 19:21:37
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 `------------------------------------------------------------------------|#

(define-class <alias-contour> (<scope-record>)
  dynamic-enclosing
  name
  in-lex-envt
  aliased-var)

;; an semi-evil hack to make an <alias-contour>
;; act in some respects like its corresponding <lexical-contour>
;; (not so evil because, after all, it really is part of the same thing...)

(define-method bindings ((self <alias-contour>))
  (bindings (dynamic-enclosing self)))

(define-method name->bindings ((self <alias-contour>))
  (name->bindings (dynamic-enclosing self)))

(define-method lexical-enclosing ((self <alias-contour>))
  (lexical-enclosing (dynamic-enclosing self)))

(define-method lookup ((self <alias-contour>) name)
  ;; in fact, the dynamic-enclosing is always the same as the
  ;; lexical enclosing would be, so just look up there...
  (lookup (dynamic-enclosing self) name))

(define (find-subst-site lex-envt)
  (let loop ((e lex-envt))
    (if (instance? e <substitution-site-record>)
	e
	(if (instance? e <top-level-contour>)
	    #f
	    (loop (lexical-enclosing e))))))

(define-thread-var *lookup-dyn-envt* #f)

(define-method lookup ((self <substitution-site-record>) var-name)
  ;; look for an alias
  ;(format #t "key envt for ~s: ~s\n" var-name self)
  (let ((d *lookup-dyn-envt*))
    (if (not d)
	(begin
	  (error "caller not using `lookup-aliased' interface!\n")
	  (lookup (lexical-enclosing self) var-name))
	(if (eq? d 'no-alias-search)
	    (lookup (lexical-enclosing self) var-name)
	    (let loop ((p d))
	      ;(format #t "   (checking: ~s)\n" p)
	      (if (eq? p self)
		  (lookup (lexical-enclosing self) var-name)
		  (if (and (instance? p <alias-contour>)
			   (eq? (in-lex-envt p) self)
			   (eq? (name p) var-name))
		      (aliased-var p)
		      (loop (dynamic-enclosing p)))))))))

(define (lookup-no-alias (var-name <symbol>)
			 (lex-envt <scope-record>))
  (thread-let ((*lookup-dyn-envt* 'no-alias-search))
    (lookup lex-envt var-name)))

(define (lookup-aliased (var-name <symbol>)
			(lex-envt <scope-record>)
			(dyn-envt <scope-record>))
  (thread-let ((*lookup-dyn-envt* dyn-envt))
    (lookup lex-envt var-name)))

;;
;;  returns the new (dynamic) envt
;;

(define (make-aliased-vars (name <symbol>)
			   (lex-envt <scope-record>)
			   (var <binding>)
			   (dyn-envt <scope-record>))
  ;; should this be `lookup-aliased' or a lower-level thing
  ;; that JUST looks up the lexical chain (like `lookup' used to)?
  (let ((b (lookup-no-alias name lex-envt)))
    (if (substitution? b)
	(if (not (symbol? (expr b)))
	    ;;
	    ;; note:  we signal an error instead of silently letting it slide
	    ;; (letting it slide amounting to not creating the alias
	    ;; record) because letting it slide would introduce non-local
	    ;; semantics.  It is presumed, from the lexical construction
	    ;; of the macro body, that a substitution-name in binding
	    ;; position is denoting intentional capture.  Hence, if a symbol
	    ;; is not supplied, then it is an error (macro use error)
	    ;;
	    (error/semantic "intentional capture ~s (local ~s) is not a symbol"
			    (expr b)
			    name)
	    (let ((c (make <alias-contour>
			   dynamic-enclosing: dyn-envt
			   name: (expr b)
			   in-lex-envt: (envt b)
			   aliased-var: var)))
	      (make-aliased-vars (expr b) (envt b) var c)))
	dyn-envt)))

(define (compile/symbol symbol lexical-envt dynamic-envt mode)
  (let ((b (find symbol lexical-envt dynamic-envt)))
    ;(format #t "compile/symbol: ~s => ~s\n" symbol b)
    (compile-ref b b lexical-envt dynamic-envt mode)))

;; make-lexical-envt

(define (make-lexical-envt bindings lex-enclosing dyn-enclosing)
  (let ((c (make <lexical-contour>
		 name->bindings: (map (lambda (bdg)
					(cons (name bdg) bdg))
				      bindings)
		 bindings: bindings
		 lexical-enclosing: lex-enclosing
		 dynamic-enclosing: dyn-enclosing)))
    (for-each (lambda (b)
		(if (instance? b <lexical-var>)
		    (set-envt! b c)))
	      bindings)
    (let loop ((c c)
	       (v bindings))
      (if (null? v)
	  c
	  (loop (make-aliased-vars (name (car v))
				   lex-enclosing
				   (car v)
				   c)
		(cdr v))))))

;;
#|
;;;;; test code...

(define *bad-contours* '())

(define (validate-chain lex-envt dyn-envt)
  ;;
  ;; make sure the lex envt is contained in the dyn envt
  ;;
  (let ((chain (dynamic-enclosing-chain dyn-envt)))
    (if (not (memq lex-envt chain))
	(begin
	  (set! *bad-contours* (cons (cons lex-envt dyn-envt) *bad-contours*))
	  (format #t "BAD CONTOUR\n")
	  (print lex-envt)
	  (format #t "-- chain:\n")
	  (print chain)))))

(define *scope-record-tbl* (make-object-table))

(define (number->scope-record n)
  (table-lookup *scope-record-tbl* n))

(define (scope-record-number self)
  (or (table-lookup *scope-record-tbl* self)
      (let ((n (table-size *scope-record-tbl*)))
	(table-insert! *scope-record-tbl* self n)
	n)))

(define-method some-info ((self <top-level-contour>))
  '(top))

(define-method some-info ((self <lexical-contour>))
  (map car (name->bindings self)))

(define-method some-info ((self <alias-contour>))
  (list (name self) (scope-record-number (in-lex-envt self))))

(define-method write-object ((self <scope-record>) port)
  (format port "#[~s ~d ~j]" 
	  (name (object-class self))
	  (scope-record-number self)
	  (some-info self)))
|#

;;  this needs to be implemented to handle threading substitutions, etc.

(define (tl-bind! name lex-envt dyn-envt bdg)
  (bind! (the-top-level lex-envt) bdg))

