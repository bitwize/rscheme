#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/envts.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.10
 | File mod date:    1998-05-23 20:24:54
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 | Purpose:          environment frame (compile-time) operations
 `------------------------------------------------------------------------|#

(define-method initialize ((self <top-level-contour>))
  (if (not (table self))
      (set-table! self (make-table eq? symbol->hash))))

(define (dynamic-enclosing-chain p)
  (if (instance? p <top-level-contour>)
      (list p)
      (cons p (dynamic-enclosing-chain (dynamic-enclosing p)))))

(define (lexical-enclosing-chain p)
  (if (instance? p <top-level-contour>)
      (list p)
      (cons p (lexical-enclosing-chain (lexical-enclosing p)))))

(define (make-top-level-contour)
    (make <top-level-contour>))

(define (copy-top-level-contour (src <top-level-contour>))
  (make <top-level-contour>
	table: (hash-table-copy (table src))))

;; proc is a procedure of one argument (the new environment)
;; which is expected to return some bindings for the environment
;; (this interface supercedes the old "set-bindings!" kluge,
;; wherin the CALLER was responsible for breaking the construction
;; of a lex envt into two parts)

(define (make-lexrec-envt proc lex-enclosing dyn-enclosing)
    (let* ((new-envt (make <lexical-contour> 
			   name->bindings: '()
			   bindings: '()
			   lexical-enclosing: lex-enclosing
			   dynamic-enclosing: dyn-enclosing))
	   (new-bdgs (proc new-envt)))
	(set-bindings! new-envt new-bdgs)
	(set-name->bindings! new-envt (map (lambda (bdg)
						(cons (name bdg) bdg))
					   new-bdgs))
	new-envt))

(define (num-bindings envt)
    (length (bindings envt)))

(define (shallow-lookup tl-envt name)
    (table-lookup (table tl-envt) name))

(define-method lookup ((self <lexical-contour>) name)
    (let ((b (assq name (name->bindings self))))
	(if b
	    (cdr b)
	    (lookup (lexical-enclosing self) name))))

(define-method lookup ((self <top-level-contour>) (name <symbol>))
  (or (object-table-lookup (table self) (symbol->hash name) name)
      (and (backing self)
	   (lookup (backing self) name))))

(define-method bind! ((self <top-level-contour>) bdg)
  (set-dirty?! self #t)
  ;; need to do something better here...
  ;;(if (table-lookup (table self) (name bdg))
  ;;  (warning "in bind: `~s' already bound" (name bdg)))
  (table-insert! (table self) (name bdg) bdg))

(define-method bind-name! ((self <top-level-contour>) (n <symbol>) bdg)
  (set-dirty?! self #t)
  (table-insert! (table self) n bdg))

;; later on we can optimize this to look up the bind!
;; method for envt and just call it repeatedly, saving
;; a GF dispatch per item

(define (multi-bind! envt stuff)
  (if (instance? envt <top-level-contour>)
      (let ((tbl (table envt)))
	(set-dirty?! envt #t)
	(let loop ((s stuff))
	  (if (null? s)
	      (values)
	      (let* ((b (car s))
		     ((n <symbol>) (gvec-ref b 0)))
		(object-table-insert! tbl (symbol->hash n) n b)
		(loop (cdr s))))))
    (for-each (lambda (bdg)
		(bind! envt bdg))
	      stuff)))

;; a better form of bind, where the stuff is an association
;; instead of a list of (conceptually nameless) binding objects

(define (multi-bind-assoc! envt stuff)
  (if (instance? envt <top-level-contour>)
      (let ((tbl (table envt)))
	(let loop ((s stuff))
	  (set-dirty?! envt #t)
	  (if (null? s)
	      (values)
	      (bind (((a <pair>) (car s))
		     ((n <symbol>) b (values (car a) (cdr a))))
		(object-table-insert! tbl (symbol->hash n) n b)
		(loop (cdr s))))))
      (for-each (lambda (assoc)
		  (bind-name! envt (car assoc) (cdr assoc)))
		stuff)))

(define (the-top-level envt)
    (if (instance? envt <top-level-contour>)
	envt
	(the-top-level (lexical-enclosing envt))))

;;;
;;;  support module TLC backings
;;;

(define-method lookup ((self <module>) var)
  (let loop ((imports (module-imports self)))
    (if (null? imports)
	#f ;; couldn't find it
	(let ((b (lookup-from-imported-module (car imports) var self)))
	  (if b
	      (begin
		; cache the binding in our table...
		(table-insert! (table (top-level-envt self)) var b)
		b)
	      (loop (cdr imports)))))))

(define (lookup-from-imported-module (self <imported-module>)
				     (var <symbol>)
				     (in-m <module>))
  (let (((m <module>) (actual-module self)))
    (table-lookup (module-exports m) var)))
