#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/hacks/repl.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.2
 | File mod date:    1997-11-29 23:10:30
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  hacks
 |
 | Purpose:          REPL/Module interaction support
 `------------------------------------------------------------------------|#

(define-generic-function repl)

(define-method env-and-name ((self <symbol>))
  (values (top-level-envt (get-module self)) self))

(define-method env-and-name ((self <top-level-contour>))
  (if (owner self)
      (env-and-name (owner self))
      (values self 'top)))

(define-method env-and-name ((self <module>))
  (let ((n (assq self (map (lambda (p)
			     (cons (cdr p) (car p)))
			   (installed-modules)))))
    (values (top-level-envt self) 
	    (if n (cdr n) 'top))))

;;;

(define (repl-in env n)
  (if (and (= (length n) 1)
	   (symbol? (car n)))
      (repl (car n))
      (format #t "`,~s' is invalid; expected a single module name\n" 
	      (cons 'go n))))

(%early-once-only
 (define-command-proc (in)
   repl-in
   ((",(in n)" "start repl in module named `n'"))))

;;;

(define-method repl ((self <object>))
  (bind ((e n (env-and-name self)))
    (cmd-loop e (format #f "~a[~~d]=>" n))))
