#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/toplevel/builtin.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.3
 | File mod date:    1998-02-27 14:27:29
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#

(define (builtin-modules)
  (list (cons '*scheme*
	      (let* ((envt (make-top-level-contour)))
		(for-each (lambda (bdg)
			    (table-insert! (table envt)
					   (name bdg)
					   bdg))
			  (append 
			   (make-special-forms)
			   (make-top-level-forms)
			   (make-objsys-forms)))
		(make <module>
		      name: '*scheme*
		      top-level-envt: envt
		      module-exports: (table envt))))))

