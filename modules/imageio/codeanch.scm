#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/imageio/codeanch.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.2
 | File mod date:    1997-11-29 23:10:30
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  imageio
 |
 | Purpose:          Construct <code-ptr-anchor> and <fn-descr-anchor>'s
 `------------------------------------------------------------------------|#

(define (get-fn-descr-anchor anchor-table (fnd <fixnum>))
  (or (table-lookup anchor-table fnd)
      (bind ((fn-name monotones part (get-c-function-descr fnd))
	     (part-name part-num fd-list module (get-c-part-descr part))
	     (module-name part-list num-roots (get-c-module-descr module)))
	(let ((a (make <fn-descr-anchor>
		       module-name: module-name
		       part-number: part-num
		       function-number: (indexq fnd fd-list))))
	  (table-insert! anchor-table fnd a)
	  a))))

(define (get-code-ptr-anchor anchor-table (fnd <fixnum>) code-ptr)
  (or (table-lookup anchor-table code-ptr)
      (bind ((fn-name monotones part (get-c-function-descr fnd)))
	(let ((a (make <code-ptr-anchor>
		       fn-descr: (get-fn-descr-anchor anchor-table fnd)
		       monotone-number: (indexq code-ptr monotones))))
	  (table-insert! anchor-table code-ptr a)
	  a))))

(define (indexq item lst)
  (let loop (((i <fixnum>) 0)
	     (l lst))
    (if (pair? l)
	(if (eq? item (car l))
	    i
	    (loop (add1 i) (cdr l)))
	#f)))

