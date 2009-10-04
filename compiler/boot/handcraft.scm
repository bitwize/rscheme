#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/boot/handcraft.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.2
 | File mod date:    1997-11-29 23:10:26
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#


;;
;;   hand-crafted object system for boot images
;;

    
(define (craft-classes tbl node parent)
  (let* ((name (car node))
	 (bdg (table-lookup tbl name)))
    (if (not bdg)
	(begin
	  (set! bdg (make <top-level-var>
			  name: name
			  value: (craft-class node parent)
			  write-prot: #t))
	  (table-insert! tbl name bdg)))
    (for-each (lambda (sub)
		(craft-classes tbl sub (value bdg)))
	      (let ((r (cddr node)))
		(if (and (pair? r)
			 (eq? (car r) 'image-mode:))
		    (cddr r)
		    r)))))

(define (craft-class node parent)
  (make-gvec
   <<class>>
	 #|name|# (car node)
	 #|heap-type|# (case (cadr node)
			  ((:gvec) 0)
			  ((:bvec) 1)
			  ((:immob) 2)
			  ((:abstract) 3)
			  (else (abort 'craft-classes "bad heap type: ~s" 
				       (cadr node))))
         #|image-mode|# (if (and (pair? (cddr node))
				 (eq? (caddr node) 'image-mode:))
			    (cadddr node)
			    (if (eq? (cadr node) ':bvec) 1 0))
	 #|super-classes|# (if parent
			       (list parent)
			       '())
	 #|slots|# '()
	 #f #f #f #f #f
	 #f #f #f #f #f
	 #f #f #f #f #f))
