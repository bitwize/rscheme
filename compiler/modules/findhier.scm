#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/modules/findhier.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.2
 | File mod date:    1997-11-29 23:10:28
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#

(define (module-name-components->rel-paths (lst <list>))
  (let ((d (make <directory-name>
		 rooted-at: #f
		 steps: lst))
	(d2 (make <directory-name>
		  rooted-at: #f
		  steps: (reverse! (cdr (reverse lst))))))
    (list (cons (make <file-name>
		      file-directory: d
		      filename: "module"
		      extension: "mif")
		'mif)
	  (cons (make <file-name>
		      file-directory: d2
		      extension: "mif"
		      filename: (last lst))
		'mif))))

(define (module-name->rel-paths (name <symbol>))
  (let ((lst (string-split (symbol->string name) #\.)))
    ;; only consider module names with at least one `.' and
    ;; with no empty components
    (if (and (> (length lst) 1)
	     (every? (lambda ((p <string>))
		       (not (eq? (string-length p) 0)))
		     lst))
	(module-name-components->rel-paths lst)
	#f)))

(define (find-m-by-style (search <list>) (in <directory-name>))
  (let loop ((s search))
    (if (null? s)
	(values)
	(let* ((p (append-path in (caar s)))
	       (f (pathname->os-path p)))
	  (if (os-file-exists? f)
	      (values f p (cdar s))
	      (loop (cdr s)))))))

(define (hier-find-module name)
  (let ((rels (module-name->rel-paths name)))
    (if rels
	(let loop ((p *library-path*))
	  (if (null? p)
	      #f
	      (bind ((sfn fn typ (find-m-by-style rels (car p))))
		(if sfn
		    fn
		    (loop (cdr p))))))
	#f)))
