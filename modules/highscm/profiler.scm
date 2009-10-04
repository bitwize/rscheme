#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/highscm/profiler.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.7
 | File mod date:    2003-06-12 21:53:56
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  high-scheme
 |
 | Purpose:          high-level profiling functions
 `------------------------------------------------------------------------|#

(define (call-with-profiling (path <string>) thunk)
  (%profile-start path #f #t)
  (bind ((#rest r (thunk)))
    (done-with-profiling path)
    (list->values r)))

(define (done-with-profiling path)
  (%profile-stop)
  ;;
  (let ((tbl (make-object-table)))
    (%profile-objects path tbl)
    (%profile-start path #t #t)
    (table-for-each
     tbl
     (lambda (h k v)
       (%profile-append-defn 
	k
	(let ((nm (name k)))
	 (if (symbol? nm)
	     (symbol->string nm)
	     nm)))
       (values)))
    (%profile-stop)))

