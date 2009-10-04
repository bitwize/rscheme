#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/boot/replace.scm
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



;; proc is called exactly once for
;; each object (including immobs) reachable from
;; `root', where reachable is defined
;; to stop at the objects listed in `stop' (proc is not called for tem)
;; and objects for which `proc' returns #f do not have their
;; substructure examined
;;
;; the view of the data structure starting at `root' is through
;; the `rplc' filter

(define (for-each-reachable root (stop <list>) (rplc <table>) proc)
  (let ((tbl (make-object-table)))
    (define (found x)
      (if (not (table-lookup tbl x))
	  (begin
	    (table-insert! tbl x #t)
	    (let ((r (table-lookup rplc x)))
	      (if r
		  (found r)
		  (if (and (proc x) (gvec? x))
		      ;; examine the substructure
		      (let loop ((i (gvec-length x)))
			(if (eq? i 0)
			    (found (object-class x))
			    (let ((k (- i 1)))
			      (found (gvec-ref x k))
			      (loop k))))))))))
    (for-each (lambda (item)
		(table-insert! tbl item #t))
	      stop)
    (found root)))

(define (rebuild-symbol-table root stop rplc)
  (let ((new-table (make-gvec <string-table>
			      (make-vector 1024 #f)
			      10
			      0
			      <table-bucket>)))
    new-table))
