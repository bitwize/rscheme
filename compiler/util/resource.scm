#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/util/resource.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.3
 | File mod date:    2002-11-05 21:36:37
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#


;; a source resource is a resource found in 
;; the SOURCE's resource search path
;; (that is, either in $SOURCE/resource or in $SOURCE/dist/install/resource)
;;
;; (note that that means we can access resources that will eventually
;;  be installed)

(define (locate-src-resource rsrc-path)
  (locate-resource *source-resource-path* rsrc-path))

(define (locate-dist-resource rsrc-path)
  (locate-resource *dist-resource-path* rsrc-path))

(define (locate-resource search-path rsrc-path)
  (let ((f (string->file rsrc-path)))
    (format #t "looking for ~s along ~s\n" rsrc-path search-path)
    (let loop ((i search-path))
      (if (null? i)
	  #f
	  (let ((p (append-path (car i) f)))
	    ;(format #t "checking: ~s\n" p)
	    (if (file-exists? (pathname->string p))
		p
		(loop (cdr i))))))))


;; return the pathname to a resource that is to be placed
;; in the distribution, and is intended to be installed

(define (install-resource-path rsrc-path)
  (append-path *dist-install-resource-dir* (string->file rsrc-path)))

;; return the pathname to a resource that is to be put
;; in the distribution, but is NOT intended to be installed

(define (distribution-resource-path rsrc-path)
  (append-path *dist-resource-dir* (string->file rsrc-path)))


(define (string->dist-dir in-dir str)
  (let* ((d (string->dir str))
	 (steps (gvec-ref d 1))
	 (h (if (null? steps)
		#f
		(car steps))))
    (if (string? h)
	(if (string=? h "$install")
	    (begin
	      (gvec-set! d 1 (cdr (gvec-ref d 1)))
	      (append-dirs (append-dirs *dist-path* 
					(string->dir "install"))
			   d))
	    (if (string=? h "$dist")
		(begin
		  (gvec-set! d 1 (cdr (gvec-ref d 1)))
		  (append-dirs *dist-path* d))
		(append-dirs in-dir d)))
	(append-dirs in-dir d))))

;;
;;  mkdirs or mkdir -p functionality
;;

(define (ensure-directory (dir <directory-name>))
  (if (file-exists? (pathname->string dir))
      dir
      (begin
	(ensure-directory (append-dirs dir (string->dir "..")))
	(mkdir (pathname->string dir)))))

;; these are like the "...-file" variants, but take a <file-name>
;; and create the directory if necessary

(define (with-output-to-path path thunk)
  (ensure-directory (file-directory path))
  (with-output-to-file (pathname->string path) thunk))

(define (call-with-output-path path proc)
  (ensure-directory (file-directory path))
  (call-with-output-file (pathname->string path) proc))
