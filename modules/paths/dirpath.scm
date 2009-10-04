#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/paths/dirpath.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.7
 | File mod date:    2003-06-22 18:15:05
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  paths
 |
 | Purpose:          parsing to make <directory-name>'s and utilities
 `------------------------------------------------------------------------|#

;;
;; *current-dir* is a high-level, conceptual "current place"
;; for example, when loading a file, *current-dir* is the directory
;; of the file
;;
;; (process-directory) accesses a low-level concept; where the process's
;; current working directory is, hence the directory to which 
;; the os path "." refers

;;
;;  this is cleared when the process-level wd is changed
;;  by chdir.  Hence, it should never be referred to directly,
;;  only via (process-directory)
;;

(define *process-dir* #f)
(set! *process-dir* #f)         ; reset to #f when image is reloaded

(define (process-directory)
  (if (not *process-dir*)
      (set! *process-dir* (string->dir (os-getwd))))
  *process-dir*)

;; #[<dir> ./]

(%early-once-only
(define $dot-dir (make <directory-name>
		       rooted-at: #f
		       steps: '())))

(define-thread-var *current-dir* $dot-dir)

;; used to be an explicit fluid var, no longer

;;  current-directory tries to return a path relative
;;  to the process directory

(define (current-directory)
  *current-dir*)

;;  current-absolute-directory returns an absolute path
;;  it will signal an error if it is unknown

(define (current-absolute-directory)
  (let ((p (process-directory)))
    (if (rooted-at p)
	(append-dirs p (current-directory))
	(error "current-absolute-directory: absolute path not known"))))

(define-method pathname->string ((p <directory-name>))
  (dir->string p "/" root-name))

(define-method pathname->os-path ((self <directory-name>))
  (dir->string self "" expanded-name))

(define (up->dot-dot item)
  (if (eq? item 'up)
      ".."
      item))

(define (dir->string (p <directory-name>) (suffix <string>) root-xlate)
  (let* ((r (rooted-at p))
	 (rx (if r (root-xlate r) "")))
    (if (null? (steps p))
	(if r rx (string-append "." suffix))
	(let ((m (string-join #\/ (map up->dot-dot (steps p)))))
	  (string-append rx m suffix)))))

; note that "/foo" relative to "../bleen" gives "/foo"
; (which is what one should expect)

(define (append-dirs p1 p2)
  (if (rooted-at p2)		;; if p2 is rooted, then forget relativity
      p2
      (let loop ((i (append (steps p1) (steps p2))) 
		 (r '()))
	(if (null? i)
	    (make <directory-name> 
		  rooted-at: (rooted-at p1) 
		  steps: (reverse r))
	    (case (car i)
	      ((up) (if (and (pair? r)
			     (not (eq? (car r) 'up)))
			(loop (cdr i) (cdr r))
			(loop (cdr i) (cons 'up r))))
	      (else (loop (cdr i) (cons (car i) r))))))))

(define (string->dir str)
  (steps->dir (parse-dname str)))

(define (steps->dir steps)
  (bind ((r s (simplify steps)))
    (if (and (null? s) (not r))
	$dot-dir
	(make <directory-name> 
	      rooted-at: r
	      steps: s))))

; returns the NAME of the last directory in the given directory
; or #f if the last directory is not named (ie, it is "..")
;   for example:
;	(last-directory (string->dir "/tmp/foo")) => "foo"
;	(last-directory (string->dir "../bleen/foo")) => "foo"
;	(last-directory (string->dir "../..")) => #f

(define (last-directory dir)
    (let ((p (steps dir)))
	(if (null? p)
	    #f
	    (let ((l (last p)))
		(if (string? l)
		    l
		    #f)))))


;; eliminate a common prefix of two directories
;;
;;  For example:
;;      d1 = /tmp/foo_dir/
;;      d2 = /tmp/quux_dir/
;;  Compute how to get from d1 to d2
;;  (dir-from-to d1 d2) ==> ../quux_dir/

;;	To get from		To			    Use
;;	-----------		----------------------	    -----------
;;      /tmp/foo_dir		/tmp/foo_dir/blech_dir	==> blech_dir/
;;      /tmp/foo_dir/blech_dir	/tmp/foo_dir		==> ../
;;	../blah/foo/		../blah/quux/		==> ../quux
;;	/tmp/foo_dir		../blah			==> $(WD/..)/blah
;;	./			../foo			==> ../foo
;;	blech_dir/		./			==> ../
;;	blech_dir/yy		foo_dir/xx		==> ../../

(define (dir-from-to d1 d2)
    (if (and (rooted-at d2) (not (rooted-at d1)))
	d2
	(if (and (rooted-at d1) (not (rooted-at d2)))
	    (dir-from-to d1 (append-dirs (current-absolute-directory) d2))
	    (if (and (rooted-at d1) 
		     (rooted-at d2) 
		     (not (equal? (car (steps d1)) 
		     	          (car (steps d2)))))
	    	d2
		(let loop ((from (steps d1)) (to (steps d2)))
		    (if (null? from)
			(make <directory-name> 
			      steps: to
			      rooted-at: #f)
			(if (and (pair? to) 
				    (equal? (car from) (car to)))
			    (loop (cdr from) (cdr to))
			    (make <directory-name> 
				  rooted-at: #f
				  steps: (append (map (lambda (x) 
							'up) 
						      from) 
						 to)))))))))
						

;;
;;  return a list of the directories above (and including) the
;;  given directory
;;
;;  for example
;;    (dir-parents #[<dir> /tmp/foo/bar/quux/]) =>
;;          (#[<dir> /tmp/]
;;           #[<dir> /tmp/foo/]
;;           #[<dir> /tmp/foo/bar/]
;;           #[<dir> /tmp/foo/quux/])

(define (dir-parents (dir <directory-name>))
  (let loop ((d (reverse (steps dir)))
	     (a '())
	     (r '()))
    (if (null? d)
	r
	(let ((next-a (cons (car d) a)))
	  (loop (cdr d)
		next-a
		(cons (make <directory-name>
			    rooted-at: (rooted-at dir)
			    steps: (reverse d))
		      r))))))

;;;

(define-method relative-file ((file <string>))
  (pathname->os-path (append-path (current-directory) 
				  (string->file file))))

(define-method relative-file ((file <file-name>))
  (pathname->os-path (append-path (current-directory) file)))

(define-method relative-file ((dir <directory-name>))
  (pathname->os-path (append-dirs (current-directory) dir)))

;;;
;;;  Remove the common prefix directory from a file name
;;;

(define (file-relative-to-dir (f <file-name>) (dir <directory-name>))
  (make <file-name>
        file-directory: (dir-from-to dir (file-directory f))
        filename: (filename f)
        extension: (extension f)))

