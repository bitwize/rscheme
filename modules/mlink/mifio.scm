#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/mlink/mifio.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.20
 | File mod date:    1998-08-29 19:45:08
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  mlink
 |
 | Purpose:          <module> image file (.mif) I/O
 `------------------------------------------------------------------------|#

(define-class <part-descr> (<object>)
    image-mode: 3
  module-name
  part-tag
  linkage)

(define-generic-function execute-link-cmd)

(define *verbose-link* #f)

;; load and save module interfaces

(define *mif-load-defs* #f)
(define *mif-save-defs-1* #f)
(define *mif-save-defs-2* #f)
(define *mifio-defs* '())

(define (clear-mifio-cache)
  (set! *mif-load-defs* #f)
  (set! *mif-save-defs-1* #f)
  (set! *mif-save-defs-2* #f)
  (values))

(define (add-mifio-class (name <string>) (class <<class>>))
  (let ((x (assoc name *mifio-defs*)))
    (set! *mifio-defs* (cons (cons name class) 
			     (if x 
				 (delq x *mifio-defs*)
				 *mifio-defs*)))
    (clear-mifio-cache)))
  
(define-rewriter (mifio-class form)
  (let ((name (cadr form))
	(class (caddr form)))
    `(%early-once-only (add-mifio-class ,name ,class))))

(define (mif-load-defs)
  (if (not *mif-load-defs*)
      (let ((t (make-table string=? string->hash)))
	(set! *mif-load-defs* t)
	(for-each
	 (lambda (e)
	   (table-insert! *mif-load-defs*
			  (car e)
			  (cdr e)))
	 *mifio-defs*)))
  *mif-load-defs*)

(define (mif-save-defs)
  (if (not *mif-save-defs-1*)
      (begin
	(set! *mif-save-defs-1* (list->vector (map cdr *mifio-defs*)))
	(set! *mif-save-defs-2* (list->vector (map car *mifio-defs*)))))
  (values *mif-save-defs-1* *mif-save-defs-2*))

;;
;; methinks this is perhaps somewhat Linux specific...?
;;

(define (find-loadable-c-module (path <file-name>) (cm <string>))
  (let ((p (pathname->os-path (make <file-name>
				    filename: (string-append "lib" cm)
				    extension: "so"
				    file-directory: (file-directory path)))))
    (if (os-file-exists? p)
	p
	#f)))

(define (ensure-c-modules-loaded why (path <file-name>) mx)
  (for-each 
   (lambda (cm)
     (if (not (find-linked-module cm))
	 ;; not already loaded... see if we can load it
	 (let ((loadable-path (find-loadable-c-module path cm)))
	   (if loadable-path
	       (begin
		 (if *verbose*
		     (format #t "loading C unit ~a: ~a\n" cm loadable-path))
		 (dl-c-unit loadable-path cm))
	       (error "~a: required C module `~a' not loaded" why cm)))))
   (caddr mx)))

(define (load-module (m-name <symbol>) 
		     (path <file-name>)
		     executable?)
  ;;
  ;;  load and exercise the MX file (meta info)
  ;;
  (let ((mx-info (call-with-input-file (pathname->os-path 
					(extension-related-path path "mx"))
		   read)))
    ;;
    (if executable?
	(ensure-c-modules-loaded m-name path mx-info))
    ;;
    ;;  load the actual image file
    ;;
    (load-image (pathname->os-path path)
		(mif-load-defs)
		(if executable?
		    #f
		    <part-descr>))))

(define (save-module path m)
  (bind ((objs names (mif-save-defs)))
    (save-image path m objs names #f)))
