#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/modules/document.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.3
 | File mod date:    2003-08-13 21:20:14
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#

;;
;;  hooks to support automated collection of documentation
;;

;;
;; called to preprocess a top-level form, such as a `define-class'
;; or an expr
;;

(define (preprocess-toplevel-form form lex-envt dyn-envt)
  ;; just capture the basics for now
  (if (pair? form)
      (case (car form)
	((define)
	 (if (pair? (cadr form))
	     (found-defining-form 'function (caadr form) '())
	     (found-defining-form 'variable (cadr form) '())))
	((define-class)
	 (found-defining-form 'class (cadr form) '()))
	((define-glue)
	 (if (pair? (cadr form))
	     (found-defining-form 'glue (caadr form) '((type function)))
	     (found-defining-form 'glue (cadr form) '((type template)))))
	((define-method)
	 (found-defining-form 'method (cadr form) '()))
	((define-syntax)
	 (found-defining-form 'syntax 
			      (if (pair? (cadr form))
				  (caadr form)
				  (cadr form))
			      '()))))
  ;;
  form)

;;
;; called to write out the documentation for a module build
;;

(define (write-documentation (bcx <build-context>))
  (if *tl-report*
      (format #t "saving documentation...\n"))
  (let ((m (building bcx)))
    (call-with-output-file
	(pathname->string (make <file-name>
				filename: (base-filename bcx)
				extension: "doc"
				file-directory: (image-dest-dir bcx)))
      (lambda (port)
	(write (list (name bcx)
		     (map name (module-imports m))
		     (map name (value-sequence (module-exports m)))
		     (reverse (documentation-data bcx)))
	       port)))))

  
;;
;;  internal functions
;;

(define (found-defining-form name type info)
  (set-documentation-data!
   *build-context*
   (cons (list (pathname->string *source-file*) name type info)
	 (documentation-data *build-context*))))
