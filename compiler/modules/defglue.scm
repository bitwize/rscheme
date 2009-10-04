#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/modules/defglue.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.7
 | File mod date:    1998-12-03 17:27:48
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 | Purpose:          compile "define-glue" constructs
 `------------------------------------------------------------------------|#

;;; pre CR 719 doesn't have these, so define them to return #f
;;; for cross-compiling from pre-0.7.3

(define-method line-number ((self <curly-braced>))
  #f)

(define-method input-port-name ((self <curly-braced>))
  #f)

;;;

(define (compile-tl-define-glue tl-def tl-envt dyn-envt)
  (bind ((body other-kwds other-flags template? literals envt props stypes
	       (parse-glue-body (cddr tl-def) tl-envt dyn-envt))
	 (name (caadr tl-def))
	 (args (cdadr tl-def))
	 (body1 args literals (parse-safe-glue-args 
			       args
			       literals
			       stypes
			       (car body)
			       tl-envt
			       dyn-envt))
	 (body (cons body1 (cdr body))))
    ;;
    ;;
    (define (make-glue-envt frames)
      (if (null? frames)
	  #f
	  (make-gvec* <binding-envt>
		      (make-glue-envt (cdr frames))
		      (car frames))))
    ;;
    (if *tl-report*
	(format #t "declaring raw glue: ~a ~a\n" 
		(if template? "template" "procedure")
		name))
    ;;
    (if (not (null? other-kwds))
	(error "glue: excess keywords: ~s" other-kwds))
    (if (not (null? other-flags))
	(error "glue: excess flags: ~s" other-flags))
    ;;
    (let ((tmpl (glue-template name args literals body props))
	  (tlv (ensure-writable-tlv name tl-envt)))
      ;;
      (set-value! tlv
		  (if template? 
		      tmpl 
		      (make <target-closure>
			    environment: (make-glue-envt envt)
			    template: tmpl)))
      (set-write-prot! tlv #t)
      ;; return #f => generate no initialization code
      #f)))

(define (glue-template name args literals body props)
  (let ((tmpl (make-gvec* <template> 
			  0 0 '()
			  literals))
  	(bod (map (lambda (m)
		    (if (c-text? m)
			(c-text->string m)
			(if (pair? m)
			    (cons (car m) 
				    (c-text->string (cadr m)))
			    (error/syntax "bad glue body form: ~s" m))))
		    body)))
	(seq-add! (fluid-ref *code-descriptors*)
		    (make <code-descriptor>
			template: tmpl
			code-properties: (append 
					  `((function-scope ,name))
					  props)
			code: (vector args bod)
			strategy: 'literal-c))
	tmpl))

