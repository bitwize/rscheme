#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/inline.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    2003-01-04 16:37:31
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#

(define-macro define-inline
  (macro-rules (nlambda)
   ((_ (proc . args) . body)
    (let* ((full-name (symbol-append "." proc))
	   (temps (map (lambda (a)
			 (symbol-append "."
					(if (pair? a)
					    (car a)
					    a)))
		       args))
	   (locals (map list args temps)))
      `(begin
	 (define (,full-name ,@args)
	   ,@body)
	 (define-syntax ,proc
	   (syntax-form (,@temps)
	     (let-syntax ((,proc (else ,full-name)))
	       (let (,@locals)
		 ,@body)))
	   (else ,full-name)))))
   ((_ proc (nlambda . ncases))
    (let ((full-name (symbol-append "." proc)))
      (define (one-case c)
	(let* ((args (car c))
	       (body (cdr c))
	       (temps (map (lambda (a)
			     (if (eq? a '#rest)
				 #f
				 (symbol-append "."
						(if (pair? a)
						    (car a)
						    a))))
			   args))
	       (locals (map list args temps)))
	  (if (memq '#rest args)
	      #f
	      `((syntax-form (,@temps)
		  (let-syntax ((,proc (else ,full-name)))
		    (let (,@locals)
		      ,@body)))))))
      ;
      `(begin
	 (%early-once-only
	  (define ,full-name
	    (nlambda ,@ncases)))
	 ;
	 (define-syntax ,proc
	   ,@(apply append (map one-case ncases))
	   (else ,full-name)))))))

(define-macro (nlambda . ncases)
  (bind ((name-insert ncases (if (and (pair? ncases)
				      (pair? (car ncases))
				      (eq? (caar ncases) 'quote))
				 (values `(',(cadar ncases)) (cdr ncases))
				 (values '() ncases))))
    (if (= (length ncases) 1)
	`(lambda ,@name-insert ,@(car ncases))
	(let* ((arglist (gensym))
	       (thecases (map 
			  (lambda (c)
			    (if (memq '#rest (car c))
				`(else
				  (bind ((,@(car c) (list->values ,arglist)))
				    ,@(cdr c)))
				`((,(length (car c)))
				  (bind ((,@(car c) (list->values ,arglist)))
				    ,@(cdr c)))))
			  ncases)))
	  `(lambda ,@name-insert ,arglist
	     (case (length ,arglist)
	       ,@thecases))))))
