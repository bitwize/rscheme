

(define (compile-tl-define-glue self tl-def tl-envt dyn-envt mode)
  (if (not (eq? mode 'top))
      (error "define-glue: not valid except at top"))
  (compile-tl-define-glue* tl-def tl-envt dyn-envt))

;;; A file-spec, for this purpose, is either
;;; a <string> filename, or a list whose
;;; first element is a <string> filename to be bound,
;;; e.g.,
;;;    "foo.c"
;;; or:
;;;    ("foo.c" other-c-flags: "-DIN_RSCHEME")

(define (bind-file-spec-to-this-dir f)
  (if (pair? f)
      (cons (bind-file-spec-to-this-dir (car f))
            (cdr f))
      (if (char=? (string-ref f 0) #\<)
          f
          (pathname->string
           (append-path (current-absolute-directory)
                        (string->file f))))))

(define (bind-files-to-this-dir props)
  (map (lambda (p)
	 (case (car p)
	   ((other-c-files
	     other-h-files
	     other-o-files
	     other-lib-dirs
	     other-include-dirs
	     other-local-include-dirs)
	    (cons (car p)
		  (map bind-file-spec-to-this-dir (cdr p))))
	   (else 
	    p)))
       props))

(define (compile-tl-define-glue* tl-def tl-envt dyn-envt)
  (bind ((body other-kwds other-flags template-only? literals envt props stypes
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
	 (body (cons body1 (cdr body)))
	 (props (bind-files-to-this-dir props))
	 ;;
	 ;; note that we detect whether or not a <template> has been
	 ;; loaded by whether or not SLOT(1) is a PTR.  (If it is a PTR,
	 ;; then presumably it refers to a PART-DESCR.  However, since
	 ;; the PART-DESCR is not even filled in until the code is flushed,
	 ;; we have to fill SLOT(1) with some other PTR...)
	 ;;
	 (templ (make-gvec* <template> 0 'deferred
			    `((function-scope ,name) ,@props)
			    literals)))
    ;;
    (accumulate-code! 
     (make <code-descriptor>
	   template: templ
	   properties: `((function-scope ,name) (via define-glue) ,@props)
	   strategy: 'literal-c
	   code: (vector args 
			 (map (lambda (code-frag)
				(if (pair? code-frag)     ; is it labelled?
				    (cons (car code-frag) ; the label
					  (text (cadr code-frag)))
				    (text code-frag)))
			      body))))
    ;;
    ;;(compile-and-load (list cd))
    ;;
    (let* ((stub-template (make-trampoline-template templ))
	   (val (if template-only?
		    templ
		    (make <closure>
			  template: stub-template
			  environment: '())))
	   (var (ensure-tlv (the-top-level tl-envt) name #t)))
      (set-value! var val)
      (if template-only?
	  ;; force a flush, because we don't have an indirection
	  ;; through a <procedure> to hide the lazy compilation
	  (flush-all-code))
      name)))

;;;

(define (ensure-tlv tle (name <symbol>) const?)
  (let ((bdg (lookup tle name)))
    (cond
     ;; there is no binding yet, so create one
     ((not bdg)
      (let ((bdg (new-bdg name const?)))
	(bind! tle bdg)
	bdg))
     ;; there is already a TLV binding
     ((instance? bdg <top-level-var>)
      (if (write-prot bdg)
	  (warning "constant `~s' is being re-defined" name)
	  (if (and const? (not (eq? (value bdg) '#unbound)))
	      (warning "`~s' being re-defined as a constant" name)))
      (set-write-prot! bdg const?)
      bdg)
     (else
      ;; there is already a binding, but its not a TLV
      (warning "`~s' being re-defined as a ~s (was a ~s)"
	       name 
	       (if const? "constant" "variable")
	       (class-name (object-class bdg)))
      (let ((bdg (new-bdg name const?)))
	(bind! tle bdg)
	bdg)))))

(define (new-bdg name const?)
  (make <top-level-var>
	name: name
	value: '#uninit
	write-prot: const?))

(define (provide-special-form module name proc)
  (let (((b <special-form>) (lookup (top-level-envt (get-module module)) 
				    name)))
    (set-compiler-proc! b proc)
    (set-compiler-description! b name)
    (values)))

(provide-special-form '*basic* 'define-glue compile-tl-define-glue)
(provide-special-form '*basic* '%strategy compile-pragma)
