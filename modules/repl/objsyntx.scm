#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/repl/objsyntx.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    1998-12-20 22:20:00
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  repl
 |
 `------------------------------------------------------------------------|#

; CHECKING CODE FOR OBJECT SYSTEM DEFINING FORMS

; For each object system defining form, we have a checking procedure
; that checks mostly syntax.  Things that are clearly errors cause
; the checker to print an error message and return false.  Things
; that are questionable cause it to issue a warning and return #t.
; The assumption is that compilation will stop at an error, and
; that the warnings will be useful for two things: reminding people
; of dangerous stuff (like redefining classes), and making it easier
; to figure things out if the compiler dies with an internal error.


; WARN and ERRMSG print warning or error messages, and return #t or
; #f (so that you can use them in tail position to return a value
; signifying whether a test passed with a warning or failed with
; an error).  We might want to come up with versions of these that
; take a port argument, e.g. pwarn and perrmsg, and/or make these
; write to standard error by default.  We should move these (or some
; improved version into the default environment, or at least the
; grubby environments like the compiler envt.

; WARN just prints WARNING: followed by its arguments, and returns #t

(define (warn . args)
   (display "WARNING: ")
   (for-each (lambda (arg)
                (display arg))
             args)
   (newline)
   #t)

; ERRMSG is just like WARN except that it prints ERROR: ... and return #t.

(define (errmsg . args)
   (display "ERROR: ")
   (for-each (lambda (arg)
                (display arg))
             args)
   (newline)
   #f)

; CHECK-DEFINE-GENERIC-FUNCTION-SYNTAX ensures that a generic function
; name is given, and warns if it's a redefinition of the generic function
; or of another kind of binding.

(define (check-define-generic-function-syntax form envt)
   (if (pair? (cdr form))
       (if (symbol? (cadr form))
           (if (envt-bound? envt (cadr form))
               (if (envt-bound-to-gf? envt (cadr form))
                   (warn " in DEFINE-GENERIC-FUNCTION, redefining: "
                         (cadr form) #\newline
                         " (Be sure to update dependent methods.)")
                   (warn " in DEFINE-GENERIC-FUNCTION, redefining: "
                         (cadr form) #\newline
                         " CLOBBERING NON-GENERIC-FN BINDING!"))
               #t)
           (errmsg "ERROR in DEFINE-GENERIC-FUNCTION---function name: "
                   (cadr form) #\newline "is not a symbol"))
       (errmsg "ERROR in DEFINE-GENERIC-FUNCTION:" form
               #\newline " (not enough subforms).")))

(define (check-define-class-syntax form envt)

   ; We wrap the body of this procedure in a call/cc.  If no errors
   ; are detected, the body lambda will return normally, and the returned
   ; value will be returned as the value of call/cc and the whole
   ; procedure.  If an error is detected, the continuation will be
   ; be invoked to return to here.
   (call-with-current-continuation

    (lambda (escape)

       ; FAIL will print out an error message and then bail out of
       ; this procedure, returning #f.
       (define (fail . args)
          (display "ERROR in define-class ")
          ; if name of class looks reasonable, display that
          (cond ((and (pair? (cdr form))) (symbol? (cadr form))
                 (display (cadr form))
                 (newline)))
          (for-each display args)
          (newline)
          (escape #f))  ; nonlocally return #f as result of call/cc

       ; CHECK-SLOT-SPEC tests that a slot spec is either just a symbol
       ; or a list starting with a symbol; it also makes sure that
       ; generating an accessor with the same name won't clobber an
       ; existing binding of something else.

       (define (check-slot-spec slot-spec)
          (let ((slot-name (cond ((symbol? slot-spec)
                                  slot-spec)
                                 ((and (pair? slot-spec)
                                       (symbol? (car slot-spec)))
                                  (car slot-spec))
                                 (#t
                                  (fail "bad slot specifier:" slot-spec)))))
             (if (envt-bound? envt slot-name)
                 (if (not (envt-bound-to-gf? envt slot-name))
                     (fail " bad slot name: " slot-name
                           ".  Can't create accessor gen. fn.---name conflicts"
                           #\newline " with existing binding of that name,"
                           " whose value is not a generic fn.")))))
    
       ; CHECK-SUPER-SPEC tests that a superclass spec looks like a
       ; it will yield a class.  If it's a symbol, it actually checks
       ; to see that the symbol is bound and evaluates to a class.
       ; Otherwise it warns, because it can't tell whether an arbitary
       ; expression evaluates to a class.

       (define (check-super-spec super-spec)
          (if (symbol? super-spec)
              (if (envt-bound? envt super-spec)
                  (if (not (envt-bound-to-class? envt super-spec))
                      (fail " superclass specifier " super-spec
                            " does not evaluate to a class" #\newline))
                  (fail " superclass specifier " super-spec " is unbound."))
              (warn "superclass specifier" super-spec " is not a variable"
                    " name;  that's okay if you INTEND" #\newline
                    " to use a computed superclass." #\newline)))

       ; STRIP-LEADING-KWDS removes all flags (:foo) and keywords (foo: n)
       ; from the beginning of a list

       (define (strip-leading-kwds lst)
	 (if (pair? lst)
	     (cond
	      ((keyword? (car lst))
	       (if (pair? (cdr lst))
		   (strip-leading-kwds (cddr lst))
		   (fail "malformed class definition keyword: " (car lst))))
	      ((flag? (car lst))
	       (strip-leading-kwds (cdr lst)))
	      (else
	       lst))
	     lst))

       ; start doing the actual checking...

       (if (not (list? form))
           (fail "ill-formed expression"))
       (if (not (> (length form) 2))
           (fail "not enough subexpressions (name, supers)"))
       (let ((class-name (cadr form))
             (superclass-list (caddr form))
             (slot-list (strip-leading-kwds (cdddr form))))

          (if (not (symbol? class-name))
              (fail " class name not a symbol"))
          (if (envt-bound? envt class-name)
              (cond ((envt-bound-to-class? envt class-name)
                     (warn " redefining class " class-name #\newline
                           " (Old subclasses and instances must be"
                           " recreated to use new definiton.)"))
                    (#t
                     (warn " defining class clobbers old value of" #\newline
                           " binding of class name: " class-name))))
          (if (not (and (pair? superclass-list)
                        (null? (cdr superclass-list))))
              (fail " Bad superclass list: " superclass-list #\newline
                    " need list of exactly one superclass (multiple" #\newline
                    " inher. not supported yet).  Use (<object>) by default."))

          (for-each (lambda (super-spec)
                       (check-super-spec super-spec))
                    superclass-list)

          (for-each (lambda (slot-spec)
                       (check-slot-spec slot-spec))
                    slot-list)

          #t))))  ; No errors detected---normally return #t as value of call/cc


;;; CHECK-FORMALS-LIST ensures that an formal arguments list is
;;; syntactically valid

(define (check-formals-list flist envt)
  (bind ((required optional rest key (parse-formals flist)))
    ;; required and rest arguments are either
    ;; `name', `(name)', `(name type)', or `(name type: type)'
    (for-each (rcurry check-required-arg envt) required)
    (for-each (rcurry check-optional-arg envt) optional)
    (for-each (rcurry check-rest-arg envt) rest)
    (for-each (rcurry check-key-arg envt) key)))

(define (check-required-arg arg-spec envt)
  (parse-rqd-spec arg-spec))

;;; well, we do some semantic checking too... clearly this needs to
;;; be improved!

(define (check-rest-arg arg-spec envt)
  (bind ((name type (parse-rqd-spec arg-spec))
	 (t (if type
		(parse-type-expr (car type) envt envt)
		<list>)))
    (cond
     ((eq? t <vector>)
      (error/semantic* arg-spec
		       "#rest arg of type <vector> is not yet supported\n   must be of type <list>"))
     ((eq? t <list>)
      (values name type))
     (else
      (error/semantic* arg-spec "#rest arg of must be of type <list>")))))

(define (check-optional-arg arg-spec envt)
  (bind ((var-name type dflt (parse-arg-spec arg-spec)))
    #t))

(define (check-key-arg arg-spec envt)
  (check-optional-arg arg-spec envt))

(define (check-define-method-syntax form envt)
  (if (not (and (pair? (cdr form)) 
		(pair? (cddr form))))
      (error/syntax "Not enough subforms\n   Expected at least generic function name and formals list"))
  ;;
  (let ((gen-fn (cadr form))
	(arg-list (caddr form))
	(body-exprs (cdddr form)))
    (if (symbol? gen-fn)
	(if (and (envt-bound? envt gen-fn)
		 (not (envt-bound-to-gf? envt gen-fn)))
	    (error/semantic
	     "First subform: ~s\n   doesn't evaluate to a generic function"
	     gen-fn))
	(error/syntax "First subform: ~s\n   is not a symbol.  Computed GFs are not supported" gen-fn))
    (if (not (pair? (car arg-list)))
	(warning "first arg. decl: ~s\n   should have an explicit type restriction for dispatching\n   (defaulting type restriction to <object>)"
		 (car arg-list)))
    (check-formals-list arg-list envt)
    #t)) ; no errors detected---return #t normally as result of call/cc

;;;;; STUBS FOR TESTING:
;
;  (define cdcs check-define-class-syntax)
;  (define cdms check-define-method-syntax)
;  (define cdgfs check-define-generic-function-syntax)

;  (define bound-vars '(gf1 gf2 v1 v2 v3 v4 c1 c2 c3 c4))

; (define gf-vars '(gf1 gf2))

; (define class-vars '(c1 c2 c3 c4))

; (define (bound? x) (member x bound-vars))
; (define (bound-to-gf? x) (member x gf-vars))
; (define (bound-to-class? x) (member x class-vars))

(define (envt-bound-to-gf? envt x)
  (let ((b (envt-binding-of envt x)))
    (and (top-level-var? b)
	 (generic-function? (binding-value b)))))

(define (envt-bound-to-class? envt x)
  (let ((b (envt-binding-of envt x)))
    (and (top-level-var? b)
	 (class? (binding-value b)))))

;;

(define (envt-binding-of envt (name <symbol>))
  (lookup envt name))

(define (envt-bound? envt (name <symbol>))
  (let ((b (lookup envt name)))
    (if (and b (not (eq? (value b) '#unbound)))
	#t
	#f)))

(define (generic-function? thing)
  (instance? thing <generic-function>))

(define (top-level-var? thing)
  (instance? thing <top-level-var>))

(define (binding-value (bdg <top-level-var>))
  (value bdg))
