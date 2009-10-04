#|------------------------------------------------------------*-Scheme-*--|
 | File:	    compiler/modules/parseglue.scm
 |
 |          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.4
 | File mod date:    1999-01-25 07:47:38
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 | Purpose:          Parse the flags and arguments for a define-glue
 `------------------------------------------------------------------------|#

(define-class <safe-glue-types> (<object>)
  safe-type-handlers
  safe-type-views)

;;;  a helper procedure for parsing CONST-EXPRs, which
;;;  are either (& VARNAME) or compile-time constant expressions

(define (compile-glue-literal expr lex-envt dyn-envt)
  (if (and (pair? expr) (eq? (car expr) '&))
      (if (and (= (length expr) 2)
	       (symbol? (cadr expr)))
	  (let* ((n (cadr expr))
		 (b (lookup-aliased n lex-envt dyn-envt)))
	    (cond
	     ((instance? (actual-bdg b) <top-level-var>)
	      b)
	     ((not b)
	      (let ((b (make <top-level-var>
			     name: n
			     value: '#unbound)))
		(bind! (the-top-level lex-envt) b)
		b))
	     (else
	      (error "glue: not a top-level-var: ~s" n))))
	  (error "glue: bad `&' syntax: ~s" expr))
      (parse-const-expr expr lex-envt dyn-envt)))

;;;
;;;  parses a glue body form, which goes something like:
;;;
;;;    GLUE-BODY ::= [:template] 
;;;                  [literals: (CONST-EXPR ...)]
;;;                  [envt: ((CONST-EXPR ...) ...)]
;;;                  [properties: ((KEY VALUE ...) ...)]
;;;                  [type-handler: (TYPE-NAME . CHECKER-SPEC)]*
;;;                  [type-view: (VIEW-NAME . VIEW-SPEC)]*
;;;                  {C-FRAGMENT}
;;;                  [ ("LABEL" {C-FRAGMENT}) ... ]
;;;
;;;  (see description of `parse-safe-glue-args', below, for the
;;;  interpretation of CHECKER-SPEC and VIEW-SPEC)
;;;
;;;  returns multiple values:
;;;    - ({C-FRAGMENT} ("LABEL" {C-FRAGMENT}) ...)
;;;    - OTHER-KEYWORDS
;;;    - OTHER-FLAGS
;;;    - TEMPLATE-FLAG?
;;;    - LITERALS-LIST
;;;    - ENVT-FRAME-LIST
;;;    - PROPERTIES
;;;    - SAFE-TYPES-ENVT

(define (c-text? thing)
  (instance? thing <curly-braced>))

(define (parse-glue-body form lex-envt dyn-envt)
  ;;
  (define (parse-glue-literal expr)
    (compile-glue-literal expr lex-envt dyn-envt))
  ;;
  (let ((template-flag? #f)
	(envt-frames #f)
	(literal-values #f)
	(properties #f)
	(type-handlers '())
	(type-views '())
	(other-keywords '())
	(other-flags '()))
    (let loop ((f form))
      (cond
       ((null? f)
	(error "glue: {C-TEXT} missing"))
       ;; parse all keyword options;
       ;; they consist of the keyword followed by a value
       ((keyword? (car f))
	(case (car f)
	  ;; parse a `literals:' keyword
	  ((literals:)
	   (if literal-values
	       (error "glue: mutiple `literals:'")
	       (set! literal-values (map parse-glue-literal (cadr f)))))
	  ;;
	  ((properties:)
	   (set! properties (merge-properties (or properties '())
					      (cadr f))))
	  ;;
	  ;; NOTE:  these appear to be added in reverse order
	  ;; (cons instead of append), but the way the macros are
	  ;; written, "higher"-level annotations will be seen later
	  ;;
	  ;; add a named type view
	  ((type-view:)
	   (set! type-views (cons (cadr f) type-views)))
	  ;; add a type handler
	  ((type-handler:)
	   (set! type-handlers (cons (cadr f) type-handlers)))
	  ;; put everything else on the `other-keywords' list
	  (else
	   (set! other-keywords (append other-keywords
					(list (car f) (cadr f))))))
	(loop (cddr f)))
       ;; parse all flag options;
       ;; they consist of the keyword followed by a value
       ((flag? (car f))
	(case (car f)
	  ((:template)
	   (set! template-flag? #t))
	  (else
	   (set! other-flags (append other-flags (list (car f))))))
	(loop (cdr f)))
       ;; everything else must be the {C-TEXT}
       ;; verify its structure, then return the whole thing
       (else
	(if (not (c-text? (car f)))
	    (error "expected {C-TEXT} at: ~#*@50s" (car f)))
	(for-each
	 (lambda (m) ; monotone
	   (if (not (and (list? m)
			 (= (length m) 2)
			 (string? (car m))
			 (c-text? (cadr m))))
	       (error "expected (\"LABEL\" {C-TEXT}) at: ~#*@50s" m)))
	 (cdr f))
	;; make sure they didn't specify a :template together
	;; with an environment (because then we won't have a place
	;; to put the environment!)
	(if (and template-flag? (pair? envt-frames))
	    (error "glue: `:template' invalid with `envt:'"))
	;; return everything
	(values f
		other-keywords
		other-flags
		template-flag?
		(or literal-values '())
		(or envt-frames '())
		(or properties '())
		(make <safe-glue-types>
		      safe-type-views: type-views
		      safe-type-handlers: type-handlers)))))))

;;;  merges lists and replaces values -- I hope that's right!
;;;  does not preserve order of keys

(define (merge-properties old-list new-list)
  (let ((tbl (make-symbol-table)))
    (for-each
     (lambda (entry)
       (table-insert! tbl (car entry) (cdr entry)))
     old-list)
    (for-each
     (lambda (entry)
       (let ((v (table-lookup tbl (car entry))))
	 (table-insert!
	  tbl
	  (car entry)
	  (if (list? v)
	      (append v (cdr entry))
	      (cdr entry)))))
     new-list)
    (map cons (key-sequence tbl) (value-sequence tbl))))


;;;
;;;  parse the argument forms for some safe glue
;;;
;;;  type-views is the accumulation of type views, 
;;;  and hence is a list of (TYPE-NAME . CHECKER-SPEC) tuples,
;;;  where
;;;
;;;     CHECKER-SPEC ::= RECOGNITION-STYLE VIEW ...
;;;
;;;  By intention, subclasses do not count in matching TYPE-NAMEs.
;;;
;;;  the RECOGNITION-STYLE specifies how the system emits a check
;;;  for conformance to the type name.  It can either be by general
;;;  instance (direct or indirect instance membership), by exact
;;;  class membership, or by a C predicate:
;;;
;;;     RECOGNITION-STYLE ::= (instance CLASS-VAR)
;;;			   |  (direct-instance CLASS-VAR)
;;;			   |  (primitive "C_PREDICATE" CLASS-VAR)
;;;			   |  (primitive)  ;; type checking in converter
;;;
;;;  the usual case is to use general instance detection, but if
;;;  speed is necessary and no subclasses are being considered,
;;;  then direct-instance may be used.  Some types are more readily checked
;;;  by C predicates, such as using OBJ_ISA_FIXNUM to check for
;;;  fixnums.
;;;
;;;  checking a TYPE-NAME that does not have an explicit 
;;;
;;;  an arg-form looks like:
;;;
;;;    ARGS-FORM ::= (ARG-SPEC ... [#rest [REST-NAME]])
;;;                   |<--- N --->|
;;;
;;;  if REST-NAME is not specified, then the remaining arguments
;;;  are not collected into a list, but more than N arguments are 
;;;  tolerated.
;;;
;;;  each ARG-SPEC is either a simple name, which results in no
;;;  type checking or conversion, or a restriction:
;;;
;;;     ARG-SPEC ::= ARG-NAME
;;;               |  (ARG-NAME TYPE-NAME)
;;;
;;;  in the latter case, the conversion and checking is controlled
;;;  by the corresponding CHECKER-SPEC

(define (parse-safe-glue-args args-form literals safe-types first-monotone
			      lex-envt dyn-envt)
  (bind ((arg-specs rest-arg (parse-glue-rest-arg args-form))
	 (arg-infos (map parse-glue-arg-spec arg-specs)))
    ;; allocate a new literal in the literal frame
    (define (alloc-literal lit . opt)
      (if (null? opt)
	  (let ((m (memq lit literals)))
	    (if m
		(- (length literals) (length m))
		(let ((i (length literals)))
		  (set! literals (append literals (list lit)))
		  i)))
	  (case (car opt)
	    ((:tlv)
	     (alloc-literal 
	      (compile-glue-literal (list '& lit) lex-envt dyn-envt)))
	    (else
	     (error "glue: invalid literal option: ~s" (car opt))))))
    ;;
    (let ((port (open-output-string)))
      ;; emit the declarations
      (render-glue-decls port arg-infos safe-types)
      (if (symbol? rest-arg)
	  (format port "  obj ~a;\n" rest-arg))
      ;; emit the basic preamble
      ;;  - as a special hack, emit the basic preamble only if there are NO
      ;;    safe-type-handlers (because that means we're being called
      ;;    from a plain (old-format) `define-glue', which doesn't expect
      ;;    a COUNT_ARGS check)
      (if (not (null? (safe-type-handlers safe-types)))
	  (render-glue-preamble port (length arg-infos) rest-arg))
      ;; emit the type checking and conversion preamble
      (render-glue-checks-and-convs port arg-infos safe-types alloc-literal)
      ;; emit the main body
      ;;  - including a `#line' directive, if we have enough info for it
      (newline port)
      (if (and (line-number first-monotone)
	       (input-port-name first-monotone))
	  (format port "#line ~d ~s\n" (line-number first-monotone)
		  (input-port-name first-monotone)))
      ;;
      ;; (wrap it in {...} because it probably contains decls 
      ;;  at the beginning)
      (write-char #\{ port)
      (display first-monotone port)
      (write-char #\} port)
      ;; return the results
      (values
       (string->c-text (close-output-port port))
       (map (lambda (info)
	      (vector-ref info 2)) ;; raw-name
	    arg-infos)
       literals))))

;;;  return an `arg-info' which is (vector ARG-NAME {TYPE-NAME|#f} RAW-NAME)

(define (parse-glue-arg-spec arg-spec)
  (if (pair? arg-spec)
      (vector (car arg-spec)
	      (cadr arg-spec)
	      ;; this is going to be changed to FOO_obj per CR 633
	      (symbol-append "raw_" (car arg-spec)))
      (vector arg-spec #f arg-spec)))

(define (parse-glue-rest-arg arg-form)
  ;; check for a #rest arg
  (let ((r (memq '#rest arg-form)))
    (if r
	(cond
	 ((null? (cdr r))
	  ;; no REST-NAME
	  (values (reverse! (cdr (reverse arg-form))) 
		  #t))
	 ((null? (cddr r))
	  ;; a REST-NAME
	  (values (reverse! (cddr (reverse arg-form)))
		  (cadr arg-form)))
	 (else
	  (error "glue: leftovers after #rest: ~s" r)))
	(values arg-form #f))))


(define (get-glue-checker-spec type-name safe-types)
  (let ((m (assq type-name (safe-type-handlers safe-types))))
    (if m
	(cdr m)
	`((instance? ,type-name)))))

(define (glue-view-name base-name view-decl)
  (if (> (length view-decl) 2)
      (format #f (caddr view-decl) base-name)
      base-name))

(define (render-glue-decl port name view-specs safe-types)
  (if (null? view-specs)
      (format port "  obj ~a;\n" name)
      (for-each (lambda (view-spec)
		  (let ((v (expand-glue-view-spec view-spec safe-types)))
		    (format port "  ")
		    (format port (car v) (glue-view-name name v))
		    (format port ";\n")))
		view-specs)))
  
;;; expand a VIEW-SPEC into a VIEW-DECL
;;; (an identity transformation unless the VIEW-SPEC is
;;;  a view macro name)

(define (expand-glue-view-spec view-spec safe-types)
  (if (symbol? view-spec)
      (let ((a (assq view-spec (safe-type-views safe-types))))
	(if a
	    (cdr a)
	    (error "view macro `~a' undefined" view-spec)))
      view-spec))

(define (render-glue-decls port arg-infos safe-types)
  (for-each
   (lambda (arg-info)
     (if (vector-ref arg-info 1) ; type-name
	 (render-glue-decl port
			   (vector-ref arg-info 0) ; name
			   (cdr (get-glue-checker-spec
				 (vector-ref arg-info 1)
				 safe-types))
			   safe-types)))
   arg-infos))

;;;  emit the basic preamble which checks the arg count
;;;  and collects the #rest args

(define (render-glue-preamble port num-args rest-arg)
  (if rest-arg
      (begin
	(format port "  COUNT_ARGS_AT_LEAST(~d);\n" num-args)
	(if (symbol? rest-arg)
	    ;;
	    ;; collect 'em and put em in a var
	    ;; (only works for <10 fixed args)
	    ;;
	    (begin
	      (format port "  COLLECT~d();\n" num-args)
	      (format port "  ~a = REG~d;\n" rest-arg num-args))))
      (format port "  COUNT_ARGS(~d);\n" num-args)))

;;; emit the type checking and conversion preamble

(define (render-glue-checks-and-convs port arg-infos safe-types alloc-literal)
  (for-each
   (lambda (arg-info)
     (if (vector-ref arg-info 1) ;; type-name
	 (begin
	   (render-glue-type-check port arg-info safe-types alloc-literal)
	   (render-glue-conv port arg-info safe-types)
	   (newline port))))
   arg-infos))

(define (render-glue-type-check port arg-info safe-types alloc-literal)
  (let* ((name (vector-ref arg-info 0))
	 (type-name (vector-ref arg-info 1))
	 (raw-name (vector-ref arg-info 2))
	 (checker (car (get-glue-checker-spec type-name safe-types)))
	 (predicate (case (car checker)
		      ((primitive)
		       (if (null? (cdr checker))
			   #f ;; no checking to be done
			   (format #f "~a(~a)" (cadr checker) raw-name)))
		      ((instance?)
		       (format #f "instance_p(~a,TLREFB(~d))"
			       raw-name
			       (alloc-literal (cadr checker) :tlv)))
		      ;; this used to be called `class-eq?' -- we're
		      ;; changing it to `direct-instance', but support
		      ;; old code for now (CR 633)
		      ((direct-instance?
			class-eq?)
		       (format #f "OBJ_ISA_PTR_OF_CLASS(~a,TLREFB(~d))"
			       raw-name
			       (alloc-literal (cadr checker) :tlv)))
		      (else
		       (error "invalid RECOGNITION-STYLE: ~s" 
			      (car checker)))))
	 (checks-type (last checker)))
    ;;
    (if predicate
	(begin
	  (format port "  if (!~a)\n" predicate)
	  (format port "    {\n")
	  (format port "      obj c;\n")
	  (format port "      c = make5( TLREFB(~d),\n" 
		  (alloc-literal '<argument-type-error> :tlv))
	  (format port "                 NIL_OBJ,  /* properties */\n")
	  (format port "                 lookup_symbol( FUNCTION ),\n")
	  (format port "                 cons( ~a, NIL_OBJ ),\n" raw-name)
	  (format port "                 lookup_symbol( \"~a\" ),\n" name)
	  (format port "                 TLREFB(~d) );\n"
		  (alloc-literal checks-type :tlv))
	  (format port "      raise_error( c );\n")
	  (format port "    }\n")))))

(define (render-glue-conv port arg-info safe-types)
  (let* ((name (vector-ref arg-info 0))
	 (type-name (vector-ref arg-info 1))
	 (raw-name (vector-ref arg-info 2))
	 (view-specs (cdr (get-glue-checker-spec type-name safe-types))))
    (if (null? view-specs)
	(format port "  ~a = ~a;\n" name raw-name)
	(for-each (lambda (view-spec)
		    (let ((v (expand-glue-view-spec view-spec safe-types)))
		      (format port "  ~a = " (glue-view-name name v))
		      (format port (cadr v) raw-name)
		      (format port ";\n")))
		  view-specs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  here are the old remarks from the description
;;;  of `define-safe-glue'

#|
	       The "Safe Glue" Wrapper for define-glue
	       =======================================

   define-safe-glue is a rewriter-macro wrapper around define-glue
   that constructs argument checking and massaging code.  This
   is extremely useful in constructing library interfaces, because
   certain conversions are often made, and the sheer repetitive
   boredom of installing correct argument count and type checking
   makes it tempting to forgo, leading to less stable code.

   define-safe-glue is usable at two levels:

     (1) "use" level
     (2) "extension" level

   the "extension" level is used to extend the functionality of the
   basic define-safe-glue, using a wrapper rewriter that passes
   hidden args to the underlying define-safe-glue.

   Using define-safe-glue
   ======================

   While the system is rife with examples, particularly in the
   corelib and unixm modules, perhaps a few more formal comments
   are appropriate.

   the usage template is:

   (define-safe-glue (function [arg-form ...] [#rest [rest-arg]])
      [:template]
      [literals: (literal-expr ...)]
      [envt: ((literal-expr ...) ...)]
   {
     C body
   })

   where:

      arg-form ::= name
                |  (name type)

      literal-expr ::=  (& tlvarname)
                    |   'scheme-datum
                    |   compile-time-const-expr

   In an arg-form, type may be either a regular class name 
   or a special type name recognized by the safe-glue (the list
   of recognized types can be extended -- see "Extending define-safe-glue",
   below).

   The first form of arg-form creates a macro that refers to the
   apropriate VM register, as usual for define-glue (this is important
   because you sometimes have to be careful to not stomp on your
   inputs when you're constructing your outputs).

   The second form, (foo bar) causes the macro name of the register
   to be "foo_raw", and creates
   one or more C local variables to hold the validated type.  Which
   variables are created depends on the type.  If the type is not
   specially recognized, then there is one variable of type `obj' called
   "foo".

   By convention, for specially recognized types, if there is a single
   transformation, it will be to an appropriately typed variable with
   the "foo" name.

   If the type specifies the construction of more than one local variable,
   the variables will be named "foo_blech" where blech describes
   the transformation applied to "foo_obj" to get the variable.

   (None of the basic recognized types expand to more than one
    variable, but the x11 package has many examples)

   The basic recognized types are:

      type name         safety predicate       type of local var
      <raw-int>            fixnum?                  int
      <raw-ascii-char>     ascii-char?              UINT_8
      <raw-string>         string?                  char *

   The basic implementation also emits optimized type checking
   for the types <fixnum> and <string>

   If :template is specified, then the value of the 'function'
   variable is a <template> instead of a closure, and 'envt:' cannot
   be specified

   If #rest is specified, then the args are checked for a min
   value instead of an exact value.  If a name follows #rest, then
   the arguments are collected into a list in the usual fashion.
   Otherwise, the args are left alone (useful when it doesn't
   make sense to construct a list and then go cruising through
   it when the values can be accessed directly in the VM registers)

   Extending define-safe-glue
   ==========================

   In order to extend define-safe-glue, a rewriter is created
   which takes it's argument, adds a description of it's type
   extensions, and passes that to define-safe-glue.

   A template for doing this is:

	(define-rewriter (define-rstore-glue form)
	  (cons 'define-safe-glue
		(cons '(extend-known-types 
			    {type-extension ...}
			    {view-macro ...})
		      (cdr form))))

   define-safe-glue will recognize the extend-known-types argument
   and interpret it as an extention (note this makes it impossible
   to use define-safe-glue to create a function named extend-known-types)

   Type format for the type-extension is as follows:

	type-extension ::= (type-name recognition-style [view ...])
                        |  (view-macro-name var-decl-fmt
					    var-init-fmt
					    [local-name-fmt])

	recognition-style ::= (instance <foo-class>)
			   |  (class-eq? <foo-class>)
			   |  (primitive "OBJ_ISA_FOO_P" <foo-class>)

   the class-eq? recognition style requires an exact class match,
   and hence is faster but doesn't tolerate subclassing or non-PTR
   types.  the primitive recognition style can be used for 
   non-PTR types where subclasses are not permitted.

	view ::= (var-decl-fmt var-init-fmt [local-name-fmt])
              |  view-macro-name

   var-decl-fmt and var-init-fmt are format strings used to declare
   and initialize (respectively) the C variable that will hold the
   view of the argument's value.

   local-name-fmt is used to show how the base name is to be
   transformed into the view's destructured name.


   If there are no views given, then the variable name is a C macro
   for the input register.  Otherwise, as indicated above, the register
   is macro'd to "foo_raw", and each view creates and initializes
   a C local variable.

   Here are some complete examples of type-extensions from x11 package:

	an <X-font> is a gvec which contains in it's first slot
	a fixnum-looking pointer to the C XFontStruct.

	     (<X-font> 
	      (class-eq? <X-font>)
	      ("XFontStruct *~a"
	       "(XFontStruct *)OBJ_TO_RAW_PTR( gvec_read(~a,SLOT(0)) )"))

        An <X-window> (subclass of <X-drawable>) is a gvec which
        has a raw display pointer in slot 0 and an XID in slot 1.
        Because many objects have this basic structure, macros were
        created for these views.

             (<X-window> 
	      (class-eq? <X-window>) 
	      display/0 
	      xid/1)

        These are the actual macro definitions:

             (display/0
	      "Display *~a" 
	      "OBJ_TO_DISPLAY( gvec_read(~a,SLOT(0)) )"
	      "~a_dsp")

	     (xid/1 
	      "XID ~a"
	      "FX_TO_XID( gvec_read(~a,SLOT(1)) )"
	      "~a_xid")
|#

;;;(define known-types
    ;;
    ;; [0] recognized type name OR view name
    ;; [1] type-checking method ((primitive predicate required-class), 
    ;;                           (class-eq? class), or 
    ;;                           (instance? class))
    ;; [2...] "views"
    ;;		a view is a way of extracting C-style data from
    ;;		the raw (scheme) argument.  This includes the
    ;;		variable declaration, the extractor function, 
    ;;		and an option name rewriting.  All of these are
    ;;		'format' argument strings.
    ;;
