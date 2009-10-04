#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/boot/makeinit.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.18
 | File mod date:    1999-02-12 09:33:49
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#


;;
;;  make a bootable image from a fully-bound module
;;
;;  the exports of the module should be:
;;      start      		==>  the entry point function
;;      *version*		==>  a TLV containing the version string
;;      *rscheme-version-list*  ==>  a TLV containing the version list
;;	exception-handler	==>  the exception handler
;;	interrupt-dispatch	==>  the interrupt dispatcher
;;	continue-intr-tmpl	==>  the interrupt continuation template
;;	<<class>>		==>  a TLV containing the <<class>> class
;;	<pair>			==>  a TLV containing the <pair> class
;;	<vector>		==>  a TLV containing the <vector> class
;;	  etc.
;;
;; the other RScheme globals are set as follows:
;;
;;	*symbol-table*  ==>     rebuilt from all the symbols
;;				present in the resulting image
;;	*builtin-modules* ==>	an alist mapping names of the builtin
;;				modules to simple <module>'s with their
;;				exports-table
;;

(load "replace.scm")
(load "handcraft.scm")

;;
;; scan the $DIST/runtime/scheme.h for the globals
;;
(define $rscheme-globals
  ;; global-name             C name                name in LOADed image
  ;; -----------             ------                --------------------
  '((*boot-image*            "boot_image")
    (*boot-args*	     "boot_args")
    (*symbol-table*	     "symbol_table")
    (*interrupt-handlers*    "interrupt_handlers")
    (exception-handler	     "exception_handler")
    (<dequeue>	             "dequeue_class")
    (continue-intr-template  "continue_intr_tmpl")

    (<<class>>               "class_class"         "<<class>>" 0) ;; [7]

    (<pair> 	             "pair_class"          "<pair>" 0)    ;; [8]
    (<vector>		     "vector_class"        "<vector>" 0)
    (<string>		     "string_class"        "<string>" 1)
    (<symbol>		     "symbol_class"        "<symbol>" 0)

    (<closure>               "closure_class"       "<closure>" 0)       ;; [12]
    (<template> 	     "template_class"      "<template>" 0)
    (<partial-continuation>  "partcont_class")
    (<binding-envt>	     "binding_envt_class"  "<binding-envt>" 0)

    (<top-level-var>         "tlv_class"           "<top-level-var>" 0) ;; [16]
    (<byte-vector>	     "byte_vector_class")

    (<boolean>		     "boolean_class")
    (<empty-list>	     "nil_class")
    (<ascii-char>	     "ascii_char_class")
    (<unicode-char>	     "unicode_char_class")
    (<unique-obj>	     "unique_obj_class")
    (<fixnum>		     "fixnum_class")

    (<double-float>          "double_float_class"   "<double-float>" 1) ;; [24]
    (<function>              "function_class"       "<function>" 0)     ;; [25]

    (<spare-1>               "spare_1_class")
    (<spare-2>               "spare_2_class")
    (<spare-3>               "spare_3_class")
    (*install-dir*           "install_dir")               ;; [29]
    (<allocation-area>       "allocation_area_class")
    (*default-allocation-area* "default_alloc_area")
    (<long-int> "long_int_class")
    (*finalizing-list*         "finalizing_list")
    (<unicode-string> "unicode_string_class")           ;; [34]
    (<os-error>       "os_error_class") ;; 35
    (<condition>      "condition_class") ;; 36
    (<type-check-failed>   "type_check_failed_class") ;; 37
    (<function-place>      "function_place_class")    ;; 38
    (return-from-call-scheme "return_from_call_scheme_template") ;; 39
    (*c-signal-names*  "c_signal_names") ;; 40 -- filled in at runtime init
    (load-cache-and-call "load_cache_and_call_proc")
    (generic-function-dispatch "gf_dispatch_template")
    (<mp-data>  "mp_data_class")
    (<rect-complex>  "rect_complex_class")
    (<mp-rational> "mp_rational_class")
    (<bignum>   "bignum_class")   ;; 46
    (<condition-stack> "condition_stack_class")  ; 47
    ((& *capture-stack-on-conditions*) "capture_stack_var") ; 48
))

(define-class <stub-gvec> (<object>))
(define-class <stub-bvec> (<object>) :bvec image-mode: 1)


;;
;;  compute the bindings necessary to close the universe on a
;;  <module>'s entry point
;;
;;  this function relies on the fact that the entry-point
;;  will only have pointers to things defined in *mifio-defs*

(define (show-reachable entry-point)
  (let ((stop (map cdr *mifio-defs*))
	(replace (make-object-table)))
    (for-each-reachable entry-point
			stop
			replace
			(lambda (item)
			  (format #t "item => ~#*60s\n" item)
			  (not (symbol? item))))))

(define (compute-closers (module <module>) entry-point)
  (let ((replace (make-object-table)))
    (for-each 
     (lambda (def)
       (let* ((xclass (cdr def))
	      (n (string->symbol (car def))))
	 ;;
	 (let ((b (table-lookup (module-exports module) n)))
	   (if (and b (not (eq? (value b) '#unbound)))
	       (table-insert! replace xclass (value b))
	       (begin
		 (format #t "closer: missing binding for: ~a\n" n)
		 (table-insert! replace 
				xclass
				(if (eq? (gvec-ref xclass 1) 0)
				    <stub-gvec>
				    <stub-bvec>)))))))
     *mifio-defs*)
    replace))


(define (exported-binding (m <module>) (n <symbol>))
  (let ((b (table-lookup (module-exports m) n)))
    (if b
	(if (instance? b <top-level-var>)
	    b
	    (begin
	      (format #t "binding for ~s is ~s, not a <top-level-var>\n" 
		      n b)
	      #f))
	(begin
	  (format #t "missing binding for ~s\n" n)
	  #f))))

(define (set-created-value (m <module>) (n <symbol>) v)
  (let ((b (exported-binding m n)))
    (if b
	(set-value! b v))))

;;
;;  build the boot_vec
;;
;;  the boot vector is a <vector> of 3 values:
;;    [0]  ==> initial global values
;;    [1]  ==> version string
;;    [2]  ==> entry point (start)
;;    [3]  ==> initialization thunks
;;    [4]  ==> main function (main)
;;    [5]  ==> patch time thunks

(define (build-boot-vector (m <module>))
  ;;
  (define (exported-value n)
    (let ((b (exported-binding m n)))
      (and b (value b))))
  ;;
  (vector #| 0 |# (list->vector
		   (map (lambda (global-info)
			  (let ((n (car global-info)))
			    (case n
			      ((*boot-image*) #f)   ;; / these are filled
			      ((*boot-args*) #f)    ;; | in at runtime
			      ((*install-dir*) #f)  ;; \ 
			      ((*symbol-table*) #f) ;; we'll fill this in later
			      (else
			       (if (pair? n)
				   (case (car n)
				     ((&)
				      (exported-binding m (cadr n))))
				   (exported-value n))))))
			$rscheme-globals))
	  #| 1 |# #f ;; filled in later... (exported-value '*version*)
	  #| 2 |# (exported-value 'start)
	  #| 3 |# (init-thunks m)
	  #| 4 |# (exported-value 'main)
	  #| 5 |# (first-init-thunks m)))

(define (build-symbol-table! (v <vector>) rplc)
  (vector-set! (vector-ref v 0)
	       2 
	       (rebuild-symbol-table v '() rplc)))

(define (build-builtin-modules! (v <vector>) bootm)
  (let ((space (map (lambda (im)
		      (cons (name im) (actual-module im)))
		    (module-imports bootm))))
    ;;
    (define (exported-value n)
      (let ((b (exported-binding bootm n)))
	(and b (value b))))
    ;;
    ;;  create a value for *version* which is the value of *program*
    ;;  followed by build name
    ;;
    (set-created-value bootm
		       '*version*
		       (format #f "~a (~a)" 
			       (exported-value '*program*)
			       *rscheme-build*))
    ;;
    ;;  create a value for *rscheme-version-list* which is a more structured
    ;;  (ie, machine-readable) version list, e.g., (0 7 3 1 30) for 0.7-3.1b30
    ;;
    (set-created-value bootm '*rscheme-version-list* *rscheme-build-vlist*)
    ;;
    ;;  create a value for *boot-modules*
    ;;  which is an assoc list of all the modules in this image
    ;;
    (set-created-value bootm '*boot-modules* space)
    ;;
    ;; delete information in source modules that is being
    ;; linked away
    ;;
    (for-each (lambda (p)
		(let ((m (cdr p)))
		  (for-each (lambda (im)
			      ; keep the <imported-module> around to
			      ; support module backings to top-level-contours,
			      ; but we don't need its link-commands any more
			      (set-link-commands! im '()))
			    (module-imports m))
		  (set-module-classes! m '())
		  (set-module-generic-functions! m '())
		  (set-module-implicit-methods! m '())
		  (set-init-thunks! m '())
		  (set-first-init-thunks! m '())))
	      space)))

(define (target-top-level space m-name)
  (let ((mb (assq m-name space)))
    (if mb
	(top-level-envt (cdr mb))
	#f)))

(define (table-of-exports space m-name)
  (format #t "   a builtin module is: ~s" m-name)
  (let ((mb (assq m-name space)))
    (if mb
	(let ((m (cdr mb))
	      (t (make-table eq? symbol->hash))
	      (n 0))
	  (table-for-each
	   (module-exports m)
	   (lambda (h k v)
	     (set! n (+ n 1))
	     (table-insert! t k v)))
	  (format #t "   -- ~d exported bindings\n" n)
	  t)
	(begin
	  (format #t "   -- missing! (supplying empty table)\n")
	  (make-table eq? symbol->hash)))))

;; 
;;  load the module that is to be made bootable
;;
(define (load-boot-module)
  (load-module 
   (pathname->string 
    (locate-dist-resource "modules/rscheme.mif"))))

