#|------------------------------------------------------------*-Scheme-*--|
 | File:    compiler/cgen/parts.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.10
 | File mod date:    2003-07-17 11:42:35
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  (rsc)
 |
 `------------------------------------------------------------------------|#


;; the link-name of a part is the name by which it's part
;; descriptor is known.  If a part is in module "foo"
;; and has link name "bar" (as may happen, e.g., if it comes from
;; a file "bar.scm"), then the part descriptor is named
;; "foo_part_bar"
;;
;; the file-name of a part is the file name (no extension, no path)
;; which will store a part

(define-class <part> (<object>)
    in-build-context
    link-name
    file-name
    src-file-label
    src-file
    (function-labels init-value: '())
    (code-in-part init-value: '#uninit); a <seq> of the pieces of code in here
    output-port
    part-tag                    ;; the tag in our part descriptor
    part-descriptor)            ;; used in <template>'s for linking

(define-method initialize ((self <part>))
    (set-code-in-part! self (make-seq))
    self)
    
(define (lexpath->basename (lex-path <list>))
    (let loop ((src lex-path) (dst '()))
	(if (null? src)
	    (apply* (cdr dst) string-append)
	    (loop (cdr src)
		  (cons "_"
			(cons (let ((s (car src)))
				(if (string? s)
				    s
				    (object->string s)))
			       dst))))))

(define (alloc-function-label (part <part>) (lex-path <list>))
    (let* ((basename (remove-specials (lexpath->basename lex-path)))
           (n (if (member basename (function-labels part))
		  (let loop ((i 1))
		    (let ((adjusted-name (format #f "~a~d" basename i)))
			(if (member adjusted-name (function-labels part))
			    (loop (+ i 1))
			    adjusted-name)))
		  basename)))
	(set-function-labels! part (cons n (function-labels part)))
	n))

;;; collect a list of (string valued) requirements

(define (collect-rqmts cds prop)
  (let ((tbl (make-string-table))
	(lst '()))
    (for-each
     (lambda (cd)
       (for-each
	(lambda (rqmt)
	  (if (not (table-lookup tbl rqmt))
	      (begin
		(table-insert! tbl rqmt #t)
		(set! lst (cons rqmt lst)))))
	(or (code-property cd prop) '())))
     cds)
    (reverse lst)))

;; in this compiler, we generate part tags whose low 10 bits
;; are the index of the part.  this will save linear lookups
;; during runtime binding/linking


(define (open-part (src-file <file-name>) cd-list)
  ;; include any additional libraries we want in the build context
  (include-other-libs (collect-rqmts cd-list 'other-libs))
  (include-other-c-files (collect-rqmts cd-list 'other-c-files))
  ;
  (let* ((basename (remove-specials (filename src-file)))
	 (p (make <part> 
		  in-build-context: *build-context*
		  link-name: (alloc-part-name *build-context* basename)
		  file-name: (alloc-file-name *build-context* basename "c")
		  src-file-label: basename
		  src-file: src-file
		  part-descriptor: '#uninit
		  output-port: '#uninit
		  part-tag: (+ (* 1024 (+ 1 (modulo (my-random) 500000)))
			       (length (parts *build-context*))))))
    (set-part-descriptor! p
			  (make <part-descr>
				module-name: (link-name *current-module*)
				part-tag: (part-tag p)
				linkage: #f))
    (set-parts! *build-context*
		(cons p
		      (parts *build-context*)))
    (set-needs-c-context! *build-context* #t)
    (set-c-files! *build-context*
		  (cons (file-name p)
			(c-files *build-context*)))
    (ensure-dest-dir *build-context*)
    (set-output-port! 
     p
     (open-output-file
      (pathname->string
       (make <file-name>
	     filename: (file-name p)
	     extension: "c"
	     file-directory: (dest-dir *build-context*)))))
    (let ((port (output-port p)))
      (display-disclaimer port)
      (center-* port "Preamble")
      (newline port)
      (format port 
	      "#define _MODULE_~a\n" 
	      (string-upcase (link-name *current-module*)))
      (format port 
	      "#define _SCM_~a\n" 
	      (string-upcase (src-file-label p)))
      (format port 
	      "#define _C_~a\n" 
	      (string-upcase (link-name p)))
      ;; cause the inclusion of the module private interface
      (format port
	      "#include \"~a\"\n"
	      (file-within-dir (private-interface-file *build-context*)))
      ;; use vinsns.h
      (format port
	      "#include <rscheme/vinsns.h>\n")
      ;; include any additional files according to any contained glue
      (for-each
       (lambda (hf)
	 (format port "#include ")
	 (if (char=? (string-ref hf 0) #\<)
	     (format port "~a" hf)
	     (format port "\"~a\"" hf))
	 (format port "\n"))
       (collect-rqmts cd-list 'other-h-files))
      ;; provide a forward declaration for the module descriptor
      (format port
	      "extern struct module_descr module_~a;\n"
	      (link-name *current-module*))
      ;; provide a forward declaration for the part descriptor
      (format port
	      "extern struct part_descr ~a_part_~a;\n"
	      (link-name *current-module*)
	      (link-name p))
      ;; provide sccs id string
      (format port "static char sccsid[] = ~s;\n"
	      (format #f "@(#)~a ~a [~d] (RS ~a)"
		      (name *build-context*)
		      src-file
		      (part-tag p)
		      *rscheme-build*))
      (newline port)
      (center-* port "Function Definitions")
      (newline port))
    p))

(define (close-part (part <part>))
    (center-* (output-port part) "Postamble")
    (let ((port (output-port part))
    	  (part-link-name (link-name part))
	  (module-link-name (link-name *current-module*)))
	(center-* port "Part Link Table")
	(display "\n\nstatic struct function_descr " port)
	(format port "*(part_~a_tab[]) = {\n" (link-name part))
	(for-each 
	    (lambda (cd)
		(format port "    &~a_descr,\n" (c-name cd)))
	    (seq->list (code-in-part part)))
	(display "    NULL };\n" port)
	(format port 
	    "struct part_descr ~a_part_~a = {\n"
	    module-link-name
	    part-link-name)
	;;
	;; part_descr.in_module
	;;
	(format port 
	    "    ~d,\n    &module_~a,\n"
	    (part-tag part)
	    module-link-name)
	;;
	;; part_descr.functions
	;;
	(format port "    part_~a_tab,\n" part-link-name )
	;;
	;; part_descr.name
	;;
	(format port "    \"~a\",\n" part-link-name)
	;; 
	;; part_descr.unswizzled_as
	;;
	(format port "    0, sccsid };\n")
	;;
	;; undefine #define's for module, part, and scm file
	;;
	(format port 
	    "#undef _MODULE_~a\n" 
	    (string-upcase module-link-name))
	(format port 
	    "#undef _SCM_~a\n" 
	    (string-upcase (src-file-label part)))
	(format port 
	    "#undef _C_~a\n" 
	    (string-upcase (link-name part))))
    (close-output-port (output-port part))
    (set-output-port! part #f))

(define (center-* port format-str . format-args)
  (let* ((line (apply format #f format-str format-args))
	 (*s (make-string (quotient (- 72 (string-length line)) 2)
			  #\*)))
    (format port "/~a ~a ~a/\n" *s line *s)))

;; a <code-descriptor> is used to store information about
;; code (a function) before it gets written to a part.

;; TEMPLATE is the <template> instance that corresponds to this
;; piece of code
;; FUNCTION-SCOPE is a <list> describing the
;; containing function relation
;; <string> & <symbol> elements denote lambda's for which a name is known
;; <integer> elements denote unnamed lambda's
;; <token> elements might be used later when we have a real compiler
;;
;;  CODE-PROPERTIES is a property list (ie, an a-list mapping symbols
;;  to things) that is intended to contain generally useful information
;;
;;  there should be a binding for 'function-scope which describes the
;;  lexical occurrence of this piece of code.  
;;	'function-scope  => <list>
;;  the first element of the list describes this particular function
;;  in relation to the next element
;;  the last element is describes the outer-most (top-level)
;;  containing function
;; for example, if the following occurred in a source file:
#|
    (define (foo x)
    	(cons (lambda () x)
	      (lambda () (lambda () x))))
|#
;; then there would be four <code-descriptor>'s:
;;	#1: function-scope = '("foo")
;;      #2: function-scope = '(0 "foo")
;;      #3: function-scope = '(0 "foo")
;;      #4: function-scope = '(0 0 "foo")
;; the compiler implements special-forms:
;; 	let-code-have-property
;; 	fluid-let-code-have-property
;; which can be used to install properties
;; (they also each have an "add-" form which causes the property
;;  value to be regarded as a list which is extended in that scope)
;; example:
#|
	(define-syntax (foo x)
	    (let-code-have-property ((alpha 100))
		(lambda 'hello () x)))

	(let-code-have-property ((alpha 200))
	    (fluid-let-code-have-property ((beta 300))
		(lambda 'main (y)
		    (cons y (foo y)))))

results in two <code-descriptors>
    the outer has:
	code-properties => ((function-scope main) 
			    (alpha . 200) 
			    (beta . 300))
    the inner has:
	code-properties => ((function-scope hello foo main)
			    (alpha . 100)
			    (beta . 300))
|#
		    
;; CODE is the AML program corresponding to the code
;;
;; STRATEGY is a symbol indicating what code generation
;; strategy is to be used.  It should be either bytecode, ccode, or literal-c

(define-class <code-descriptor> (<object>)
  template
  (code-properties init-value: '() type: <list>)
  (c-name init-value: #f)
  strategy
  code)

(define-generic-function function-scope)
(define-generic-function code-property)

;; return the VALUE of a property
;; if there is no binding, return #f

(define-method code-property ((self <code-descriptor>) (property <symbol>))
  (let ((b (assq property (code-properties self))))
    (if b
	(cdr b)
	#f)))

;; return the lexical path of the code
;; if no lexical path is known, call it "anon"

(define-method function-scope ((self <code-descriptor>))
    (or (code-property self 'function-scope)
        '("anon")))

(define (estimate-size (self <code-descriptor>))
    (case (strategy self)
	((bytecode) 0)
	((ccode) (count-vinsns (code self)))
	((literal-c) 
		(quotient (count-literal-bytes (vector-ref (code self) 1))
			  2))))

;; write out the code for a sequence of <code-descriptor>'s 
;; associated with a particular source file in a module.
;;
;; creates at least one <part>, possibly more, to hold the 
;; the <template>s' linkage pointers are set to 
;;
;; should only be called for code descriptors with the ccode strategy

(define (write-code-descriptors source-file cd-list)
    (if (pair? cd-list)
	(let loop ((size-so-far 0) 
		   (lst cd-list)
		   (part (open-part source-file cd-list)))
	    (if (null? lst)
		(close-part part)
		(let* ((cd (car lst))
		       (sz (estimate-size cd))
		       (nsz (+ size-so-far sz)))
		    (if (or (< nsz $max-size)
		    	    (eq? size-so-far 0))
			(begin
			    (if (>= nsz $max-size)
				(warning 
				    "Function in ~s is of size ~d (max is ~d)"
				    source-file 
				    sz
				    $max-size))
			    (write-into-part part cd)
			    (loop nsz (cdr lst) part))
			(begin
			    (close-part part)
			    (loop 0 lst (open-part source-file cd-list)))))))))


(define (write-into-part (p <part>) (cd <code-descriptor>))
    (set-code-pointer! (template cd)
		       (length (seq->list (code-in-part p))))
    (set-linkage-info! (template cd)
		       (part-descriptor p))
    ;;
    ;; for some reason, in 0.6 the code-properties are called `function-descr'
    ;; -- I should probably fix that
    ;;
    (set-function-descr! (template cd) (code-properties cd))
    (seq-add! (code-in-part p) cd)
    (set-c-name! cd (alloc-function-label p (function-scope cd)))
    (with-output-to-port
	(output-port p)
	(lambda ()
	    (case (strategy cd)
		((ccode)
		    (write-vinsns (last (function-scope cd))
				    (code cd)
				    (c-name cd)
				    (link-name p)))
		((literal-c)
		    (write-literal-c (last (function-scope cd))
		    		     (code cd)
				     (c-name cd)
				     (link-name p)))
		(else
		    (error/internal "bad time for strategy: ~s (~s)"
		    		    (strategy cd)
				    cd))))))

;; remove special characters
;;
;; used for converting scheme representations into those
;; palatable to the OS in the form of file names and palatable
;; to C in the form of identifiers

(define (remove-specials (str <string>))
    (letrec ((result (make-seq))
    	     (initial (lambda (s)
	     		(if (null? s)
			    "squiggle"
			    (if (valid-initial? (car s))
				(begin
				    (seq-add! result (car s))
				    (saw-valid (cdr s)))
				(initial (cdr s))))))
	     (saw-valid (lambda (s)
			    (if (null? s)
			    	(list->string (seq->list result))
				(if (valid-non-initial? (car s))
				    (begin
					(seq-add! result (car s))
					(saw-valid (cdr s)))
				    (saw-invalid (cdr s))))))
	     (saw-invalid (lambda (s)
			    (if (null? s)
			    	(list->string (seq->list result))
				(if (valid-non-initial? (car s))
				    (begin
					(seq-add! result #\_)
					(seq-add! result (car s))
					(saw-valid (cdr s)))
				    (saw-invalid (cdr s)))))))
    (initial (string->list str))))					

(define (valid-initial? ch)
    (char-alphabetic? ch))

(define (valid-non-initial? ch)
 (or (char-alphabetic? ch) 
     (char-numeric? ch)
     (eq? ch #\_)))

(define (string-upcase str)
    (list->string (map char-upcase (string->list str))))
    
(define (display-disclaimer p)
    (display "/**********************************************\n" p)
    (display "THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY\n" p)
    (display "BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER\n" p)
    (display "OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!\n" p)
    (format p "RScheme Build (~a)\n" *rscheme-build*)
    (display "**********************************************/\n\n" p))

(define (display-copy-disclaimer p file)
    (display "/**********************************************\n" p)
    (display "THIS FILE WAS AUTOMATICALLY COPIED FROM THE\n" p)
    (display "RSCHEME SOURCE TREE, AND THE ORIGINAL MAY CHANGE.\n" p)
    (display "HENCE, DO NOT MODIFY THIS FILE BY HAND!\n" p)
    (format p "RScheme Build (~a)\n" *rscheme-build*)
    (display "**********************************************/\n\n" p)
    (format p "#line 1 ~s\n" file))

(define (copy-source-file (path <file-name>) . opt)
  (let ((dest (make <file-name>
		    filename: (filename path)
		    extension: (extension path)
		    file-directory: (dest-dir *build-context*))))
    (ensure-dest-dir *build-context*)
    ;; if we have defined a *target-dest-dir* and 
    ;; the file is already there, then don't copy it
    (if (and *target-dest-dir* (file-exists? dest))
        (format #t "copy-source-file: ~a already present\n" dest)
        (if (path=? path dest)
            (format #t "copy-source-file: refuse to copy ~a onto itself\n" 
                    path)
            ;;
            ;; be careful to READ the file before writing the new
            ;; one, just in case (e.g., if the target is a soft
            ;; link to the source, not doing this would erase the
            ;; contents!  It's happened to me more than once)
            ;;
            (let ((src (file->string (pathname->string path))))
              (set-copied-files! *build-context*
                                 (cons (list (pathname->string 
                                              (append-path *process-dir*
                                                           path))
                                             (file-within-dir path))
                                       (copied-files *build-context*)))
              (call-with-output-file 
                  (pathname->string dest)
                (lambda (port)
                  (if (null? opt)
                      (display-copy-disclaimer port (pathname->string path))
                      (display (car opt) port))
                  (display src port))))))))

(define (bind-to-code module-name part-number function-number)
  (values function-number
	  (make <part-descr>
		module-name: module-name
		linkage: #f
		part-tag: part-number)))
  
