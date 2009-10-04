#|------------------------------------------------------------*-Scheme-*--|
 | File:	    rs/backend/c/parts.scm
 |
 |          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.2
 | File mod date:    2005-02-18 15:58:17
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  rs.backend.c
 |
 | Purpose:          Part management
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
  (properties type: <vector>)
  unit-name     ;; link name of module
  link-name
  ;
  (dest-file type: <file-name>)
  (src-file type: <file-name>)
  ;
  (function-labels init-value: '())
  (code-in-part init-value: '())
  output-port
  part-tag                    ;; the tag in our part descriptor
  part-descriptor)            ;; used in <template>'s for linking

(define (lexpath->basename (lex-path <list>))
  (let loop ((src lex-path) (dst '()))
    (if (null? src)
	(apply* (cdr dst) string-append)
	(loop (cdr src)
	      (cons "_"
		    (cons (let ((s (car src)))
			    (if (string? s)
				s
				(format #f "~a" s)))
			  dst))))))

(define (alloc-function-label (part <part>) (lex-path <list>))
  (let* ((basename (make-c-identifier (lexpath->basename lex-path)))
	 (n (if (member basename (function-labels part))
		(let loop ((i 1))
		  (let ((adjusted-name (format #f "~a~d" basename i)))
		    (if (member adjusted-name (function-labels part))
			(loop (+ i 1))
			adjusted-name)))
		basename)))
    (set-function-labels! part (cons n (function-labels part)))
    n))

;(define-class <part-descr> (<object>) image-mode: 3
;  module-name
;  part-tag
;  linkage)

;;;
;;;  these #define's provide a way for conditional
;;;  compilation at the C level according to the
;;;  module, source file, and C compilation unit (.c)
;;;
;;;  e.g., in module `foo.bar', the code generated from
;;;  file baz.scm, when generating a part `baz1', the
;;;  following CPP definitions are active:
;;;
;;;    #define _MODULE_FOO_BAR
;;;    #define _SCM_BAZ
;;;    #define _C_BAZ1
;;;

(define (c-define-for-module (part <part>))
  (string-upcase (unit-name part)))

(define (c-define-for-src (part <part>))
  (string-upcase (make-c-identifier (filename (src-file part)))))

(define (c-define-for-c (part <part>))
  (string-upcase (link-name part)))

;; in this compiler, we generate part tags whose low 10 bits
;; are the index of the part.  this will save linear lookups
;; during runtime binding/linking



(define (open-part (src-f <file-name>)
		   (dest-f <file-name>)
		   (unit-nm <string>)
		   (part-nm <string>)
		   (props <vector>))
  (let* ((tag (if (vassq 'tag props)
		  (vector-ref props (vassq 'tag props))
		  (+ 1000000 (random 1000000))))
	 (p (make <part>
		  properties: props
		  unit-name: unit-nm
		  link-name: part-nm
		  src-file: src-f
		  dest-file: dest-f
		  output-port: #f
		  part-tag: tag
		  part-descriptor: (make <part-descr>
					 module-name: unit-nm
					 part-tag: tag
					 linkage: #f))))
    ;; the output file is created lazily
    p))

(define (get-output-port (part <part>))
  (if (output-port? (output-port part))
      (output-port part)
      (if (eq? (output-port part) 'closed)
	  (error "~s: part already closed" part)
	  (let ((port (open-output-file
		       (pathname->string (dest-file part)))))
	    (set-output-port! part port)
	    (render-part-preamble part)
	    port))))

(define (close-part (part <part>))
  (if (output-port? (output-port part))
      (begin
	(render-part-postamble part)
	(close-output-port (output-port part))
	(set-output-port! part 'closed)))
  (reverse (code-in-part part)))

(define (render-part-postamble (part <part>))
  (let ((port (output-port part))
	(part-link-name (link-name part))
	(module-link-name (unit-name part)))
    (center-* port "Postamble")
    (center-* port "Part Link Table")
    (display "\n\nstatic struct function_descr " port)
    (format port "*(part_~a_tab[]) = {\n" (link-name part))
    (for-each
     (lambda (cd)
       (format port "    &~a_descr,\n" (c-name cd)))
     (reverse (code-in-part part)))
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
    (format port "    0 };\n")
    ;;
    ;; undefine #define's for module, part, and scm file
    ;;
    (format port "#undef _MODULE_~a\n" (c-define-for-module part))
    (format port "#undef _SCM_~a\n" (c-define-for-src part))
    (format port "#undef _C_~a\n" (c-define-for-c part))
    (newline port)))

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
  (properties init-value: '() type: <list>)
  (c-name init-value: #f)
  strategy  ;; we can handle in (union 'literal-c 'ccode)
  code)

(define-generic-function function-scope)

;; return the VALUE of a property
;; if there is no binding, return #f

(define-method code-property ((self <code-descriptor>) (property <symbol>))
  (get-property self property #f))

;; return the lexical path of the code
;; if no lexical path is known, call it "anon"

(define-method function-scope ((self <code-descriptor>))
  (or (code-property self 'function-scope)
      '("anon")))

(define (write-into-part (p <part>) (cd <code-descriptor>))
  ;;
  ;;  install the appropriate linkage info into the <template>
  ;;
  ;;  this amounts to setting:
  ;;
  ;;                  +----------------+
  ;;      code-pointer|       ENTRY #  |
  ;;                  +----------------+      <part-descr>
  ;;      linkage-info|             *--+---->+------------+
  ;;                  +----------------+     |            |
  ;;          . . .   :                :     |            |
  ;;                  :                :     +------------+
  ;;
  (set-code-pointer! (template cd) (length (code-in-part p)))
  (set-linkage-info! (template cd) (part-descriptor p))
  (set-code-in-part! p (cons cd (code-in-part p)))
  ;;
  (set-properties! (template cd) (properties cd))
  ;;
  (if (not (c-name cd))
      (set-c-name! cd (alloc-function-label p (function-scope cd))))
  ;;
  (with-output-to-port
      (get-output-port p)
    (lambda ()
      (case (strategy cd)
	((ccode)
	 (write-vinsns (last (function-scope cd))
		       (code cd)
		       (c-name cd)
		       (link-name p)
		       (unit-name p)))
	((literal-c)
	 (write-literal-c (last (function-scope cd))
			  (c-name cd)
			  (link-name p)
			  (unit-name p)
			  (vector-ref (code cd) 0)    ;; C arg aliases
			  (vector-ref (code cd) 1)))  ;; C monotones
	(else
	 (error "bad time for strategy: ~s (~s)" (strategy cd) cd))))))

(define (render-part-preamble p)
  (let ((port (output-port p)))
    (display-disclaimer p port)
    (center-* port "Preamble")
    (newline port)
    ;
    (format port "#define _MODULE_~a\n" (c-define-for-module p))
    (format port "#define _SCM_~a\n" (c-define-for-src p))
    (format port "#define _C_~a\n\n" (c-define-for-c p))
    ;; cause the inclusion of any necessary files
    (for-each
     (lambda ((incl <string>))
       (if (char=? (string-ref incl 0) #\<)
	   (format port "#include ~a\n" incl)
	   (format port "#include \"~a\"\n" incl)))
     (get-property p 'includes))
    ;; use vinsns.h
    (format port "\n#include <rscheme/vinsns.h>\n")
    ;; provide a forward declaration for the module descriptor
    (format port "extern struct module_descr module_~a;\n" (unit-name p))
    ;; provide a forward declaration for the part descriptor
    (format port
	    "extern struct part_descr ~a_part_~a;\n"
	    (unit-name p)
	    (link-name p))
    ;; provide sccs id string
    (let ((sccs-id (get-property p 'sccs-id #f)))
      (if sccs-id
	  (format port "\nstatic char sccsid[] = ~s;\n"
		  (string-append "@(#)" sccs-id))))
    (newline port)
    (center-* port "Function Definitions")
    (newline port)))
