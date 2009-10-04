#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/imageio/schema.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.4
 | File mod date:    2007-01-28 10:02:16
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  imageio
 |
 `------------------------------------------------------------------------|#

(define-class <image-schema> (<object>)
  class-table   ;; maps class names (symbols) to class objects
  class-dict    ;; provides short names (ids) for class symbols
  symbol-dict   ;; provides short names for other symbols
  link-in?      ;; swizzle code pointers? (#t => yes)
  ref-proc)     ;; proc to handle other references

(define *default-image-schema* #f)

(define (default-image-schema)
  (if (not *default-image-schema*)
      (set! *default-image-schema* (make-default-image-schema)))
  *default-image-schema*)

;;

(define (other-refs-are-invalid others)
  (if (null? others)
      (values '() '())
      (error "object->image: don't know how to pickle ~d references: ~s"
	     (length others)
	     others)))

;;

(define (make-default-image-schema)
  (let ((class-tbl (make-symbol-table)))
    ;;
    (letrec-syntax ((class-list (syntax-form ())
				(syntax-form (c . more)
				  (table-insert! class-tbl (class-name c) c)
				  (class-list . more))))
      (class-list <bounded-string-output-port> <output-pipe-port>
		  <string-input-port> <eof> <output-port> <std-output-port>
		  <std-input-port> <input-port> <curly-braced>
		  <string-output-port> <input-pipe-port> <output-filter>
		  <excess-initializers> <random-state> <integer-table>
		  <symbol-table> <hash-table> <eq-table> <table>
		  <hash-integer-table> <string-table> <string-ci-table>
		  <generic-table> <table-bucket> <object-table>
		  <pair> <<class>> <sequence> <template>
		  <float> <rewriter> <fixnum> <unique-obj>
		  <partial-continuation> <top-level-contour> <bvec>
		  <<standard-class>> <spare-1> <real> <macro> <complex> <list>
		  <spare-2> <generic-function> <spare-3> <macro-form>
		  <binding-envt> <byte-vector> <top-level-var> <binding>
		  <allocation-area> <number> <method> <scope-record> <integer>
		  <symbol> <winding-protect> <winding-contour> <ascii-char>
		  <unicode-char> <object> <rational> <vector> <closure>
		  <long-int> <boolean> <collection> <gvec> <double-float>
		  <condition> <slot-descriptor> <char> <function> <string>
		  <lexical-contour> <empty-list> <byte-coded>))
    ;;
    (make <image-schema>
	  class-table: class-tbl
	  class-dict: (make <symbol-dict>
			    id->symbol-vector: $standard-scheme-class-names)
	  symbol-dict: (make <symbol-dict>
			     id->symbol-vector: $common-symbols)
	  link-in?: #t
	  ref-proc: other-refs-are-invalid)))

(define $standard-scheme-class-names
  '#(;; Section I.  standard scheme classes
     <pair> <vector> <string> <function>
     <number>
     ;;
     ;; Section II. standard implementation
     ;;
     <template> <double-float> <closure>
     ;;
     ;; Section III. basic RScheme
     ;;
     <file-name> <directory-name>
     <table>
     <symbol-table> <string-table> <string-ci-table>
     <integer-table> <eq-table> <generic-table>
     <table-bucket>
     <byte-vector>
     ;;
     ;; Section IV. Object system
     ;;
     <object>
     <<standard-class>>
     <slot-descriptor>
     <generic-function> <method>
     ;;
     ;; Section V. Module system
     ;;
     <module>
     <top-level-envt>
     <imported-module> <imported-binding>
     <link-bdgs> <link-cmd> <link-method>
     <link-value> <link-xform>))

(define $common-symbols
  '#(cons self port eq? car cdr make
	  required optional prohibited))

;;
;;  typical extensions to schemas
;;

(define (add-class-anchors-to-schema! (schema <image-schema>) . classes)
  (let ((class-tbl (class-table schema))
	(vec (list->vector classes))
	(dict (class-dict schema)))
    ;;
    (for-each
     (lambda (c)
       (table-insert! class-tbl (class-name c) c))
     classes)
    ;;
    (let* ((n (- 256 (vector-length (id->symbol-vector dict))))
	   (m (vector-length vec))
	   (k (min n m)))
      (if (> k 0)
	  (let ((x (subvector (vector-map class-name vec) 0 k)))
	    (set-symbol->id-table! dict #f)
	    (set-id->symbol-vector! 
	     dict
	     (vector-append (id->symbol-vector dict) x))))
      schema)))

;;; return the inverse mapping table

(define (class-name-table (self <image-schema>))
  (let ((tbl (make-object-table)))
    (table-for-each
     (class-table self)
     (lambda (h k v)
       (table-insert! tbl v k)))
    tbl))


