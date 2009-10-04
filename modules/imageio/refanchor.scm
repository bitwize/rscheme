#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/imageio/refanchor.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.6
 | File mod date:    2007-01-28 10:02:16
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  imageio
 |
 `------------------------------------------------------------------------|#

(define-class <anchor> (<object>) :abstract)

(define-class <fn-descr-anchor> (<anchor>)
  module-name
  part-number
  function-number
  (code-ptrs init-value: '()))

(define-class <code-ptr-anchor> (<anchor>)
  fn-descr
  (monotone-number init-value: 0))

(define-method write-object ((self <fn-descr-anchor>) port)
  (format port "#[<fn-descr> ~a/~d.~d[~d]]"
	  (module-name self)
	  (logical-shift-right (part-number self) 10)
	  (bitwise-and (part-number self) #x3FF)
	  (function-number self)))

(define-method write-object ((self <code-ptr-anchor>) port)
  (let ((p (fn-descr self)))
    (format port "#[<code-ptr> ~a/~d.~d[~d] [~s]]"
	    (module-name p)
	    (logical-shift-right (part-number p) 10)
	    (bitwise-and (part-number p) #x3FF)
	    (function-number p)
	    (if (eq? (monotone-number self) 0)
		'entry
		(monotone-number self)))))

;; dictionaries used to compress class names and symbol tables
;;
;; note that using a dictionary for compressing class names
;; neither obligates the loader to provide the named class, nor
;; alleviates the need to supply the actual class in the classtable

(define-class <symbol-dict> (<object>)
  id->symbol-vector
  (symbol->id-table init-value: #f))

(define (symbol-dict-id-table (dict <symbol-dict>))
  (or (symbol->id-table dict)
      (let (((v <vector>) (id->symbol-vector dict))
	    (tbl (make-symbol-table))
	    ((n <fixnum>) (vector-length (id->symbol-vector dict))))
	(set-symbol->id-table! dict tbl)
	(let loop (((i <fixnum>) 0))
	  (if (eq? i n)
	      tbl
	      (begin
		(table-insert! tbl (vector-ref v i) i)
		(loop (fixnum+ i 1))))))))


;; a dictionary of common symbols

;; use *empty-dict* if there isn't a dict

(%early-once-only

(define *empty-dict*
  (make <symbol-dict>
	id->symbol-vector: '#()))

(define *standard-symbol-dict* 
  (make <symbol-dict>
	id->symbol-vector: '#(cons
			      x y z i j k a b c foo bar
			      self port
			      eq? equal?)))
  
;; a dictionary of common class names

(define *standard-class-dict* 
  (make <symbol-dict>
	id->symbol-vector: '#(;; Section I.  standard scheme classes
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
			      <link-value> <link-xform>)))
)
#|
(define *big-class-dict*
  (make <symbol-dict>
	id->symbol-vector: '#(<<class>> <<standard-class>> <DBM-table>
  <anchor> <ascii-char> <binding-envt> <binding> <boolean>
  <bounded-string-output-port> <bvec> <byte-coded> <byte-vector>
  <char> <closure> <cmd-loop> <code-ptr-anchor> <collection> <complex>
  <definer> <directory-name> <double-float> <edit-input-port>
  <empty-list> <eof> <eq-table> <expr-icode> <fd-select-set>
  <file-name> <fixnum> <float> <fn-descr-anchor> <function>
  <generic-function> <generic-table> <gvec> <hash-integer-table>
  <hash-table> <ic-bind> <ic-call-prim> <ic-call> <ic-const> <ic-if>
  <ic-jump> <ic-lambda> <ic-lex-ref> <ic-lex-set> <ic-loop> <ic-multi>
  <ic-prim-expr> <ic-procedure> <ic-root-ref> <ic-root-set> <ic-seq>
  <ic-tl-ref> <ic-tl-set> <icode> <imported-binding> <imported-module>
  <input-pipe-port> <input-port> <integer-table> <integer> <interval>
  <lexical-contour> <lexical-var> <link-bdgs> <link-cmd> <link-method>
  <link-value> <link-xform> <list> <loop-var> <macro-form> <macro>
  <method> <module> <number> <object-table> <object>
  <output-pipe-port> <output-port> <pair> <part-descr>
  <partial-continuation> <patch> <primop> <promise> <random-state>
  <rational> <real> <rewriter> <root-dir> <scope-record> <seq>
  <sequence> <slot-descriptor> <spare-1> <spare-2> <spare-3>
  <special-form> <stat-buf> <std-input-port> <std-output-port>
  <string-ci-table> <string-input-port> <string-output-port>
  <string-table> <string> <substitution> <symbol-table> <symbol>
  <table-bucket> <table> <template> <time> <token> <top-level-contour>
  <top-level-var> <unicode-char> <unique-obj> <vector>
  <winding-contour> <winding-protect>)))
|#

