#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/classes.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.17
 | File mod date:    2003-11-04 15:40:56
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 | Purpose:          compile-time classes
 `------------------------------------------------------------------------|#

(define-generic-function lookup)
(define-generic-function bind!)

;---

(define-class <icode> (<object>)
  (does-lambda? init-keyword: #f
		init-value: #f)
  (does-save? init-keyword: #f
	      init-value: #f)
  (vars-referenced init-value: '())
  (vars-captured init-value: '()))

(define-class <expr-icode> (<icode>)
  (return-types init-value: '(<obj>))
  (mode init-value: 'value))

(define-class <ic-multi> (<expr-icode>)
    arg-list)

(define-class <ic-call> (<expr-icode>)
  (function type: <icode>)
  (args type: <ic-multi>))		;; an <ic-multi>

;;; an <ic-cast> is a claim (a la trust-me) about the actual types
;;; of it's argument (which is a single expr)

(define-class <ic-cast> (<expr-icode>)
  (expr type: <icode>))

(define-class <ic-call-prim> (<expr-icode>)
  function
  (args type: <ic-multi>))

(define-class <ic-bind> (<expr-icode>)
  envt
  inits		; #f => bind arguments!
  (body type: <icode>)
  (num-args init-keyword: #f)  ;; filled in by initializer
  rest?)

(define-class <ic-tl-ref> (<expr-icode>)
  var)

(define-class <ic-lex-ref> (<expr-icode>)
  var)
    
(define-class <ic-tl-set> (<expr-icode>)
  var
  rhs)

(define-class <ic-lex-set> (<expr-icode>)
  var
  (rhs type: <icode>))

(define-class <ic-if> (<expr-icode>)
  (condition type: <icode>)
  (if-true type: <icode>)
  (if-false type: <icode>))

(define-class <ic-const> (<expr-icode>)
  value)

(define-class <ic-procedure> (<icode>)
  name
  (body type: <icode>))

(define-class <ic-lambda> (<expr-icode>)
  proc
  (code-properties init-value: '()))

(define-class <ic-seq> (<expr-icode>)
  stmt-list)

(define-class <ic-loop> (<expr-icode>)
  inits  ;; normally <icode>, but set to #f during loop AML generation!
  (body type: <icode>)
  envt
  loop-var
  (first-arg-reg init-value: 0)
  (ct-envt init-value: #f))

(define-class <ic-jump> (<icode>)
  (args type: <icode>)
  loop-var)

(define-generic-function compile-head)
(define-generic-function compile-ref)
(define-generic-function compile-set)

;(define-generic-function set-write-prot!)
;(define-generic-function write-prot)
;(define-generic-function table)
;(define-generic-function set-table!)

(define-class <special-form> (<binding>)
    compiler-description
    (compiler-proc init-value: #f);; cached translation of compiler-description
    (syntax-checker init-value: #f))

(mifio-class "<special-form>" <special-form>)

(define (special-form-compiler (self <special-form>))
  (let ((p (compiler-proc self)))
    (if p
	p
	(begin
	  (set! p (special-form-compiler->proc (compiler-description self)))
	  (set-compiler-proc! self p)
	  p))))

(define-class <lexical-var> (<binding>)
    (envt init-keyword: #f)  ;; this is the envt the var LIVES IN
    type
    (trust-me-type? init-value: #f)
    (ever-set? init-value: #f))

(define-class <loop-var> (<binding>)
    label
    compile-with-full-semantics
    loop-icode
    in-procedure)

(define-class <substitution-site-record> (<scope-record>)
  lexical-enclosing
  dynamic-enclosing
  some-info)
