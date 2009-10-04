#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/codegen/aml2tmpl.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.10
 | File mod date:    1999-01-27 22:55:28
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose:          aml->template function
 `------------------------------------------------------------------------|#

(%strategy bytecode
 (define (bc-template-prototype z) z))

(%early-once-only
 (define *bc-template-prototype* (template bc-template-prototype)))

(define (bytecode-aml->template asm code-ctx)
  (make-gvec* <template>
	      (gvec-ref *bc-template-prototype* 0)
	      (gvec-ref *bc-template-prototype* 1)
	      (code-ctx-properties code-ctx)
	      (aml->byte-coded asm)
	      (code-ctx-literals code-ctx)))

(define (default-aml->template asm code-ctx)
  ;; note that `glue' does not come through this path,
  ;; so the default thing to do can be to just do bytecodes
  (bytecode-aml->template asm code-ctx))

(%early-once-only
 (define *codegen-implementations*
  (list (cons 'default default-aml->template)
	(cons 'bytecode bytecode-aml->template))))

(define (add-codegen-implementation! name proc)
  (set! *codegen-implementations* 
	(cons (cons name proc)
	      *codegen-implementations*)))

(define *global-codegen-strategy* 'default)

(define (choose-strategy cc)
  (let ((a (assq 'strategy (code-ctx-properties cc))))
    (if a
	(cadr a)
	*global-codegen-strategy*)))

(define (aml->template asm code-ctx)
  (let ((op (assq (choose-strategy code-ctx) *codegen-implementations*)))
    ((if op (cdr op) default-aml->template) asm code-ctx)))

(mifio-class "<byte-coded>" <byte-coded>)
