#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/with.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.2
 | File mod date:    1997-11-29 23:10:36
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 | Purpose:          Implement local module scopes
 `------------------------------------------------------------------------|#

(define-class <with-envt> (<scope-record>)
  table
  lexical-enclosing
  dynamic-enclosing)

(define-method lookup ((self <with-envt>) (name <symbol>))
  (or (table-lookup (table self) name)
      (lookup (lexical-enclosing self) name)))

(define (compile/with-module sf form lxe dye mode)
  (let ((e (make <with-envt>
                 table: (module-exports (get-module (cadr form)))
                 lexical-enclosing: lxe
                 dynamic-enclosing: dye)))
    (compile/body (cddr form) e e mode)))
