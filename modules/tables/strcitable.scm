#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/tables/strcitable.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.6
 | File mod date:    1997-11-29 23:10:34
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  tables
 |
 | Purpose:          string (case insensitive) table
 `------------------------------------------------------------------------|#

(define-class <string-ci-table> (<hash-table>))

(define-method table-key-present? ((self <string-ci-table>) (key <string>))
  (string-ci-table-probe? self (string-ci->hash key) key))

(define-method table-lookup ((self <string-ci-table>) (key <string>))
  (string-ci-table-lookup self (string-ci->hash key) key))

(define-method table-remove! ((self <string-ci-table>) (key <string>))
  (string-ci-table-remove! self (string-ci->hash key) key))

(define-method table-insert! ((self <string-ci-table>) (key <string>) value)
  (string-ci-table-insert! self (string-ci->hash key) key value))

;; introspection

(define-method table-hash-function ((self <string-ci-table>))
  string-ci->hash)
    
(define-method table-equal-function ((self <string-ci-table>))
  string-ci=?)

(define (get-system-symbol-table)
  (rscheme-global-ref 2))
