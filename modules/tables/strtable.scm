#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/tables/strtable.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    1997-11-29 23:10:34
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  tables
 |
 | Purpose:          string (case-sensitive) tables
 `------------------------------------------------------------------------|#

(define-class <string-table> (<hash-table>))

(define-method table-key-present? ((self <string-table>) (key <string>))
  (string-table-probe? self (string->hash key) key))

(define-method table-lookup ((self <string-table>) (key <string>))
  (string-table-lookup self (string->hash key) key))

(define-method table-remove! ((self <string-table>) (key <string>))
  (string-table-remove! self (string->hash key) key))

(define-method table-insert! ((self <string-table>) (key <string>) value)
  (string-table-insert! self (string->hash key) key value))

;; introspection

(define-method table-hash-function ((self <string-table>))
    string->hash)
    
(define-method table-equal-function ((self <string-table>))
    string=?)
