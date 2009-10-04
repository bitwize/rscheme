#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/tables/classes.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    1999-01-10 01:37:10
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  tables
 |
 | Purpose:          table classes
 `------------------------------------------------------------------------|#

;; introspection

(define-generic-function table-hash-function)
(define-generic-function table-equal-function)

(define-generic-function table-size)
;(define-generic-function table-keys->list)
;(define-generic-function table-values->list)

;;

(define-class <table-bucket> (<object>)
    bucket-bits
    bucket-overflow)

;; there may be other kinds of tables, like explicit alists
;; or even DBM covers (which really are hash tables, I think,
;; but in a vendor library)
;;
;;  a <table> maps keys onto values (with explicit keys)

(define-class <table> (<collection>) :abstract)

(define-class <hash-table> (<table>)
    (directory init-keyword: #f)
    (directory-bits init-value: 4)
    (table-size init-value: 0 init-keyword: #f)
    (bucket-class init-value: #f))

;; operations

(define-generic-function table-lookup)
(define-generic-function table-insert!)
(define-generic-function table-remove!)
(define-generic-function table-key-present?)

(define-generic-function value-sequence)
(define-generic-function key-sequence)		;; DIRM p.129
#|
(define-generic-function element)		;; DIRM p.128
|#

(define-generic-function table-for-each)
