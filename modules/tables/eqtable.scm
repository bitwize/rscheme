#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/tables/eqtable.scm
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
 | Purpose:          hash tables for things using eq? as the comparison function
 `------------------------------------------------------------------------|#

(define-class <eq-table> (<hash-table>)
  (table-hash-function type: <function>))

(define-method table-key-present? ((self <eq-table>) key)
  (object-table-probe? self ((table-hash-function self) key) key))

(define-method table-lookup ((self <eq-table>) key)
  (object-table-lookup self ((table-hash-function self) key) key))

(define-method table-remove! ((self <eq-table>) key)
  (object-table-remove! self ((table-hash-function self) key) key))

(define-method table-insert! ((self <eq-table>) key value)
  (object-table-insert! self ((table-hash-function self) key) key value))

;; introspection

(define-method table-equal-function ((self <eq-table>))
    eq?)

;; tables of integers with NO hashing

(define-class <integer-table> (<hash-table>))

(define-method table-key-present? ((self <integer-table>) (key <fixnum>))
  (object-table-probe? self key key))

(define-method table-lookup ((self <integer-table>) (key <fixnum>))
  (object-table-lookup self key key))

(define-method table-remove! ((self <integer-table>) (key <fixnum>))
  (object-table-remove! self key key))

(define-method table-insert! ((self <integer-table>) (key <fixnum>) value)
  (object-table-insert! self key key value))

;; introspection

(define-method table-equal-function ((self <integer-table>))
  eq?)

(define-method table-hash-function ((self <integer-table>))
  identity)


;; tables of integers WITH hashing

(define-class <hash-integer-table> (<hash-table>))

(define-method table-key-present? ((self <hash-integer-table>) (key <fixnum>))
    (object-table-probe? self (integer->hash key) key))

(define-method table-lookup ((self <hash-integer-table>) (key <fixnum>))
    (object-table-lookup self (integer->hash key) key))

(define-method table-remove! ((self <hash-integer-table>) (key <fixnum>))
    (object-table-remove! self (integer->hash key) key))

(define-method table-insert! ((self <hash-integer-table>) (key <fixnum>) value)
    (object-table-insert! self (integer->hash key) key value))

;; introspection

(define-method table-equal-function ((self <hash-integer-table>))
  eq?)

(define-method table-hash-function ((self <hash-integer-table>))
  integer->hash)
