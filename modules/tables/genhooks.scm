#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/tables/genhooks.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    1997-11-29 23:10:33
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  tables
 |
 | Purpose:          generic-table hooks
 `------------------------------------------------------------------------|#

(define generic-hash-table-lookup
  (well-known-function
   code: ("RScheme" 9502 0)))

(define generic-hash-table-remove!
  (well-known-function
   code: ("RScheme" 9502 1)))

(define generic-hash-table-insert!
  (well-known-function
   code: ("RScheme" 9502 2)))

(define generic-hash-table-probe?
  (well-known-function
   code: ("RScheme" 9502 3)))
