#|------------------------------------------------------------*-Scheme-*--|
 | File:    test/db.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.2
 | File mod date:    1997-11-29 23:10:42
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#

,(use db tables)

(define x (make-db-table "/etc/passwd"
			 type: 'recno
			 flags: #b10000)) ;; read-only
(define *keys* (key-sequence x))
