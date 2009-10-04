#|------------------------------------------------------------*-Scheme-*--|
 | File:    test/d627.scm
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

(check #t (> (expt 2 30) 100))
(check #t (> (expt 2.0 30) 100))
(check #t (> (expt 2 30.0) 100))
(check #t (> (expt 2.0 30.0) 100))

(check 1024 (expt 2 10))
(check 1024.0 (expt 2 10.0))
