#|------------------------------------------------------------*-Scheme-*--|
 | File:    bytcodes/setup.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.3
 | File mod date:    1997-11-29 23:10:43
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#

;;
;; load and process bytecodes
;;

(load "process.scm")
(process-defs-file)
(create-primop-module 'primops)

;;
;; install them
;;

(load "loadbyt.scm")
(%save-self "-- bytecodes installed")
