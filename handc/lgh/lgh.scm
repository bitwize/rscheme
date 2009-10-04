#|------------------------------------------------------------*-Scheme-*--|
 | File:    handc/lgh/lgh.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.2
 | File mod date:    1997-11-29 23:10:29
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#

,(use repl)

(define *top-level* #f)

(define (eval-string str)
  (eval (with-input-from-string str read) *top-level*))

(define (main args)
  (set! *top-level* (make-user-initial))
  ;; just return the eval-string procedure
  eval-string)

(restart-with "sys.img" main)