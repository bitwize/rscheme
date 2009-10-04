#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/threads/idle.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.3
 | File mod date:    1997-11-29 23:10:41
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#

(define (idle)
  (let loop ((i 0))
    ;;
    (if (eq? (cdr *ready-queue*) *ready-queue*)
	(sleep-process-until-thread-awakens)
	(thread-yield))
    (loop (+ i 1))))

