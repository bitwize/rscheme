#|------------------------------------------------------------*-Scheme-*--|
 | File:    librarydev/srfi/18/threads.scm
 |
 |          Copyright (C) 2003 Joerg F. Wittenberger <joerg.wittenberger@pobox.com>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.0
 | File mod date:    2003.02.22 13:28:00
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  srfi-18
 |
 | Purpose:          Implement thread primitives according to srfi-18.
 `------------------------------------------------------------------------|#

(define thread-start! thread-resume)

(define thread-join! thread-join)

(define thread-suspend! thread-suspend)

(define (thread-terminate! thread)
  (thread-suspend! thread)
  (set-thread-stack!
   thread
   (%make <partial-continuation>
	  (lambda () #f)
	  thread-entry
	  (code-pointer thread-entry)
	  #f))
  (thread-resume thread))
