#|------------------------------------------------------------*-Scheme-*--|
 | File:    librarydev/srfi/18/mutex.scm
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
 | Purpose:          Implement mutex.
 `------------------------------------------------------------------------|#

(define (mutex? obj) (instance? obj <semaphore>))

(define (make-mutex . name)
  (make-semaphore (and (pair? name) (car name)) 1))

(define mutex-lock! semaphore-wait)

(define mutex-unlock! semaphore-signal)

(define-syntax (with-mutex mutex . body)
  (mutex-lock! mutex)
  (bind ((#rest r (handler-case
		      (begin . body)
		         ((<condition> condition: c)
			      (mutex-unlock! mutex)
			          (signal c)))))
    (mutex-unlock! mutex)
    (list->values r)))
