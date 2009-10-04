#|------------------------------------------------------------*-Scheme-*--|
 | File:    librarydev/srfi/34/srfi-34.scm
 |
 |          Copyright (C) 2003 Joerg F. Wittenberger <joerg.wittenberger@pobox.com>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.0
 | File mod date:    2003.02.25 11:28:00
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  srfi-34
 |
 | Purpose:          Implement mutex.
 `------------------------------------------------------------------------|#

;; This implementation is a bit brain dead.  It translates SRFI
;; compliant code into rschemes exception handling system.  I'd
;; recomment to have a native, pure implementation, but I don't have
;; the time to do that right now.

(define (with-exception-handler handler thunk)
  (handler-case (thunk)
                ((<condition> condition: c) (handler c))))

(define-macro (raise obj) `(error ,obj))

(define-macro (guard clause . body)
  `(handler-case
    ,(if (and (pair? body) (null? (cdr body))) (car body) `(begin . ,body) )
    ((<condition> condition: ,(car clause))
     (cond ,@(cdr clause)
           . ,(if (assq 'else (cdr clause))
                  '()
                  `((else (raise ,(car clause)))))))))

