#|------------------------------------------------------------*-Scheme-*--|
 | File:	    packages/threads/shell/bg.scm
 |
 |          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.4
 | File mod date:    2003-07-17 11:42:45
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  rs.sys.threads.shell
 |
 | Purpose:          Provide functionality somewhat like shell's "&"
 |		     in creating background jobs
 `------------------------------------------------------------------------|#

(define (run-in-bg thunk #optional name)
  (let ((err (current-error-port)))
    ;
    (define (bg-condition-handler (c <condition>) next)
      (format err "*** Error in background thread ~a ~s\n~a" 
              (current-thread) 
              name
              c)
      (halt-thread (current-thread) c))
    ;
    (thread-resume
     (make-thread 
      (lambda ()
	(handler-bind (<condition> bg-condition-handler)
	  (thunk)))
      "bg"))))

(define-syntax (bg . body)
  (run-in-bg
   (lambda ()
     (begin . body))
   (mquote body)))
