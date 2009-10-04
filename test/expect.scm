#|------------------------------------------------------------*-Scheme-*--|
 | File:    test/expect.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.7
 | File mod date:    1999-01-23 16:03:30
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#

(define *errors* '())
(define-fluid *test-place* #f)
(define compare equal?)

(define (with-place place thunk)
  (fluid-let ((*test-place* place))
    (thunk)))

(define (get-error-report)
  *errors*)

(define (report-failure in-expr report-conditions-thunk)
  (if *test-place*
      (begin
	(set! *errors* (cons *test-place* *errors*))
	(display (make-string 60 #\-))
	(format #t "\nin ~j::\n" *test-place*)
	(set! *test-place* #f)))
  (format #t "ERROR IN ~@#*60s\n" in-expr)
  (report-conditions-thunk (current-output-port)))
  
(define (check* expr expected-value actual-value)
  (format #t "checking ~@#*60s\n" expr)
  (if (not (compare expected-value actual-value))
      (report-failure
       (mquote expr)
       (lambda (port)
	 (format port "  expected ~@#*60s\n" expected-value)
	 (format port "       got ~@#*60s\n" actual-value))))
  (values))

(define-syntax (check value expr)
  (check* (mquote expr) value expr))

(define-syntax (compare-using proc . body)
  (fluid-let ((compare proc))
    (begin . body)))

(define-syntax (test-section place . body)
  (fluid-let ((*test-place* (append (or *test-place* '()) (mquote place))))
    (begin . body)))


(define-syntax (expect-to-fail expr)
  (expect-to-fail* (mquote expr) (lambda () expr)))

(define (expect-to-fail* expr thunk)
  (format #t "checking failure of ~@#*60s\n" expr)
  (handler-case
   (let ((result (thunk)))
     (report-failure
      expr
      (lambda (port)
	(format port "  expected an exception to be raised,\n")
	(format port "  got ~@#*60s\n" result))))
   ((<condition>)
    #t))
  (values))

(define (load-file file envt)    
  (with-place
   (list file 'file)
   (lambda ()
     (handler-case
      (load-into envt file)
      ((<condition> condition: c)
       (format (current-error-port)
	       "ERROR loading ~a\n~a" file c)
       (set! *errors* (cons (list file 'file) *errors*)))))))
