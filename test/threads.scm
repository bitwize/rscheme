#|------------------------------------------------------------*-Scheme-*--|
 | File:    test/threads.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.7
 | File mod date:    2005-06-13 08:29:41
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#

,(use rs.sys.threads.manager)

(define *console-mbox* (make-mailbox))

(define (tester n slp)
  (lambda ()
    (let loop ((i 0))
      (if (< i 10)
	  (begin
	    (send-message! *console-mbox*
			   (list "running ==> ~s (~d left)" 
				 n
				 (os-get-time-remaining)))
	    (thread-sleep slp)
	    (loop (+ i 1)))
	  (values n 'x n)))))


(define (console)
  (let loop ((n 0))
    (let ((m (receive-message! *console-mbox*)))
      (format #t "[~d] " n)
      (apply format #t m)
      (newline)
      (loop (+ n 1)))))

(define (test)
  (let ((tests (list (make-thread (tester 'foo 0.3) "foo")
		     (make-thread (tester 'bar 0.2) "bar")
		     (make-thread console "spew"))))
    (for-each thread-resume tests)
    (list (values->list (thread-join (car tests)))
	  (values->list (thread-join (cadr tests))))))

;;;

(test-begin "threads: mailbox tests")

(test-equal (test) '((foo x foo) (bar x bar)))

(test-end)

