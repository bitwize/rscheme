#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/repl/backtrac.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.6
 | File mod date:    1999-01-28 10:03:32
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  repl
 |
 | Purpose:          Apply backtrace support
 `------------------------------------------------------------------------|#

(define (suppress-abt-frame? argf)
  #f)

(define-method apply-backtrace ((self <condition-stack>))
  (apply-backtrace (vm-dynamic-state-reg self)))

(define-method apply-backtrace ((self <condition>))
  (let ((s (assq 'stack (properties self))))
    (if s
	(apply-backtrace (cdr s))
	(format #t "~s: stack not saved\n" self))))

(define-method apply-backtrace ((self <list>))
  ;; presumably its a dynamic state chain
  (apply-backtrace* self))

(define-method print-abt-frame ((self <vector>) port i)
  (if (procedure? (vector-ref self 0))
      (if (not (suppress-abt-frame? self))
	  (let ((n (vector-length self)))
	    (format port " [~d] : " i)
	    (write (vector-ref self 0) port)
	    (newline port)
		      (let loop ((j 1))
			(if (not (eq? j n))
			    (begin
			      (format port 
				      "      with [~d] = ~a\n"
				      (sub1 j)
				      (object->bounded-string 
				       59 
				       (vector-ref self j)))
			      (loop (add1 j)))))))))

(define-method print-abt-frame ((self <object>) port i)
  (format port " (~d) : " i)
  (display (object->bounded-string 68 self) port)
  (newline port))

(define (apply-backtrace* from)
  (let ((port (current-output-port)))
    (let loop ((i 0)
	       (s from))
      (if (pair? s)
	  (begin
	    (print-abt-frame (car s) port i)
	    (loop (add1 i) (cdr s)))))))

(define (abt-cmd envt)
  (apply-backtrace (get-dynamic-state-reg)))

