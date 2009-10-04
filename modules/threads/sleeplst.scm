#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/threads/sleeplst.scm
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

;;
;; subtracts `passed' ms from the head element(s), making
;; those ready as much as possible
;;

(define (unsleep-some-threads passed)
  (if (null? *sleep-list*)
      0
      (let* ((n (car *sleep-list*))
	     (h (car n)))
	(if (fixnum>=? passed h)
	    (begin
	      (mark-thread-ready (cdr n))
	      (if (null? (cdr *sleep-list*))
		  (set! *sleep-list* '())
		  (begin
		    (set! *sleep-list* (cdr *sleep-list*))
		    (unsleep-some-threads (fixnum- passed h)))))
	    (set-car! n (fixnum- h passed))))))

(define (delta-list-insert lst item in-time)
  (assert (fixnum>? in-time 0))
  (let loop ((p lst)
	     (prev #f)
	     (t in-time))
    (if (null? p)
	;; we're at the end of the list -- this object
	;; is being inserted after everything else
	(if prev
	    (begin
	      (set-cdr! prev (cons (cons t item) '()))
	      lst)
	    (cons (cons t item) '()))
	;;
	;; see if we are before or after the current element
	;; already in the list
	;;
	;; the `=' causes this item to be inserted as early in the
	;; list as possible, which will cause it to get executed
	;; AFTER all the other threads in the same time zone, because
	;; they are popped off this list and pushed on the readyq
	;;
	(if (fixnum<=? t (caar p))
	    ;;
	    ;; we are to be inserted before it
	    ;;
	    (begin
	      (set-car! (car p) (fixnum- (caar p) t))
	      (if prev
		  (begin
		    (set-cdr! prev (cons (cons t item) p))
		    lst)
		  (cons (cons t item) p)))
	    ;;
	    ;; we are to go somewhere after it
	    ;;
	    (loop (cdr p) p (fixnum- t (caar p)))))))

(define (thread-sleep ms)
  (if (fixnum<=? ms 0)
      (thread-yield)
      (let ((t (os-halt-timer)))
	(thread-msg "thread sleep ~s: ~d ms (w/ ~d time left)\n"
		*current-thread* 
		ms
		t)
	(leave-user-mode t)
	;;
	(set! *sleep-list* (delta-list-insert *sleep-list*
					      *current-thread*
					      ms))
	;;
	(mark-thread-unrunnable *current-thread*)
	(let ((t (switch-to-next-thread)))
	  ;;
	  (thread-msg "back after thread sleep: ~s\n" *current-thread*)
	  (os-set-timer t)
	  (values)))))

;;

;;
;; this function is called when the idle thread is the 
;; only ready thread
;;

(define (sleep-process-until-thread-awakens)
  (if (null? *sleep-list*)
      (exit-threads)
      (let ((x (os-halt-timer)))
	(leave-user-mode x)
	(let ((t (if (null? *sleep-list*)
		     1000 ;; wait 1 second, arbitrarily
		     (car (car *sleep-list*)))))
	  (unsleep-some-threads (os-sleep t)))
	(set! *setup-time* *time-slice*)
	((thread-state *current-thread*) *time-slice* #f))))

