#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/threads/messages.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.6
 | File mod date:    1997-11-29 23:10:41
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  threads
 |
 `------------------------------------------------------------------------|#

(define-class <mailbox> (<object>)
  (waiting-objects init-value: '())
  (waiting-threads init-value: '()))

(define (receive-message! (mbox <mailbox>))
  (let ((t (os-halt-timer)))
    (let ((q (waiting-objects mbox)))
      (if (null? q)
	  (begin
	    (set-waiting-threads! mbox (append (waiting-threads mbox)
					       (list *current-thread*)))
	    ;;
	    ;; thread-yield-until-friend will reenable
	    ;; the timer when it returns
	    ;;
	    (thread-yield-until-friend t))
	  (begin
	    (set-waiting-objects! mbox (cdr q))
	    (os-set-timer t)
	    (car q))))))

(define (send-message! (mbox <mailbox>) msg)
  (let ((t (os-halt-timer)))
    ;(format #t "[send message] ~s\n" msg)
    ;;
    ;; interrupts are now effectively disabled,
    ;; and the time we spend here is billed to
    ;; the kernel
    ;;
    (let ((q (waiting-threads mbox)))
      (if (null? q)
	  (begin
	    (set-waiting-objects! mbox (append (waiting-objects mbox)
					       (list msg)))
	    (os-set-timer t)
	    (values))
	  (begin
	    (set-waiting-threads! mbox (cdr q))
	    ;; hand our timeslice off to the waiting
	    ;; thread, along with the object being enqueued
	    (os-set-timer (thread-yield-to-friend (car q) t msg))
	    (values))))))
