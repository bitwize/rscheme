#|------------------------------------------------------------*-Scheme-*--|
 | File:    packages/threads/shell/pool.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rosette.com>
 |          as part of the RScheme project, licensed for free use
 |
 | Version: 1.4
 | Date:    2003-07-17 11:42:45
 | Build:   v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose: Create and invoke a thread pool
 +------------------------------------------------------------------------|
 | Notes:
 |	A thread pool is a growable collection of threads which provide
 |	some particular service, in the form of a function which applied
 |	to each object sent to the pool.
 |
 |	A function pool is the same as a thread pool, except that the
 |	associated (pooled-over) function is expected to return a value,
 |	so the `invoke' operation blocks until the answer is returned.
 |	
 `------------------------------------------------------------------------|#
;;;
;;;  
;;;

(define-class <thread-pool> (<object>)
  (name type: <string>)
  (mbox type: <mailbox>)
  (group type: <thread-group>)
  (consumer type: <function>))

(define-class <thread-rpc-pool> (<thread-pool>))

(define (make-thread-pool name proc)
  (make <thread-pool>
	name: name
	mbox: (make-mailbox (format #f "tpool:~a" name))
	group: (make <thread-group>)
	consumer: proc))

(define (make-function-pool name proc)
  (make <thread-rpc-pool>
	name: name
	mbox: (make-mailbox (format #f "fpool:~a" name))
	group: (make <thread-group>)
	consumer: proc))

(define-method thread-pool-thunk ((self <thread-pool>))
  (lambda ()
    (let loop ()
      ((consumer self) (receive-message! (mbox self)))
      (loop))))

(define-method thread-pool-thunk ((self <thread-rpc-pool>))
  (lambda ()
    (let loop ()
      (let ((req (receive-message! (mbox self))))
	(send-message! (car req) ((consumer self) (cdr req)))
	(loop)))))

(define (grow-thread-pool (self <thread-pool>))
  (thread-resume
   (make-thread* (thread-pool-thunk self)
		 (string-append "|" (name self))
		 (group self))))

(define-method thread-pool-invoke ((self <thread-pool>) item)
  (critical-section ()
   ;; if there are no threads waiting to take our data,
   ;; then create some.  Note that we have to do this in
   ;; a mutex region, or else two invoking threads might
   ;; both think that there is a ready thread and send
   ;; an object in, in which case the same pooled thread will be
   ;; obliged to consume both inputs, which it may not be
   ;; able to do (the first object in might be a poison pill
   ;; which crashes or stalls the first thread)
   (if (not (mailbox-has-waiters? (mbox self)))
       (grow-thread-pool self))
   (send-message! (mbox self) item))
  (values))

(define-method thread-pool-invoke ((self <thread-rpc-pool>) item)
  (let ((mbox (make-mailbox "tpool-rpc")))
    (next-method self (cons mbox item))
    mbox))

#|
;;;
;;;  testing...
;;;

(define *lst* '())

(define (ins item)
  (set! *lst* (cons item *lst*)))

(define p (make-thread-pool "ins" ins))
|#
