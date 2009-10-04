#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/threads/threads.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.9
 | File mod date:    1997-11-29 23:10:41
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  threads
 |
 | Purpose:          Thread system kernel
 `------------------------------------------------------------------------|#

(define *time-slice* 15)

;; the currently executing thread, and the amount of time
;; it was given for it's slice (usually = *time-slice*, but may
;; be less if it was started because a friend yielded the remainder
;; of its timeslice)

(define *current-thread* #f)
(define *setup-time* #f)

;; a <dequeue> object containing threads awaiting execution.
;; does *not* contain the current thread

(define *waiting-queue* #f)

;;
;;============================================================

(define-syntax (thread-msg . args))
;(define-syntax (thread-msg msg . args) (format #t msg . args))

(define-class <thread> (<object>)
    (thread-state init-value: #f
		  init-keyword: #f)
    (thread-return-values init-value: #f
			  init-keyword: #f)
    (thread-name init-value: #f
		 init-keyword: #f)
    (suspend-count init-value: 0
		   init-keyword: #f)
    (total-time init-value: 0
		init-keyword: #f))

;;

(define-method write-object ((self <thread>) port)
  (format port "#[<thread> ~a]" (thread-name self)))

(define (make-thread name (thunk <function>))
  (let (((self <thread>) (%make <thread> #f #f name 0 0)))
    (call-with-current-continuation
     (lambda (exit)
       (start-thread
	thunk
	self
	(call-with-current-continuation
	 (lambda (state)
	   (set-thread-state! self state)
	   (exit self))))))))

(define-method initialize ((self <thread>) #rest kwds)
  (let* ((kvv (keyword-value-list->vector kwds))
	 (thunk (get-keyword-value kvv 'thunk: '#unbound))
	 (name (get-keyword-value kvv 'name: '#f)))
    ;;
    (if (eq? thunk '#unbound)
	(error "missing `thunk:' keyword to new <thread>"))
    (check-function thunk)
    (set-thread-name! self name)
    ;;
    (call-with-current-continuation
     (lambda (exit)
       (start-thread
	thunk
	self
	(call-with-current-continuation
	 (lambda (state)
	   (set-thread-state! self state)
	   (exit self))))))))

;;
;; this is the entry point to a new thread, called when
;; the thread is run for the first time
;;

(define (start-thread thunk (t <thread>) time)
  (thread-msg "starting ~s (~s)\n" t *current-thread*)
  (os-set-timer time)
  (bind ((#rest result (thunk))
	 (time-left (os-halt-timer)))
    (thread-msg "halted ~s with ~d left, total ~d\n  RESULT => ~s" 
		t 
		time-left
		(total-time t)
		result)
    (leave-user-mode time-left)
    (set-thread-return-values! t result)
    ;; this thread is now defunct
    (dispatch-to-next-thread)))
  
;;
;; note that we don't have a halt-timer; the timer is implicitly
;; halted when it goes off, which is why we're being called
;;

(define (time-slice-over)
  (thread-msg "time-slice-over: ~s\n" *current-thread*)
  (leave-user-mode 0)
  ;; re-schedule us for some more time...
  (dequeue-push-back! *waiting-queue* *current-thread*)
  (let ((t (switch-to-next-thread)))
    (thread-msg "return after time-slice-over switch: ~s\n" *current-thread*)
    ;; we're coming back from yielding
    ;; set up to timeout again
    (os-set-timer t)
    (values)))

(define (thread-yield)
  (let ((t (os-halt-timer)))
    (thread-msg "thread yield: ~s (~d time left)\n" *current-thread* t)
    (leave-user-mode t)
    ;; we'll be ready again after everybody else has run...
    (dequeue-push-back! *waiting-queue* *current-thread*)
    (let ((t (switch-to-next-thread)))
      (thread-msg "back after thread yield: ~s\n" *current-thread*)
      (os-set-timer t)
      (values))))

(define (thread-yield-until-friend t)
  (thread-msg "thread yield until friend of: ~s (~d time left)\n"
	      *current-thread* t)
  (leave-user-mode t)
  ;; we don't re-enqueue ourself, so
  ;; somebody has to call us from `thread-yield-to-friend'
  (bind ((t v (switch-to-next-thread)))
    (thread-msg "back after friend: ~s (~s time, value ~s)\n" 
		*current-thread* t v)
    (os-set-timer t)
    v))
  

;;
;; yield our remaining time to a friend,
;; who is *NOT* in the queue already

(define (thread-yield-to-friend thread time value)
  (thread-msg "thread yield ~d from: ~s to: ~s\n" time *current-thread* thread)
  (leave-user-mode time)
  ;; re-enqueue us for execution a little later
  (dequeue-push-back! *waiting-queue* *current-thread*)
  (call-with-current-continuation
   (lambda (state)
     (set-thread-state! *current-thread* state)
     (thread-msg "switch to friend: ~s\n" *current-thread*)
     (run-thread thread time value))))

(define exit-threads (lambda ()))
(define event-manager-time-did-pass (lambda (t)))
(define event-manager-no-ready-threads (lambda ()))

(define (run-threads . initial-threads)
  (run-threads*
   (lambda ()
     (values (lambda 'default-time-did-passed (t))
	     (lambda 'default-no-ready-threads () (exit-threads))))
   initial-threads))

;;
;; the thread system / event manager protocol deserves some comments.
;;
;; the event manager provides two functions,
;;  the `time-did-pass' function
;;  and the `no-ready-threads' function
;; both are called from "kernel mode", meaning that the timer has
;; been halted.  `time-did-pass' is called from the dynamic
;; state, and with *current-thread* set to, the thread which was
;; previously running.
;;
;; in the current implementation, `no-ready-threads' is called from the
;; DYNAMIC STATE OF THE LAST EXECUTED THREAD
;;

(define (run-threads* init-event-manager initial-threads)
  (call-with-current-continuation
   (lambda (x)
     (set! exit-threads x)
     ;; 
     ;; install the event manager's hooks
     ;;
     (bind ((tpass none-ready (init-event-manager)))
       (set! event-manager-time-did-pass tpass)
       (set! event-manager-no-ready-threads none-ready))
     ;;
     (thread-msg "initializing ~d threads...\n" (length initial-threads))
     (set! *waiting-queue* (make-dequeue))
     (for-each (lambda (t)
		 (dequeue-push-back! *waiting-queue* t)
		 (values))
	       initial-threads)
     ;;
     (register-interrupt-handler! 'timer time-slice-over)
     (dispatch-to-next-thread)))
  (thread-msg "threads system exiting!\n")
  (os-halt-timer))

;;
  
(define (mark-thread-ready thread)
  (dequeue-push-back! *waiting-queue* thread)
  (values))

(define (mark-thread-ready-soon thread)
  (dequeue-push-front! *waiting-queue* thread)
  (values))

;;
;;  enters user mode for the current thread,
;;  which involves setting the timer.
;;
;;  passes the given arguments to the thread's resumption

(define *setup-time* #f)

;; leaves user mode for the current thread
;; this involves billing the time in user mode
;; to the thread, which in turn involves unsleeping
;; any sleeping threads

(define (leave-user-mode time-left)
  (let ((time-passed (fixnum- *setup-time* time-left)))
    (set-total-time! *current-thread* (fixnum+ (total-time *current-thread*)
					       time-passed))
    (event-manager-time-did-pass time-passed)))


;; ===new code===
;;=======================
;; the current thread has been disposed of (one way or another),
;; and the OS timer is halted.  Run the next thread in line, if
;; there is one

(define (dispatch-to-next-thread)
  (if (dequeue-empty? *waiting-queue*)
      ;; nothing is ready to be run
      (begin
	(event-manager-no-ready-threads)
	(dispatch-to-next-thread))
      ;; something is ready
      (run-thread (dequeue-pop-front! *waiting-queue*) *time-slice* #f)))

(define (run-thread thread time arg)
  (set! *current-thread* thread)
  (set! *setup-time* time)
  (thread-msg "dispatching thread: ~s\n" thread)
  ((thread-state thread) time arg))

;;
;;  does the "kernel" work of yield, which is to 
;;  capture the current thread's state and run another thread
;;  *DOES NOT* re-enqueue the current thread for execution

(define (switch-to-next-thread)
  (thread-msg "switch from thread: ~s\n" *current-thread*)
  (call-with-current-continuation
   (lambda (state)
     (set-thread-state! *current-thread* state)
     (dispatch-to-next-thread))))
