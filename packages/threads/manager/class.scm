;;;

(define-constant $thread-state-suspend 0)
(define-constant $thread-state-waiting 1)
(define-constant $thread-state-running 2)
(define-constant $thread-state-blocked 3)
(define-constant $thread-state-sleeping 4)
(define-constant $thread-state-complete 5)


(define-constant $thread-state-names
  '#(SUSP WAIT RUN BLOCK SLEEP DONE))

(define-class <thread-group> (<object>)
  (threads type: <list> init-value: '())
  (parent-group init-value: #f))

;;; `thread-blocked-on' can be any of the following classes of object
;;; when the state is BLOCKED
;;;
;;;    <thread>              -- in `thread-join'
;;;    <semaphore>           -- in `semaphore-wait'
;;;    <queued-output-port>  -- in `fd-output-port-write-string'
;;;    <mailbox>             -- in `receive-message'
;;;
;;; also, if the state is SLEEPING, then `thread-state-blocked' is:
;;;    Event            -- in `thread-sleep'
;;;
;;; also, when the state is SUSPENDED, then any of those objects
;;; may be present as well.  When the thread is resumed, the type
;;; of object determines the new state, which is the inverse map
;;; of the above table:
;;;
;;;    thread-blocked-on     new thread-state
;;;    0                 --> WAITING
;;;    <fixnum>          --> SLEEPING   (ie, its a raw Event)
;;;    <object>          --> BLOCKED
;;;
;;; ** note that a thread may be suspended and still be present
;;;    on a queue according to how its blocked (ie, in either
;;;    a run list according to its priority, or in a <mailbox>,
;;;    <semaphore>, <queued-output-port>, or <thread> queue if it
;;;    was blocked, or in an Event object if it was sleeping)
;;;
;;;    whether or not the thread is still on the queue is indicated
;;;    by the SIGN of the suspend count.  If the suspend count is
;;;    postive (> 0), then the thread is still queued somewhere.
;;;    if the suspend count is negative, the thread has been removed
;;;    from the queue.
;;;
;;;    A thread is never SLEEPING with a negative suspend count,
;;;    because when the time goes off, the event is freed and the
;;;    thread is marked ready-to-run-when-unsuspended (ie, by
;;;    setting its thread-blocked-on to ZERO)
;;;
;;;    Likewise, a thread is never BLOCKED on a <queued-output-port>
;;;    with a negative suspend count; it works like timers, in that
;;;    it makes sense for the caller, `qout-flush', to return eventually
;;;    when the thread is resumed.
;;;

(define-class <thread> (<object>)
  (thread-state type: <fixnum> init-value: $thread-state-suspend :sealed)
  (thread-stack :sealed)
  (thread-vars :sealed)
  (thread-dynamic-state :sealed)
  (thread-time type: <interval> :sealed)
  (thread-group type: <thread-group> :sealed)
  (thread-priority type: <fixnum> init-value: 5 :sealed)
  ;; `thread-blocked-on' defaults to 0 instead of #f for event's benefit
  (thread-blocked-on init-value: 0 :sealed)
  (thread-next-threadtime-event init-value: 0 :sealed)
  (thread-joins type: <list> init-value: '() :sealed)
  (internal-thread-name type: <string> :sealed)
  (thread-number type: <fixnum> :sealed)
  (thread-suspend-count type: <fixnum> init-value: -1 :sealed)
  ;; later...
  ;(thread-owns-mutex-list type: <list> init-value: '())
  (thread-initial-dynamic-state :sealed)
  (thread-specific init-value: #f setter: thread-specific-set!))

(define-method thread-runnable? ((self <thread>))
  (or (eq? (thread-state self) $thread-state-waiting)
      (eq? (thread-state self) $thread-state-running)))

(define-method thread-blocked? ((self <thread>))
  (let ((s (thread-state self)))
    (or (eq? s $thread-state-suspend)
        (eq? s $thread-state-blocked))))

(define-method thread-complete? ((self <thread>))
  (eq? (thread-state self) $thread-state-complete))

;;;
;;;  the queue holds threads if `has-data?' is #f
;;;  or data elements if `has-data?' is #t
;;;

(define-class <mailbox> (<dequeue>)
  (has-data? type: <boolean> init-value: #t :sealed)
  (name init-value: #f))

;;;
;;;  the queue holds waiting threads, and there are
;;;  |count| of them if count<0, otherwise there are 
;;;  0 of them
;;;

(define-class <semaphore> (<dequeue>)
  (count type: <fixnum> init-value: 0)
  (name init-value: #f))

;;;

(define-class <thread-queue> (<dequeue>))

;;;

(define-method post-mortem ((self <thread>))
  (if (instance? (thread-stack self) <condition>)
      (post-mortem (thread-stack self))
      (error "Thread ~s did not die on a condition" self)))


;;;

(define-class <service> (<object>)
  (mbox type: <mailbox>)                        ;; [0]
  (event init-value: 0 getter: #f setter: #f)   ;; [1]
  (fd type: <fixnum>))                          ;; [2]

(define-class <server-socket> (<object>)
  filedes
  service
  (name init-value: #f))

(define-class <peer-socket> (<object>) :abstract
  filedes                               ; [0]
  peer                                  ; [1]
  (name init-value: #f)                 ; [2]
  (input-port init-value: #f)           ; [3]
  (output-port init-value: #f))         ; [4]

(define-class <responder-socket> (<peer-socket>))
  
(define-class <initiator-socket> (<peer-socket>)
  (event init-value: 0)                 ; [5]
  (connect-waiter init-value: #f))      ; [6] a <thread> during connection

(define-class <select-event> (<object>)
  (properties init-value: '#())         ; [0]
  (event init-value: 0)                 ; [1]
  (select-waiter init-value: #f))       ; [2] a <thread>

