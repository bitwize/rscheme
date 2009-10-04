
(define *interrupt-mbox* (make-mailbox "signal"))

(define (do-user-intr)
  (send-message! *interrupt-mbox* (cons 'SIGINT (time))))

(define (do-c-signal name)
  (send-message! *interrupt-mbox* (cons name (time))))

(define *finalization-mbox* (make-mailbox "finalize"))

(define *finalizer-pool* (make-thread-pool "finalize" finalize))

(define (thread-yield)
  (thread-sleep-ms 1))

(define (synchronize-with-finalization)
  (send-message! *finalization-mbox* 'use-sync)
  (thread-yield)
  (gc-now)
  (thread-yield)
  (let ((s (make-semaphore)))
    (send-message! *finalization-mbox* s)
    (semaphore-wait s)))

(define (synchronous-finalizer)
  (set! *finalizer-pool* #f)
  (let outer-loop ()
    (let loop ((lst (receive-message! *finalization-mbox*)))
      (cond
       ((instance? lst <semaphore>)
        (semaphore-signal lst)
        (outer-loop))
       ((eq? lst 'use-sync)
        (outer-loop))
       ((null? lst)
        (outer-loop))
       (else
        ;(format #t "(( finalize ~s ))\n" (car lst))
        (finalize (car lst))
        (loop (cdr lst)))))))

(define (finalizer)
  (set-thread-priority! (current-thread) 4)     ; slightly higher priority
  (let outer-loop ()
    ;;(format #t "trying to finalize\n")
    (let loop ((lst (receive-message! *finalization-mbox*)))
      (if (eq? lst 'use-sync)
          (synchronous-finalizer)
          (if (null? lst)
              (outer-loop)
              (begin
                (thread-pool-invoke *finalizer-pool* (car lst))
                ;; we yield here so that a fast thread has a chance to
                ;; come back and consume another input.  we'd rather not
                ;; yield to all the other threads out there, though...
                (thread-yield)
                (loop (cdr lst))))))))
	
(define (do-finalize items)
  (send-message! *finalization-mbox* items))

(define (do-gc-flip)
  ;(display "GC flip\n")
  (values))
  
;;

(define (boot-threads-system thunk)
  (set-thread-state-reg! (thread-var-initial-state))
  (set-dynamic-state-reg! '())
  (start-threads
   (list (make-thread* thunk "main" (make <thread-group>))
	 (make-thread* finalizer "finalize" (make <thread-group>)))))

(define (start-with-thread thunk)
  ;
  (let ((inp (open-mbox-input-port 0))
	(out (open-queued-output 1))
	(err (open-queued-output 2))
        (initial-tv (make-empty-thread-vars)))
    ;;
    (set-flush-lines?! out #t)
    (set-flush-lines?! err #t)
    ;
    (update-thread-state! initial-tv 
                          (*input-port* inp)
                          (*output-port* out)
                          (*handler-chain* *backstop-handler-chain*)
                          (*console-error-port* err)
                          (*console-output-port* out)
                          (*console-input-port* inp)
                          (*error-port* err))
    (set-thread-var-initial-state! initial-tv))
  ;
  (set! *interrupt-mbox* (make-mailbox "signal"))
  (set! *finalization-mbox* (make-mailbox "finalize"))
  (set! *finalizer-pool* (make-thread-pool "finalize" finalize))
  ;
  (register-interrupt-handler! 'timer time-slice-over)
  (register-interrupt-handler! 'user-intr do-user-intr)
  (register-interrupt-handler! 'child-exited do-child-exited)
  (register-interrupt-handler! 'finalize do-finalize)
  (register-interrupt-handler! 'gc-flip do-gc-flip)
  (register-interrupt-handler! 'c-signal do-c-signal)
  ;
  (setup-c-signal-handler! 'SIGUSR1)
  (ignore-c-signal! 'SIGPIPE)
  ;(setup-c-signal-handler! 'SIGPIPE)
  ;
  (enable-subprocess-capture)
  (boot-threads-system 
   (lambda ()
     (thunk)
     ;; XXX a normal exit (return) is not possible once threads are started...
     ;; however, once the "main" procedure (which is what the given
     ;; `thunk' should tail call to -- see start.scm) exits, we want
     ;; the process to exit
     (process-exit 0))))

(%early-once-only
 (set-start-threads-hook! start-with-thread))
