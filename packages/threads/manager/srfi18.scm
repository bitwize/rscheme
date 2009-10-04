
(define (thread? obj)
  (instance? obj <thread>))

(define (thread-join! (thread <thread>) #rest timeout-spec)
  (if (null? timeout-spec)
      (thread-join thread)
      (error "timeout not supported by underlying thread-join")))


(define-class <terminated-thread-exception> (<condition>))

(define (thread-terminate! (target <thread>))
  (halt-thread target (make <terminated-thread-exception>)))

(define (terminated-thread-exception? obj)
  (instance? obj <terminated-thread-exception>))

(define (uncaught-exception? obj)
 (instance? obj <uncaught-exception>))

(define thread-start! thread-resume)

(define thread-suspend! thread-suspend)

;;;

(define (thread-yield!)
  (thread-sleep-ms 0))

(define-method thread-sleep! ((self <boolean>))
  (if self
      (error "thread-sleep!: invalid argument ~s" self)
      (thread-sleep-ms 0)))

(define-method thread-sleep! ((self <real>))
  (thread-sleep self))

(define-method thread-sleep! ((self <time>))
  ;;; XXX yuck...  need 'thread-sleep-until'
  (thread-sleep (max 0 (interval->seconds (time-time self (time))))))


