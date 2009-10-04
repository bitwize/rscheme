
(define-constant $zero-interval (seconds->interval 0))

(define-class <system-service> (<object>) :abstract
  (properties type: <vector> init-value: '#())
  (name type: <string>)
  (start-time init-value: #f)
  (parent init-value: #f)
  (children init-value: '())
  ;;
  (num-instances-started init-value: 0)
  (num-instances-finished init-value: 0)
  (last-instance-started init-value: #f)
  (past-instance-time-accum init-value: $zero-interval)
  (active-instances))

(define-method instance-time-accum ((self <system-service>))
  (let ((v (dequeue-state (active-instances self))))
    (let loop ((i 0)
               (a (past-instance-time-accum self)))
      (if (< i (vector-length v))
          (loop (+ i 1)
                (interval+interval a (thread-time (thread (vector-ref v i)))))
          a))))

(define-class <service-instance> (<object>)
  (properties type: <vector> init-value: '#())
  (container type: <system-service>)
  (start-time type: <time>)
  (stop-time init-value: #f)
  thread)

(define (stop-instance (self <service-instance>))
  (if (not (stop-time self))
      (let ((dt #f)
            (c (container self)))
        (thread-sleep-ms 0)
        (set! dt (thread-time (thread self)))
        (set-past-instance-time-accum! 
         c 
         (interval+interval dt (past-instance-time-accum c)))
        (set-stop-time! self (time))
        (set-thread! self #f)
        (dequeue-delq! (active-instances c) self)
        (set-num-instances-finished! c (add1 (num-instances-finished c)))))
  (values))

(define (start-instance (self <system-service>) #key (thread default: #f))
  (let ((i (make <service-instance>
                 container: self
                 start-time: (time)
                 thread: (or thread (current-thread)))))
    ;;
    (set-last-instance-started! self (start-time i))
    (set-num-instances-started! self (add1 (num-instances-started self)))
    (dequeue-push-back! (active-instances self) i)
    i))

(define-method initialize ((self <system-service>))
  (if (parent self)
      (set-children! (parent self) (cons self (children (parent self))))))

(define-method service-start ((self <system-service>) #rest options)
  (values))

(define-method service-shutdown ((self <system-service>) #rest options)
  (values))

(define-method service-reinit ((self <system-service>) #rest options)
  (values))

(define-method service-status ((self <system-service>))
  "unknown")

(define-thread-var *current-service* #f)

(define (current-service)
  *current-service*)

(define (call-with-service (self <system-service>) thunk)
  (thread-let ((*current-service* self))
    (thunk)))
