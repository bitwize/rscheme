
;;; A mutex contains a queue of waiters and a state (we
;;; use a queue instead of a list because we want to
;;; satisfy waiting threads in FIFO order, which would
;;; cost us O(n) time if we used a list)
;;;

(define-class <mutex> (<semaphore>)
  (mutex-owner init-value: #f)
  (mutex-name setter: #f)
  (mutex-specific init-value: #f setter: mutex-specific-set!))

(define (mutex? obj) 
  (instance? obj <mutex>))

(define-method write-object ((self <mutex>) port)
  (format port "#[<mutex> ~s ~a]" (name self) (count self)))

(define (make-mutex #optional name)
  (make <mutex>
        state: (%make <vector> #f #f)
        count: 1
        mutex-name: name))

(define-method name ((self <mutex>))
  (mutex-name self))

(define (mutex-lock! (self <mutex>) #rest r)
  (if (pair? r)
      (error "mutex-lock!: timeouts and give-away not supported"))
  (semaphore-wait self)
  (set-mutex-owner! self (current-thread)))

(define (mutex-unlock! (self <mutex>) #rest r)
  (if (pair? r)
      (error "mutex-unlock!: timeouts not supported"))
  (if (<= (count self) 0)
      ;; it is not an error to unlock a mutex multiple times,
      ;; but we sure don't want to signal the underlying semaphore,
      ;; or else the next attempt to lock will blow right through
      (begin
        (set-mutex-owner! self #f)
        (semaphore-signal self))))

(define-syntax (with-mutex mutex . body)
  (dynamic-wind
      (lambda () (mutex-lock! mutex))
      (lambda () (begin . body))
      (lambda () (mutex-unlock! mutex))))

(define (mutex-state (self <mutex>))
  ;; we don't yet support unowned or abandoned mutexes
  (if (> (count self) 0)
      'not-abandoned
      (mutex-owner self)))


