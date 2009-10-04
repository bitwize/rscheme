,(use rs.sys.threads.manager)

(define-class <async-variable> (<object>)
  name
  (async-value init-value: '#none)
  async-lock
  async-condition)

(define-method write-object ((self <async-variable>) port)
  (format port "#[<async-variable> ~s ~a]"
          (name self)
          (if (eq? (async-value self) '#none)
              ":unset"
              ":set")))

(define (make-async-variable name)
  (make <async-variable>
        name: name
        async-lock: (make-semaphore (~ "~a.lock" name) 1)
        async-condition: (make-condition-var (~ "~a.condition" name))))

(define-method kill-async-variable! ((self <async-variable>))
  (set-async-variable! self '#missing))

(define-method async-variable-set? ((self <async-variable>))
  (not (eq? (async-value self) '#none)))

(define-method set-async-variable! ((self <async-variable>) value)
  (with-semaphore
   (async-lock self)
   (set-async-value! self value)
   (condition-signal (async-condition self))))

(define-method force ((self <async-variable>))
  (let ((c (async-value self)))
    (cond
     ((eq? c '#none)
      (condition-test-and-alter
       (async-lock self) 
       (async-condition self)
       (lambda () (not (eq? (async-value self) '#none))))
      (force self))
     ((eq? c '#missing)
      (signal (make <terminated-thread-exception>)))
     (else
      c))))

#|
testing...
,(use rs.sys.threads.shell)

(define x (make-async-variable 'x))

(bg (format #t "T1 => ~s\n" (force x)))
(bg (format #t "T2 => ~s\n" (force x)))
(format #t "set yet? ~s\n" (async-variable-set? x))
(set-async-variable! x 33)
(format #t "set yet? ~s\n" (async-variable-set? x))
|#
