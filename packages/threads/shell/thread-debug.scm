;; on behalf of CR 701

(define (thread-debug envt args)
  (let* ((t (table-lookup *thread-table* (car args)))
         (t (and t (thread t))))
    (if t
        (let ((s (thread-stack t)))
          (cond
           ((instance? s <condition>)
            (format #t "***************************************************\n")
            (display s)
            (format #t "***************************************************\n")
            (post-mortem s))
           ((instance? s <partial-continuation>)
            (post-mortem s (thread-dynamic-state t)))
           ((pair? s)
            (error "Thread completed normally with ~d values: ~s"
                   (length s)
                   s))
           (else
            (error "Thread is in a wierd state; don't know how to debug: ~s" 
                   s))))
        (error "No such thread: ~s" (car args)))))

(%early-once-only
 (define-command-proc (td) thread-debug ((",(td id)" "debug thread"))))
