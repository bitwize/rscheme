
(define (thread-list envt)
  (display-thread-list))

(define (show-thread-condition (self <thread>) res port)
  (cond
   ((instance? res <uncaught-exception>)
    (let ((inner (uncaught-exception-reason res)))
      (format port " uncaught: ~s\n" (class-name (object-class inner)))
      (format port "--------------------\n~a--------------------\n" inner)))
   (else
    (format port " condition: ~s" (class-name (object-class res))))))


(define (display-thread-list #optional (port default: (current-output-port)))
  (for-each
   (lambda ((k <fixnum>))
     (let* ((t (table-lookup *thread-table* k))
	    (t (and t (thread t))))
       (if t
	   (let (((n <string>) (internal-thread-name t))
                 (s (thread-state t)))
	     (format port " ~-5d [~a]~a ~6a ~10a"
		     (thread-number t)
		     n
		     (vector-ref '#("          "
				    "         "
				    "        "
				    "       "
				    "      "
				    "     "
				    "    "
				    "   "
				    "  "
				    " "
				    "")
				 (min 10 (string-length n)))
		   (gvec-ref $thread-state-names s)
		   (thread-time t))
	     (cond
              ((eq? s $thread-state-blocked)
               (format port " ~s" (thread-blocked-on t)))
              ((eq? s $thread-state-complete)
               (let ((res (thread-stack t)))
                 (cond
                  ((instance? res <condition>)
                   (show-thread-condition t res port))
                  ((list? res)
                   (format port " returned ~d values" (length res)))))))
             (newline port)))))
   (sort (key-sequence *thread-table*) <)))

(%early-once-only
 (define-command-proc tl thread-list ((",tl" "show thread list"))))

;;;
