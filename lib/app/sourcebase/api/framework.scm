
(define-thread-var obtain-time time)
(define *timestamp* (obtain-time))
(define *user* #f)
(define *client-version* 0)
(define *audit-entry* #f)
(define *logical-log* #f)

(define (set-logical-log! (port <output-port>))
  (set! *logical-log* port))

(define-rewriter (define-api form)
  (let* ((api-fn (caadr form))
         (arglist (map (lambda (x)
       				  (if (pair? x)
				      (car x)
				      x))
			       (cdadr form)))
         (raw-form (string->symbol
  		    (string-append
		    	"raw-"
			(symbol->string api-fn)))))
    `(begin
       (define (,api-fn ,@arglist)
         (let ((journal-time (time)))
           (set! *timestamp* (obtain-time))
           (set! *audit-entry*
                 (make <audit-log-entry>
                       user: *user*
                       timestamp: *timestamp*
                       arg-list: (vector ,@arglist)
                       operation: ',api-fn
                       result: '#uninit
                       info: (if (> (abs (interval->seconds 
                                          (time-time journal-time
                                                     *timestamp*)))
                                    5)
                                 (list (cons 'journal-time journal-time))
                                 '()))))
	 (format #t "+API ~a ~s\n" ',api-fn (list ,@arglist))
	 (if *logical-log*
	     (begin
	       (write (list ',api-fn *timestamp* ,@arglist) *logical-log*)
	       (newline *logical-log*)
	       (flush-output-port *logical-log*)))
	 (bind ((#rest results (,raw-form ,@arglist)))
	   (set-result! *audit-entry* (list->vector results))
	   (list->values results)))
	(define (,raw-form ,@(cdadr form)) ,@(cddr form)))))
