
(define (init)
  (make-database "/tmp/test074/sto"
		 "testfam"
		 "sbtester"
		 "SourceBase Tester"
		 "test@rscheme.org"
		 "rscheme"
		 (gethostbyaddr (gethostbyname (hostname))))
  ;;
  (table-insert! (property-table *application*)
		 "target"
		 (make <property>
		       name: "target"
		       property-value-type: '<string>
		       description: "Target"))
  ;;
  (set-service-state! *application* 'available)
  (commit *pstore*))

(define (open0)
  (database-connect "/tmp/test074/sto" 'update))

(define (open1)
  (database-connect "/tmp/test074/sto.v1" 'update))

(define (open2)
  (database-connect "/tmp/test074/sto.v2" 'update))

(define (sigint)
  (flush-output-port (current-output-port))
  (format (current-error-port) "*** Going down on SIGINT\n")
  (flush-output-port (current-error-port))
  (process-exit 7))

(define (restart)
  (with-module
      unixm
    ;; tail call another process
    (format (current-error-port) "*** SIGHUP -- Restarting...\n")
    ;; close everything except 0, 1, and 2
    (for-each (lambda (fd)
		(fd-close fd))
	      (cdddr (range 256)))
    (exec (string-append
	   (or (getenv "SBTEST_SB") ".")
	   "/test/test-074.rc")
	  "test-074.rc")))

;(with-module rs.sys.threads.shell (bg (server-daemon 2123)))

#|

...(these are other tests...)

(database-connect "/tmp/xyz2.sto" 'update)
(set! *do-auto-commit* #t)
(with-module rs.sys.threads.shell (bg (server-daemon 2123)))
(server-daemon 2123)
|#

(define (bd #key (tell default: #f))
  (set! *do-auto-commit* #t)
  ;;
  (if tell
      (add-server-startup-hook!
        (lambda ()
         (fd-close (inet-client "localhost" tell)))))
  (with-module
      start
    (register-c-signal-handler! 'sigint sigint))
  (with-module
      start
    (register-interrupt-handler! 'c-signal handle-c-signal)
    (register-c-signal-handler! 'SIGHUP restart))
  ;;
  (handler-case
   (begin
     (format #t "testfam listening on port 2123\n")
     (server-daemon 2123))
   ((<condition> condition: c)
    (format (current-error-port) "ERROR: ~a" c)
    (process-exit 1))))
