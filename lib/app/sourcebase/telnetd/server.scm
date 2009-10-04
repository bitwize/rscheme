;;
;;  server daemon
;;

(define *remote-host* #f)

(define (collect-client service)
  (bind ((fd peer (get-next-client service))
	 (peer-host port (inet-socket-hostname peer)))
    (values peer-host fd)))

(define (get-daemon-port) 2059)

(define-hook server-startup)

(define (sigpipe)
  (format (current-error-port) "*** Got a SIGPIPE\n"))

(define (server-daemon #optional (port default: (get-daemon-port)))
  (let* ((fd (inet-server port))
	 (svc (make-service fd)))
    ;;
    (format #t "server socket fd ~d\n" fd)
    (with-module
        start
      (register-c-signal-handler! 'SIGPIPE sigpipe))
    ;;
    (run-hooks *server-startup-hook*)
    (let loop ()
      (bind ((peer-host fd (collect-client svc))
	     (thread (thread-resume
		      (make-thread (lambda ()
				     (run-server peer-host fd))
				   (format #f "server[~a]" peer-host)))))
	;; single-threaded for now, because I'm
	;; not sure the system can actually handle concurrency
	;; against the object database
	(thread-join thread)
	(loop)))))

(define (run-server peer fd)
  (let* ((i (open-mbox-input-port fd))
	 (o (open-queued-output fd)))
    (handler-case
     (app-server peer i o fd)
     ((<condition> condition: c)
      ;;; The only way we get here is if there is an error
      ;;; during error handling in `app-server', in which
      ;;; case things are pretty hosed.  Common case is
      ;;; an attempt to roll back the object store state,
      ;;; which means our in-memory image is inconsistent
      ;;; and we should just BAIL!
      (format #t "*** service from ~s failed:\n*** ~a" peer c)
      (format #t "*** EXITING PROCESS DUE TO INCONSISTENT MEMORY IMAGE ***\n")
      (flush-output-port (current-output-port))
      (process-exit 1)))
    (close-input-port i)
    (close-output-port o)
    (fd-close fd)))
