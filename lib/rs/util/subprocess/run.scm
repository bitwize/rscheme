;; returns two ports, the first feeds the input of the command,
;; the second accesses the output of the command

(define (port->run->port cmd . args)
  (bind ((r1 w1 (pipe))
	 (r2 w2 (pipe))
	 (p (run* cmd args (vector r1 w2 2))))
    (fd-close r1)
    (fd-set-blocking w1 #f)
    (fd-close w2)
    (values (filedes->output-port w1 #t)
	    (filedes->input-port r2 #t)
	    p)))

;;

(define (pipe-cons ign)
  (bind ((r w (pipe)))
    (cons r w)))

;;;
;;;  Extended process form:
;;;
;;;    <xpf> ::= (<process-form> <redir> ...)
;;;
;;;  where
;;;    <redir> ::= (< [<fdes>] <file-name>)  ;; default to fd 0
;;;             |  (> [<fdes>] <file-name>)  ;; default to fd 1
;;;             |  

;;;
;;;  somewhat like scsh `run/collecting*',
;;;
;;;    except 
;;;      (1) takes [cmd arg ...] instead of [thunk]
;;;      (2) returns the process object as the first value
;;;      (3) really does use pipes 
;;;          (our threads system avoids the deadlock issue, 
;;;          I believe)
;;;

(define (run/collecting* fds cmd . args)
  (let ((pipes (map pipe-cons fds))
	(redir-vec (make-vector (+ 1 (apply max 2 fds)) -1)))
    ;; set up defaults
    (vector-set! redir-vec 0 0)
    (vector-set! redir-vec 1 1)
    (vector-set! redir-vec 2 2)
    ;;
    (for-each
     (lambda (fd pipe)
       (if (eq? fd 0)
	   (vector-set! redir-vec fd (car pipe))
	   (vector-set! redir-vec fd (cdr pipe))))
     fds
     pipes)
    ;;
    (let* ((proc (run* cmd args redir-vec))
	   (ports (map (lambda (fd pipe)
			 (if (eq? fd 0)
			     (begin
			       (fd-close (car pipe))
                               (fd-set-blocking (cdr pipe) #f)
			       (open-queued-output (cdr pipe)))
			     (begin
			       (fd-close (cdr pipe))
			       (open-mbox-input-port (car pipe)))))
		       fds
		       pipes)))
      (list->values (cons proc ports)))))

;;;
