#|------------------------------------------------------------*-Scheme-*--|
 | File:	    packages/threads/manager/processio.scm
 |
 |          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.14
 | File mod date:    2005-02-24 11:10:17
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  rs.sys.threads.manager
 |
 | Purpose:          Thread-safe Process I/O (pipes & stuff)
 `------------------------------------------------------------------------|#

;;;-----------------------------------------------------------------------
;;;			 Process Environment
;;;-----------------------------------------------------------------------


(define *process-environment* #f)
(define *process-environment-as-vector* #f)

(set! *process-environment* #f) ;; reset when process starts
(set! *process-environment-as-vector* #f)

(define (process-environment)
  (if (not *process-environment*)
      (let ((tbl (make-string-table)))
	(for-each (lambda ((e <pair>))
		    (table-insert! tbl (car e) (cdr e)))
		  (the-environ))
	(set! *process-environment* tbl)))
  *process-environment*)

(define-thread-var *thread-environ* '())

(define (env-table->vector tbl)
  (vector-map
   (lambda (k)
     (string-append k "=" (table-lookup tbl k)))
   (sort (list->vector (key-sequence tbl)) string<?)))

(define (process-environment-as-vector)
  (if (not *process-environment-as-vector*)
      (set! *process-environment-as-vector* (env-table->vector (process-environment))))
  *process-environment-as-vector*)

(define (thread-environment-as-vector)
  (let ((te *thread-environ*))
    (if (null? te)
        (process-environment-as-vector)
        (begin
          (if (not (car te))
              (let ((tbl (hash-table-copy (process-environment))))
                (for-each (lambda (p)
                            (if (cdr p)
                                (table-insert! tbl (car p) (cdr p))
                                (table-remove! tbl (car p))))
                          (reverse (cdr te)))
                (set-car! te (env-table->vector tbl))))
          (car te)))))

(define (thread-directory)
  (if (eq? (current-directory) $dot-dir)
      #f
      (pathname->os-path (current-directory))))

(define-syntax (thread-environ-extend key value)
  (let ((te *thread-environ*))
    (cons* #f   ; this is where the cached vector will be stored
           (cons key value)
           (if (pair? te)
               (cdr te)         ; skip the now-invalid vector
               '()))))

(define (thread-environ-set! key value)
  (set! *thread-environ* (thread-environ-extend key value)))

(define (thread-environ-let key value thunk)
  (thread-let ((*thread-environ* (thread-environ-extend key value)))
    (thunk)))

;;;-----------------------------------------------------------------------
;;;		       Ways to run a subprocess
;;;-----------------------------------------------------------------------

(define-syntax (as-string object)
  (if (string? object)
      object
      (to-string object)))

(define (map-to-string lst)
  (map (lambda (item)
         (as-string item))
       lst))

(define (run cmd . args)
  (run* cmd args '#(0 1 2)))

;;;
;;;  This is the core procedure for creating a new subprocess,
;;;  possibly with it's input/output redirected to some pipes
;;;  that have been set up.
;;;
;;;  For this API, `args' is what comes after argv[0], 
;;;  which is set to the command itself

(define (run* cmd args fds #optional new-pgroup?)
  (make-subprocess arguments: args
                   command: (as-string cmd)
                   files: fds
                   new-pgroup?: #t))

(define (make-subprocess #key 
                         (arguments type: <list>)
                         (prepend-command? type: <boolean> default: #t)
                         (command type: <string> default: (car arguments))
                         (files type: <vector> default: '#(0 1 2))
                         (new-pgroup? default: #f)
                         (directory default: (thread-directory))
                         (environ default: (thread-environment-as-vector)))
  (let* ((cmd-file (if (string-search command #\/)
		       (if (file-access? command (access-mask execute))
			   command
			   (error "~a: not accessible in `execute' mode" command))
		       (find-in-path command)))
	 (p (make <process>
		  name: command)))
    (fork-and-exec p
                   cmd-file
                   (list->vector (if prepend-command?
                                     (cons cmd-file (map-to-string arguments))
                                     (map-to-string arguments)))
                   directory
                   environ
                   files
                   new-pgroup?)
    p))

;;;-----------------------------------------------------------------------
;;;	     Thread-aware implementations of process I/O
;;;-----------------------------------------------------------------------

;;;			 --- Input Ports ---

(define-class <process-input-port> (<input-port>)
  (name type: <string>)
  underlying-input-port
  process)

(define-method finalize ((self <process-input-port>))
  (close-input-port self))

(define (open-input-process/thread (str <string>))
  (bind ((port proc (run->port "sh" "-c" str)))
    (set-name! proc str)
    (set-name! port str)
    port))

(define-delegation ((self <process-input-port>) (underlying-input-port self))
  (input-port-read-char self)
  (input-port-peek-char self)
  (input-port-scan-token self)
  (input-port-read self)
  (input-port-read-line self)
  (input-port-read-rest self)     ;; / constituents of internal
  (input-port-read-len self len)) ;; \ read-string protocol


(define-method close-input-port ((self <process-input-port>))
  ;; since we created this port marked as the filedes "owner",
  ;; closing the port will close the filedes, which
  ;; may well cause the subprocess to get a SIGPIPE...
  (close-input-port (underlying-input-port self))
  (exit-status (process self)))

;;;			 --- Output Ports ---

(define-class <process-output-port> (<output-port>)
  (name type: <string>)
  underlying-output-port
  process)

(define-method finalize ((self <process-output-port>))
  (close-output-port self))

(define-delegation ((self <process-output-port>) (underlying-output-port self))
  (flush-output-port self)
  (output-port-write-char self char)
  (write-string self string))

(define-method close-output-port ((self <process-output-port>))
  (let ((fd (fd (underlying-output-port self))))
    (close-output-port (underlying-output-port self))
    (fd-close fd)
    (exit-status (process self))))

(define (open-output-process/thread (str <string>))
  (bind ((port proc (port->run "sh" "-c" str)))
    (set-name! proc str)
    (set-name! port str)
    port))

(define (make-process-input-port fd proc cmd)
  (let ((p (make <process-input-port>
                 name: cmd
                 underlying-input-port: (filedes->input-port fd #t)
                 process: proc)))
    (register-for-finalization p)
    p))


(define (make-process-output-port fd proc cmd)
  (let ((p (make <process-output-port>
                 name: cmd
                 underlying-output-port: (filedes->output-port fd #t)
                 process: proc)))
    (register-for-finalization p)
    p))

;; returns a port which accesses the output of the command

(define (run->port cmd . args)
  (bind ((r w (pipe))
	 (p (run* cmd args (vector 0 w 2))))
    (fd-close w)
    (values (make-process-input-port r p cmd) p)))

;; returns a port which feeds the input of the command

(define (port->run cmd . args)
  (bind ((r w (pipe))
	 (p (run* cmd args (vector r 1 2))))
    (fd-set-blocking w #f)
    (fd-close r)
    (values (make-process-output-port w p cmd) p)))

;;;  Returns two ports, the first feeds the input of the command,
;;;  the second accesses the output of the command.  
;;;
;;;  *NOTE*  Use with care, since you can get deadlocked
;;;  with the subprocess.  Currently, RScheme will eagerly read
;;;  from the subprocess's output, buffering data in the input port
;;;  returned as the second value.  However, that may change at some
;;;  point in which case the caller had better make sure the process
;;;  can deliver enough output to get to the point of reading more input,
;;;  whatever that means for the particular process in question.

(define (port->run->port cmd . args)
  (bind ((r1 w1 (pipe))
	 (r2 w2 (pipe))
         (p (run* cmd args (vector r1 w2 2))))
    (fd-set-blocking w1 #f)
    (fd-close r1)
    (fd-close w2)
    (values (make-process-output-port w1 p cmd)
            (make-process-input-port r2 p cmd)
            p)))

;;;
;;;  install the hooks
;;;

(%early-once-only
 (set-process-io-proc! 'open-input-process open-input-process/thread)
 (set-process-io-proc! 'open-output-process open-output-process/thread))
