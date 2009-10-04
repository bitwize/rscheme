#|------------------------------------------------------------*-Scheme-*--|
 | File:	    packages/threads/shell/taskmgr.scm
 |
 |          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |	    See <http://www.rscheme.org/> for the latest info.
 |
 | File version:     1.2
 | File mod date:    2003-12-15 09:47:04
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  rs.sys.threads.shell
 |
 | Purpose:          Task management in a multi-threaded environment
 `------------------------------------------------------------------------|#

;;; we need to merge the notion of <task> and <session>
;;; with the REPL's idea of <cmd-loop> -- they have some
;;; functionality overlap

(define-class <task> (<object>)
  (console-in type: <input-port>)
  (console-out type: <output-port>)
  (group type: <thread-group>)
  (top-level-envt type: <top-level-contour>))

(define-class <session> (<object>)
  (session-task type: <task>)
  ;; this might want to be different if the process is managing
  ;; different "logins", e.g., as a telnet server.  Then some
  ;; low-level mechanism can turn a ^C along telnet protocol into
  ;; a signal to this mbox
  (interrupt-mbox type: <mailbox>))

(define-thread-var *current-session* #f)

(define (current-session)
  *current-session*)

(define (current-task)
  (session-task *current-session*))

(define (monitor-exception-mbox)
  (let ((box (interrupt-mbox (current-session))))
    (let loop ((k 0))
      (let* ((intr (receive-message! box))
	     (old-t (current-task))
	     (port (console-out old-t)))
	(format port "\n** ~d: Interrupt received: ~s\n" k intr)
	;; halt all threads in the current task
	
	(let ((suspension-list (thread-group-suspend (group old-t)))
              (new-t (make <task>
			   top-level-envt: (top-level-envt old-t)
			   console-in: (console-in old-t)
			   console-out: port
			   group: (make <thread-group>
					parent-group: (group old-t)))))
	  (set-session-task! (current-session) new-t)
	  (thread-resume
	   (make-thread* (lambda ()
			   (cmd-loop (top-level-envt old-t) "break[~d]=>")
			   (format port "** ~d: Resuming\n" k)
			   (set-session-task! (current-session) old-t)
                           (for-each thread-resume suspension-list)
			   ;; force a new `read' event if the previous
			   ;; one was lost due to an EOF
			   (more-input-ready? (console-in old-t)))
			 "break"
			 (group new-t)))
	  (loop (+ k 1)))))))

(define (thread-group-suspend (self <thread-group>))
  ;; if we don't capture the list of threads now as non-weak pointers,
  ;; then it may so happen that all these threads get GC'd, and we won't
  ;; be able to restart them!
  (let ((lst (thread-group-members self)))
    (for-each thread-suspend lst)
    lst))

(define (thread-group-resume (self <thread-group>))
  (for-each thread-resume (thread-group-members self)))

;;;

(define (make-new-session envt)
  (make <session>
	interrupt-mbox: *interrupt-mbox*
	session-task: (make <task>
			    top-level-envt: envt
			    console-in: (current-input-port)
			    console-out: (current-output-port)
			    group: (current-thread-group))))
  
(define (start-threads-repl envt)
  ;;
  (thread-let ((*current-session* (make-new-session envt)))
    (thread-resume
     (make-thread* monitor-exception-mbox
		   "monitor"
		   (make <thread-group>)))
    ;;
    (handler-bind (<condition> repl-condition-handler)
      (register-interrupt-handler! 'control-c do-user-intr)
      (cmd-loop envt $default-prompt))))

;; register our `start-repl' procedure to override the
;; basic (non-threads-aware) one in base RScheme

(set-start-repl-proc! start-threads-repl)

