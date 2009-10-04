(define-class <process> (<object>)
  (name type: <string> init-value: "")
  (process-id type: <fixnum> init-value: 0)
  (the-exit-status init-value: #f)
  (status-waiters type: <list> init-value: '()))

(define-method write-object ((self <process>) port)
  (format port "#[~a pid ~d" 
          (name (object-class self)) 
          (process-id self))
  (let ((x (the-exit-status self)))
    (if x
        (case (vector-ref x 0)
          ((exited)
           (format port " (rc ~d)" (vector-ref x 1)))
          ((signalled)
           (let ((sn (signal-number->name (vector-ref x 1))))
             (if sn
                 (format port " (~a)" sn)
                 (format port " (sig ~d)" (vector-ref x 1)))))))
    (write-string port "]")))


(define-glue (fork-and-exec proc path argv dir envv fdv pgrp)
  literals: ((& *subprocess-table*))
{
  obj pid;
  pid = rs_fork_and_exec( proc, path, argv, envv, fdv, dir, pgrp );

  gvec_write_non_ptr( proc, PROCESS_PID, pid );
  objecttable_insert( TLREF(0), rehash_fixnum(pid), pid, proc );
  REG0 = pid;
  RETURN1();
})

(define-glue (do-child-exited pid type code)
  literals: ((& *subprocess-table*))
{
  obj w, h = rehash_fixnum(pid);
  obj proc = objecttable_remove( TLREF(0), h, pid );
  if (!EQ(proc,FALSE_OBJ))
   {
     gvec_set( proc, PROCESS_EXIT_STATUS, make2( vector_class, type, code ) );
     w = gvec_ref( proc, PROCESS_STATUS_WAITERS );
     while (!EQ(w,NIL_OBJ))
       {
         obj thr = pair_car( w );
         UNBLOCK_THREAD( thr );
         /* 
          *  If a thread which was waiting on this process
          *  is suspended, then, since it is no longer blocked
          *  on anything, when it is resumed it will be marked
          *  ready.  If it isn't suspended, then mark it ready
          *  now.
          */
         if (EQ( gvec_ref( thr, THREAD_SUSPEND_COUNT ), ZERO )) {
            mark_thread_ready( thr );
         }
         w = pair_cdr(w);
       }
     gvec_set( proc, PROCESS_STATUS_WAITERS, NIL_OBJ );
   }
  RETURN0();
})

(define-safe-glue (exit-status (proc <process>))
{
  obj stat = gvec_ref( proc, PROCESS_EXIT_STATUS );
  if (EQ(stat,FALSE_OBJ))
   {
     gvec_write_ptr( proc, PROCESS_STATUS_WAITERS, 
		     cons( current_thread, 
			   gvec_ref( proc, PROCESS_STATUS_WAITERS )));
     SAVE_CONT1( subproc_did_exit );
     SWITCH_THREAD( proc, TSTATE_BLOCKED );
   }
  else
   {
     REG0 = gvec_ref( proc, PROCESS_EXIT_STATUS );
     RETURN1();
   }
}
("subproc_did_exit" {
   RESTORE_CONT1();
   REG0 = gvec_ref( REG0, PROCESS_EXIT_STATUS );
   RETURN1();
}))

;;; condition describing a failed process
;;; the `the-exit-status' slot of the given process
;;; is not #(exited 0)

(define-class <process-failed> (<condition>)
  (failing-process type: <process>))

;;; checks to make sure the exit status is #(exited 0)
;;; signals an error if it isn't, otherwise returns no values
;;; if the process hadn't exited yet, waits for it do so

(define (check-exit-status (process <process>))
  (if (exit-success? (exit-status process))
      (values)
      (signal (make <process-failed>
                    failing-process: process))))

(define (exit-success? (status <vector>))
  (if (and (eq? (vector-ref status 0) 'exited)
           (eq? (vector-ref status 1) 0))
      #t
      #f))

(define-method write-object ((self <process-failed>) port)
  (format port "#[<process-failed> ~a ~s]"
	  (name (failing-process self))
	  (the-exit-status (failing-process self))))

(define-method display-object ((self <process-failed>) port)
  (format port "Process `~a' failed: " (name (failing-process self)))
  (let (((v <vector>) (the-exit-status (failing-process self))))
    (case (vector-ref v 0)
      ((exited)
       (format port "exited with return code ~d" (vector-ref v 1)))
      ((signalled)
       (format port "died on signal ~a" 
	       (or (signal-number->name (vector-ref v 1))
		   (number->string (vector-ref v 1)))))
      (else
       ;; this shouldn't happen!
       (write v port)))
    ;; by convention, display-object on a <condition> ends with a newline
    (newline port)))

