
(define *subprocess-table* #f)

#|
 *  This is used to run scheme-level operations detected in kernel mode.
 *
 *  The procedures are actually _run_ in kernel mode, so limited
 *  functionality is available (ie, there is no current-thread,
 *  so you can't try to block!)
 *
 *  Is this used for anything besides running error-proc's for
 *  <queued-output-port>?
 |#

(define (kernel fns)
  (let loop ((f fns))
    (if (pair? f)
	(begin
	  ((car f))
	  (loop (cdr f)))
	(kernel-next-thread))))

(define-glue (kernel-next-thread)
{
  return dispatch_to_next_thread();
})

(define-glue (get-current-thread)
{
  REG0 = current_thread;
  RETURN1();
})

(define-glue (current-thread-group)
{
  REG0 = gvec_ref( current_thread, THREAD_GROUP );
  RETURN1();
})

;; if we make the glue function be `current-thread', then the C
;; name will conflict with the variable/macro current_thread

(define-syntax (current-thread)
  (get-current-thread))

;;;


;;; Note that we silently ignore requests to suspend a complete thread
;;; This is so that the application can suspend the thread to get
;;; it into a stable state and then examine it.  Otherwise, there
;;; would be a race condition between when the app checked the state 
;;; to see if it was "suspendable" and when the app actually suspended
;;; the thread.

(define-safe-glue (thread-suspend (thread <thread>))
{
  if (!EQ(gvec_ref(thread,THREAD_STATE),int2fx(TSTATE_COMPLETE)))
  {
    mark_thread_suspended( thread );
  }
  RETURN1();
})

;;; In contrast, it is an error to resume a completed thread

(define-safe-glue (thread-resume (thread <thread>))
{
  if (EQ(gvec_ref(thread,THREAD_STATE),int2fx(TSTATE_COMPLETE)))
  {
    scheme_error( "thread-resume: cannot resume completed thread ~s", 
                  1, thread );
  }
  mark_thread_resumed( thread );
  RETURN1();
})

(define-safe-glue (thread-suspend-and-unqueue (thread <thread>))
{
  kthread_unqueue_suspend( thread );
  RETURN0();
})

(define (start-threads lst)
  (for-each thread-resume lst)
  (kernel-next-thread)) ;; never returns

;;;

(define-class <thunkified-combo> (<function>)
  (procedure type: <function>)
  (args type: <list> init-value: '()))

(define-safe-glue (thunkified-combo) :template
{
  obj self = envt_reg;
  REG0 = gvec_ref( self, SLOT(2) );
  arg_count_reg = 1;
  arg_count_reg = expand_last();
  APPLYF(arg_count_reg,gvec_ref( self, SLOT(1) ));
})

;;;

(define-class <uncaught-exception> (<condition>)
  uncaught-exception-reason)

(define-method interpret-join-result ((self <condition>) (on <thread>))
  (signal self))

(define-method interpret-join-result ((self <list>) (on <thread>))
  (list->values self))

(define-safe-glue (thread-join (t <thread>))
  literals: ((& interpret-join-result))
{
  if (EQ(gvec_ref(t,THREAD_STATE),int2fx(TSTATE_COMPLETE))) {
    REG1 = t;
    REG0 = gvec_ref( t, THREAD_STACK );
    APPLYG( 2, TLREFB(0) );
  } else {
    REG1 = t;
    SAVE_CONT2( delayed_join );
    gvec_set( t, 
	      THREAD_JOINS, 
	      cons( current_thread, gvec_ref( t, THREAD_JOINS ) ) );
    SWITCH_THREAD( t, TSTATE_BLOCKED );
  }
}

("delayed_join" {
  /* we get resumed with SAVED REG0 => values */
  RESTORE_CONT2();
  /* ignore the continued REG0: reload the THREAD_STACK slot  */
  REG0 = gvec_ref( REG1, THREAD_STACK );
  APPLYG( 2, TLREFB(0) );
})
)

(define-glue (thread-entry) :template
{
  RESTORE_CONT0();
  SAVE_CONT0(thread_done);
  APPLY(0,envt_reg);
}
("thread_done" {
  obj p;
  COLLECT0();
  RESTORE_CONT0();

  /* save the return values in the `thread-stack' slot */
  gvec_set( current_thread, THREAD_STACK, REG0 );

  /* clear out other slots whose values are no longer needed */
  gvec_set( current_thread, THREAD_VARS, FALSE_OBJ );
  gvec_set( current_thread, THREAD_DYNAMIC_STATE, FALSE_OBJ );

  if (DEBUG_THREAD_SWITCH)
    printf( " [%s] thread is done\n", thread_name(current_thread) );

  /* krelease_joiners() actually stores *our* REG0
     in the join's continuation */
  REG0 = FALSE_OBJ;
  krelease_joiners( current_thread );
  SWITCH_THREAD( ZERO, TSTATE_COMPLETE );
}))

;;  The second argument to `halt-thread' is set as the exit (final)
;;  value of the thread.  For successful exits, this should be a list
;;  representing the zero or more return values.  For failures,
;;  this should be a <condition> object.

(define-safe-glue (halt-thread (thr <thread>) ret)
{
  obj t = thr;

  if (EQ( gvec_ref( t, THREAD_STATE ), int2fx( TSTATE_COMPLETE ))) {
    /* The thread has already completed */
    RETURN0();
  } else {
    /* Set up the completion value */

    gvec_set( t, THREAD_STACK, ret );

    /* krelease_joiners() actually stores *our* REG0
       in the join's continuation */
    REG0 = FALSE_OBJ;
    krelease_joiners( t );

    if (EQ(t,current_thread)) {
      SWITCH_THREAD( ZERO, TSTATE_COMPLETE );
    } else {
      /* We are being called from a different thread...
         Remove the target thread from wherever it might
         be queued up
         */

      kthread_unqueue_suspend( t );
      gvec_set( t, THREAD_STATE, int2fx( TSTATE_COMPLETE ) );
      RETURN0();
    }
  }
})

(define-safe-glue (%apply-on-thread t (thunk <function>))
  literals: ((& <partial-continuation>))
{
  obj jmpa = JUMP_ADDR_TO_OBJ( LABEL_TO_JUMP_ADDR( thread_applier ) );
  obj jmpa2 = JUMP_ADDR_TO_OBJ( LABEL_TO_JUMP_ADDR( thread_retrier ) );
  obj sp, new_stack;

  /* eq? case handled by Scheme wrapper */
  assert( !EQ( t, current_thread ) );   
  
  if (((enum thread_state)fx2int( gvec_ref( t, THREAD_STATE ) )) 
      == TSTATE_COMPLETE) {
    REG0 = FALSE_OBJ;
    RETURN1();
  }
  

  sp = gvec_read( t, THREAD_STACK );

  /* Continuation that will invoke retrier */

  new_stack = alloc( SLOT(6), partcont_class );

  gvec_write_init( new_stack, SLOT(0), thunk );
  gvec_write_init( new_stack, SLOT(1), literals_reg );
  gvec_write_init( new_stack, SLOT(2), jmpa2 );
  gvec_write_init( new_stack, SLOT(3), sp );
  gvec_write_init( new_stack, SLOT(4), gvec_read( t, THREAD_STATE ) );
  gvec_write_init( new_stack, SLOT(5), gvec_read( t, THREAD_BLOCKED_ON ) );
  sp = new_stack;

  /* Continuation that will invoke thread_applier */

  new_stack = alloc( SLOT(4), partcont_class );

  gvec_write_init( new_stack, SLOT(0), thunk );
  gvec_write_init( new_stack, SLOT(1), literals_reg );
  gvec_write_init( new_stack, SLOT(2), jmpa );
  gvec_write_init( new_stack, SLOT(3), sp );
  sp = new_stack;

  gvec_write( t, THREAD_STACK, sp );

  kthread_unqueue_suspend( t );
  mark_thread_resumed( t );
  REG0 = TRUE_OBJ;
  RETURN1();
}

("thread_applier" {
  RESTORE_CONT0();
  APPLYF( 0, envt_reg );
})

("thread_retrier" {
  RESTORE_CONT2();
  scheme_error( "thread retry not implemented for ~s", 1, REG0 );
  RETURN0();
}))

(define (thread-deliver-signal! (target <thread>) (condition <condition>))
  (if (eq? target (current-thread))
      (signal condition)
      (if (not (%apply-on-thread target (lambda () (signal condition))))
          (error "thread ~s already completed" target))))

(define-glue (time-slice-over)
{
  if (DEBUG_THREAD_SWITCH)
     printf( " [%s] thread interrupted by timer\n", 
	     thread_name(current_thread) );
  return did_timeout();
})

;;;


(%early-once-only
 ;; these classes are copied into thread_sys_root[] (see struct.h)
 ;; by the code in dispatch.c:init_threads()
 (define *thread-sys-classes*
   (vector <thread-queue>         ;; 0
	   <thunkified-combo>     ;; 1
	   <thread>               ;; 2
	   <semaphore>            ;; 3
	   <queued-output-port>   ;; 4
	   <mailbox>              ;; 5
           <initiator-socket>     ;; 6
           <process>              ;; 7
           <future>               ;; 8
	   )))

(define-glue (init-threads-glue)
  literals: ((& *thread-sys-classes*)
	     (& kernel)
	     (& thunkified-combo))
{
  init_threads( TLREF(0), TLREF(1), TLREF(2) );
  RETURN0();
})

(define (thread-backstop-handler condition next)
  (let ((ds (get-dynamic-state-reg))
        (ds0 (thread-initial-dynamic-state (current-thread))))
    (do-unwind (get-dynamic-state-reg) (find-common-ancestor ds ds0))
    (halt-thread (current-thread) 
                 (make <uncaught-exception>
                       uncaught-exception-reason: condition))))


(define (init-threads)
  (set! *thread-table* (make-fixnum-table))
  (set! *num-threads-alloced* 0)
  (set! *subprocess-table* (make-fixnum-table))
  (init-threads-glue))
  
;;; initialize threads during system startup
;;; (note that we don't actually START threads yet)

(init-threads)
