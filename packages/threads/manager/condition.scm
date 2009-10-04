(define-class <condition-var> (<dequeue>)
  (name init-value: #f))

(define-method write-object ((self <condition-var>) port)
  (format port "#[<condition-var> ~s]" (name self)))

(define (make-condition-var #optional name)
  (make <condition-var>
        name: name
        state: (%make <vector> #f #f)))

;;

(define (condition-test-and-alter (sem <semaphore>)
                                  (cvar <condition-var>) 
                                  test
                                  #optional alter)
  (semaphore-wait sem)
  (let loop ()
    (let ((temp (test)))
      (if temp
          (begin
            (if alter
                (begin
                  (alter)
                  (condition-signal cvar)))
            (semaphore-signal sem)
            temp)
          (begin
            (condition-wait sem cvar)
            (loop))))))

(define-safe-glue (condition-signal (cv <condition-var>))
{
  while (!dequeue_empty(cv))
    {
      obj thr = dequeue_pop_front( cv );

      /* the thread is no longer blocked */
      UNBLOCK_THREAD( thr );
      mark_thread_ready( thr );
    }
  RETURN0();
})

(define-safe-glue (condition-wait (sem <semaphore>) (cv <condition-var>))
  literals: ((& semaphore-wait))
{
  /* XXX<TODO>  Factor this out into a C-level rs_sem_signal() */

  obj n = gvec_ref( sem, SEMAPHORE_COUNT );

  gvec_write_non_ptr( sem, SEMAPHORE_COUNT, ADD1(n) );
  if (FX_LT( n, ZERO ))
   {
     obj thr = dequeue_pop_front( sem );
     /* the top thread is no longer blocked... */
     UNBLOCK_THREAD( thr );
     mark_thread_ready( thr );
   }

  dequeue_push_back( cv, current_thread );
  SAVE_CONT1( cw_resume );
  SWITCH_THREAD( cv, TSTATE_BLOCKED );
}
("cw_resume" {
  RESTORE_CONT1();
  /* try to grab lock quickly here? */
  APPLY( 1, TLREF(0) );
}))
