;;;
;;;  A semaphore is implemented as a queue of waiters
;;;  plus a counter, which if non-negative is the number
;;;  of signals
;;;

(define-syntax make-semaphore
  (syntax-form (name n)
    (make <semaphore>
          state: (%make <vector> #f #f)
          count: n
	  name: name))
  (syntax-form (n) (make-semaphore #f n))
  (syntax-form ()
    (make-semaphore #f 0))
  (else
   make-semaphore.))

(define-method write-object ((self <semaphore>) port)
  (format port "#[<semaphore> ~s ~a]" (name self) (count self)))

(define (make-semaphore. #optional (n default: 0))
  (make-semaphore n))

(define-safe-glue (semaphore-signal (s <semaphore>))
{
  obj n = gvec_ref( s, SEMAPHORE_COUNT );

  gvec_write_non_ptr( s, SEMAPHORE_COUNT, ADD1(n) );
  if (FX_LT( n, ZERO ))
   {
     obj thr = dequeue_pop_front( s );

     /* the top thread is no longer blocked... */
     UNBLOCK_THREAD( thr );

     mark_thread_ready( thr );
   }
  RETURN0();
})

(define-safe-glue (semaphore-wait (s <semaphore>))
{
  obj n = SUB1( gvec_ref( s, SEMAPHORE_COUNT ) );
  gvec_write_non_ptr( s, SEMAPHORE_COUNT, n );
  if (FX_LT(n,ZERO))
   {
     dequeue_push_back( s, current_thread );
     SAVE_CONT0(sem_ok);
     SWITCH_THREAD( s, TSTATE_BLOCKED );
   }
  else
   {
     RETURN0();
   }
}
("sem_ok" {
   RESTORE_CONT0();
   RETURN0();
}))

(define-syntax (with-semaphore s . body)
  (dynamic-wind
      (lambda () (semaphore-wait s))
      (lambda () (begin . body))
      (lambda () (semaphore-signal s))))
