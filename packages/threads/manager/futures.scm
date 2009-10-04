
;;;  a `future' is the multi-threaded analogue of a promise,
;;;  and is your basic latching response structure
;;;
;;;  repeated set-future-values of a <future> are silently ignored,
;;;  making this a convenient mechanism for union joins

;;;  (the analogies between a <future> and the result values
;;;  of a <thread> indicate that the two concepts should be
;;;  unified -- either a <future> should simply be a wrapper
;;;  for a thread, or, if explicit `set-future-values!' is 
;;;  desired, then a <thread> should use a <future> to broker
;;;  its exit/join values)

(define-class <future> (<object>)
  (future-waiters type: <list> init-value: '())
  (computed-values init-value: #f)
  (name init-value: #f))

(define-method write-object ((self <future>) port)
  (format port "#[<future>")
  (if (name self)
      (format port " ~s" (name self))
      (format port " @~a" (machine-bits->string self)))
  (format port "]"))

;;;

(define (make-future #optional name)
  (make <future>
        name: name))

(define (thunk->future thunk #optional name)
  (let ((f (make <future>
                 name: name)))
    (thread-resume
     (make-thread (lambda ()
		    (set-future-values-from-thunk f thunk))
		  (if name
                      (if (string? name)
                          name
                          (format #f "future:~@#*20a" name))
                      "future")))
    f))

(define-syntax future 
  (syntax-form (expr)
    (thunk->future
     (lambda ()
       expr)
     (mquote expr)))
  (syntax-form (expr name)
    (thunk->future
     (lambda ()
       expr)
     name)))
    

;;;

(define-safe-glue (set-future-values! (f <future>) #rest)
{
  obj w;

  COLLECT1();
  if (EQ( gvec_ref( f, SLOT( 1 ) ), FALSE_OBJ ))
   {
     gvec_set( f, SLOT( 1 ), REG1);
     for (w = gvec_ref( f, SLOT( 0 ) ); !NULL_P( w ); w = pair_cdr( w ))
      {
        obj thr = pair_car( w );
        /* the thread is no longer blocked, and mark it ready */
        UNBLOCK_THREAD( thr );
        mark_thread_ready( thr );
      }
     gvec_set( f, SLOT( 0 ), NIL_OBJ );
   }
  RETURN0();
})

(define-safe-glue (set-future-values-from-thunk (f <future>) 
						(thunk <function>))
{
  SAVE_CONT1(done_w_thunk);
  APPLYF(0,thunk);
}
("done_w_thunk" {
  obj r, w;

  COLLECT0();
  r = REG0;
  RESTORE_CONT1();

  if (EQ(gvec_ref(REG0,SLOT(1)),FALSE_OBJ))
    {
      gvec_set( REG0, SLOT(1), r );

      for (w = gvec_ref( REG0, SLOT(0) ); !NULL_P(w); w=pair_cdr(w))
        {
          obj thr = pair_car( w );
          /* the thread is no longer blocked, and mark it ready */
          UNBLOCK_THREAD( thr );
          mark_thread_ready( thr );
        }
      gvec_set( REG0, SLOT(0), NIL_OBJ );
    }
  RETURN0();
}))

(define-safe-glue (await-future (f <future>))
{
  if (EQ(gvec_ref(f,SLOT(1)),FALSE_OBJ))
   {
     gvec_set( f, SLOT(0), cons( current_thread, gvec_ref(f,SLOT(0)) ) );
     SAVE_CONT1(future_did_arrive);
     SWITCH_THREAD( f, TSTATE_BLOCKED );
   }
  else
   {
     REG0 = gvec_ref( f, SLOT(1) );
     arg_count_reg = expand_last();
     if (arg_count_reg == 0) {
       RETURN0();
     } else {
       RETURN(arg_count_reg);
     }
   }
}
("future_did_arrive" {
  RESTORE_CONT1();
  REG0 = gvec_ref( REG0, SLOT(1) );
  arg_count_reg = 1;
  arg_count_reg = expand_last();
  if (arg_count_reg == 0) {
    RETURN0();
  } else {
    RETURN(arg_count_reg);
  }
}))

;;; overload `force' to work on futures

(define-method force ((self <future>))
  (await-future self))

