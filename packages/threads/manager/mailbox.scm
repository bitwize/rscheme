(define (make-mailbox . name)
  (%make <mailbox>
	 (%make <vector> #f #f #f #f #f)                ; DEQ state
	 0                                              ; DEQ front
	 0                                              ; DEQ back
	 #t                                             ; MBOX has-data?
	 (and (pair? name) (car name))))                ; MBOX name

(define (mailbox-empty? (self <mailbox>))
  (dequeue-empty? self))

(define (mailbox-has-waiters? (self <mailbox>))
  (not (has-data? self)))

(define (mailbox-has-data? (self <mailbox>))
  (and (has-data? self)
       (not (dequeue-empty? self))))

(define-method write-object ((self <mailbox>) port)
  (format port "#[<mailbox> ~s]" (name self)))

(define-safe-glue (send-message/prepend! (mbox <mailbox>) item)
{
  int rc = ksend_mailbox_pre( mbox, item );

  if (rc == 0) {
    REG0 = TRUE_OBJ;
  } else {
    REG0 = FALSE_OBJ;
  }
  RETURN1();
})

(define-safe-glue (close-mailbox (mbox <mailbox>) leave)
{
  close_mailbox( mbox, leave );
  RETURN0();
})

(define-safe-glue (send-message! (mbox <mailbox>) item)
{
  int rc = ksend_mailbox( mbox, item );

  if (rc == 0) {
    REG0 = TRUE_OBJ;
  } else {
    REG0 = FALSE_OBJ;
  }
  RETURN1();
})

(define-safe-glue (receive-message-if-present! (mbox <mailbox>))
{
  if (!EQ(gvec_ref(mbox,MAILBOX_HAS_DATA_Q),FALSE_OBJ) && !dequeue_empty(mbox))
    {
      obj item = dequeue_pop_front(mbox);
      if (DEBUG_THREAD_MBOX)
	printf( " [%s] extracted item{%#lx} from mailbox{%#lx}\n",
		thread_name(current_thread), VAL(item), VAL(mbox) );
      REG0 = item;
      REG1 = TRUE_OBJ;
    }
  else
    {
      REG0 = FALSE_OBJ;
      REG1 = FALSE_OBJ;
    }
  RETURN(2);
})

(define-safe-glue (receive-message! (mbox <mailbox>))
{
  obj state = gvec_ref(mbox,MAILBOX_HAS_DATA_Q);

  if (!EQ( state, FALSE_OBJ ) && !dequeue_empty(mbox)) {
      obj item = dequeue_pop_front(mbox);
      if (DEBUG_THREAD_MBOX)
	printf( " [%s] extracted item{%#lx} from mailbox{%#lx}\n",
		thread_name(current_thread), VAL(item), VAL(mbox) );
      REG0 = item;
      RETURN1();
  } else if (BOOLEAN_P( state )) {
      if (DEBUG_THREAD_MBOX)
	 printf( " [%s] blocking on mailbox{%#lx}\n",
	         thread_name(current_thread), VAL(mbox) );
      gvec_write_non_ptr(mbox,MAILBOX_HAS_DATA_Q,FALSE_OBJ);
      dequeue_push_back(mbox,current_thread);
      REG1 = REG0;
      SAVE_CONT2(rcv_n_go);
      /* note... mbox::=REG0 still has mbox, but in REG1 too for later */
      SWITCH_THREAD(mbox,TSTATE_BLOCKED);
  } else if (FUNCTION_P( state )) {
    APPLYF( 0, state );
  } else {
    REG0 = FALSE_OBJ;
    RETURN1();
  }
}
("rcv_n_go" {
  /* our continuation's REG0 got filled in with the item */
  RESTORE_CONT2();
  if (DEBUG_THREAD_MBOX)
    printf( " [%s] mailbox{%#lx}: item{%#lx} delivered\n",
	    thread_name(current_thread), VAL(REG1), VAL(REG0) );
  if (EQ(REG0,NOVALUE_OBJ)) {
    obj state = gvec_ref( REG1, MAILBOX_HAS_DATA_Q );
    if (FUNCTION_P(state)) {
      APPLYF( 0, state );
    } else {
      REG0 = FALSE_OBJ;
      RETURN1();
    }
  } else {
    RETURN1();
  }
}))

(define-method close ((self <mailbox>) #optional thunk)
  (close-mailbox self (or thunk 0)))
