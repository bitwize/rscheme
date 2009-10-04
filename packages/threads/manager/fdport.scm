
(define-safe-glue (make-read-event (fd <raw-int>) (port <mbox-input-port>))
{
  REG0 = RAW_PTR_TO_OBJ( make_read_event( fd, port ) );
  RETURN1();
})

(define-safe-glue (make-recv-event (fd <raw-int>) (port <packet-input-port>))
{
  REG0 = RAW_PTR_TO_OBJ( make_recv_event( fd, port ) );
  RETURN1();
})


(define-safe-glue (free-read-event* owner (slot <fixnum>))
{
  obj evt; 
  struct sys_event *p;

  /* atomically extract and make sure the event pointer is non-NULL */

  assert( GVEC_P( owner ) );
  evt = gvec_ref( owner, FXWORDS_TO_RIBYTES( slot ) );
  p = (struct sys_event *)OBJ_TO_RAW_PTR(evt);
  if (p) {
    free_read_event(p);
  }
  RETURN0();
})

(define-safe-glue (thread-fd-select ms (set <fd-select-set>))
  literals: ((& <select-event>) 'fdset 'timeout)
{
  struct sys_event *e;
  struct sys_time *pt = NULL, t;
  obj se;

  if (truish( ms )) {
    if (EQ( ms, ZERO )) {
      t.usec = 0;
      t.sec = 0;
    } else {
      get_sys_time( &t );
      t.usec += (fx2int( ms ) % 1000) * 1000;
      t.sec += fx2int( ms ) / 1000;
      if (t.usec > 1000000) {
        t.usec -= 1000000;
        t.sec++;
      }
    }
    pt = &t;
  }

  se = make3( TLREF(0), 
              make4( vector_class,
                     LITERAL(1), set,
                     LITERAL(2), ms ),
              ZERO,
              current_thread );

  e = make_select_event( (fd_set *)PTR_TO_DATAPTR( set ), pt, NIL_OBJ, se );

  SAVE_CONT1( select_fin );
  SWITCH_THREAD( se, TSTATE_BLOCKED );
}
("select_fin" {
  obj x;

  RESTORE_CONT1();
  x = REG0;
  REG0 = gvec_ref( x, SLOT(0) );        /* read ready list */
  REG1 = gvec_ref( x, SLOT(1) );        /* write ready list */
  REG2 = gvec_ref( x, SLOT(2) );        /* exception list */
  REG3 = gvec_ref( x, SLOT(3) );        /* timeout? */
  RETURN(4);
}))


