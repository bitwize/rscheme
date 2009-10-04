
(define-safe-glue (thread-sleep-ms (ms <raw-int>))
{
  struct sys_time t;
  struct sys_event *e;

  get_sys_time(&t);

  if (ms > 0)
    {
      t.usec += (ms % 1000) * 1000;
      t.sec += ms / 1000;
      if (t.usec > 1000000)
	{
	  t.usec -= 1000000;
	  t.sec++;
	}
      e = make_time_event( t, current_thread );
      SWITCH_THREAD( SYS_EVENT_TO_OBJ(e), TSTATE_SLEEPING );
    }
  else if (ms == 0)
    {
      mark_thread_ready( current_thread );
      SWITCH_THREAD( ZERO, TSTATE_WAITING );
    }
  else
    RETURN0();
})

(define-method thread-sleep ((self <fixnum>))
  (thread-sleep-ms (fixnum* self 1000)))

(define-method thread-sleep ((self <double-float>))
  (thread-sleep-ms (float-round (float* self 1000.0))))

