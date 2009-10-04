#line 2 "synch.c"
#include "rs_sys_threads_manager_p.h"

static void requeue( obj thr, obj blocked_on );


void mark_thread_suspended( obj th )
{
  obj susp_count = gvec_ref( th, THREAD_SUSPEND_COUNT );
  int still_in_q = 1;

  if (FX_LT( susp_count, ZERO ))
    {
      still_in_q = 0;
      susp_count = FX_SUB( ZERO, susp_count );
    }
  else if (EQ( susp_count, ZERO ))
    {
      /* newly suspended */
      gvec_write_non_ptr( th, THREAD_STATE, int2fx( TSTATE_SUSPEND ) );
    }

  susp_count = ADD1( susp_count );

  if (still_in_q)
    gvec_write_non_ptr( th, 
			THREAD_SUSPEND_COUNT, 
			susp_count );
  else
    gvec_write_non_ptr( th, 
			THREAD_SUSPEND_COUNT, 
			FX_SUB(ZERO,susp_count) );
}

void mark_thread_resumed( obj th )
{
  obj susp_count = gvec_ref( th, THREAD_SUSPEND_COUNT );
  int still_in_q = 1;

  if (FX_LT( susp_count, ZERO ))
    {
      still_in_q = 0;
      susp_count = FX_SUB( ZERO, susp_count );
    }
  else if (EQ( susp_count, ZERO ))
    {
      /* do nothing */
      return;
    }

  susp_count = SUB1( susp_count );

  if (EQ( susp_count, ZERO ))
    {
      obj blkd_on = gvec_ref( th, THREAD_BLOCKED_ON );

      gvec_write_non_ptr( th, THREAD_SUSPEND_COUNT, ZERO );

      /* unblock it */
      if (EQ( blkd_on, ZERO ))
	{
	  /* if it IS still in the queue, then we need do nothing */

	  if (!still_in_q)
	    mark_thread_ready( th );
	}
      else if (FIXNUM_P( blkd_on ))
	{
	  /* it's an Event, so it must have been sleeping *and*
	     the timer hasn't gone off (see comments in `class.scm')
	   */
	  assert( still_in_q );
	  gvec_set( th, THREAD_STATE, int2fx( TSTATE_SLEEPING ) );
	}
      else
	{
	  /* it must be some other object, like a <mailbox> */
	  if (!still_in_q)
	    {
	      /* put it back in the queue */
	      requeue( th, blkd_on );
	    }
	}
    }
  else
    {
      /* still suspended */

      if (still_in_q)
	gvec_write_non_ptr( th, 
			    THREAD_SUSPEND_COUNT, 
			    susp_count );
      else
	gvec_write_non_ptr( th, 
			    THREAD_SUSPEND_COUNT, 
			    FX_SUB(ZERO,susp_count) );
    }
}

static void send_item_to_thread( obj mbox, obj thr, obj item )
{
  UNBLOCK_THREAD( thr );

  store_resume_value( thr, item );
  mark_thread_ready( thr );
  if (dequeue_empty( mbox ))
    gvec_write_non_ptr( mbox, MAILBOX_HAS_DATA_Q, TRUE_OBJ );
}

/*
 *   requeue a thread on a blockable scheme object, 
 *   which is one of:
 *
 *      <thread>          -- see `thread-join'
 *      <semaphore>       -- see `semaphore-wait'
 *      <fd-output-port>  -- see `fd-output-port-write-string'
 *      <mailbox>         -- see `receive-message!'
 *
 *   Note that we are only called when the thread
 *   being requeued has been removed from the blocking
 *   queue.
 */


static void requeue( obj thr, obj blocked_on )
{
  if (instance_p( blocked_on, mailbox_class ))
    {
      obj mbox = blocked_on;

      /* two cases to consider
       *   1. mailbox has data now
       *   2. mailbox has no data
       */
      if (truish(gvec_ref(mbox,MAILBOX_HAS_DATA_Q)) && !dequeue_empty(mbox))
	{
	  /* mark_thread_ready() will set the state to WAITING */
	  send_item_to_thread( mbox, thr, dequeue_pop_front( mbox ) );
	}
      else
	{
	  /* put it back in the wait queue */
	  gvec_write_non_ptr( mbox, MAILBOX_HAS_DATA_Q, FALSE_OBJ );
	  dequeue_push_back( mbox, thr );
	  /* now, we're blocked again */
	  gvec_set( thr, THREAD_STATE, int2fx( TSTATE_BLOCKED ) );
	}
    }
  else
    {
      /*  it can't be a <queued-output-port>, because we are only
       *  called when the thread has been ejected from the queue,
       *  and that never happens for qout's (they work like timers)
       *  -- see comments in output.c and class.scm
       */
      assert( !instance_p( blocked_on, qout_class ) );
      assert(0); /* not implemented yet... */
    }
}

/*
 *  Remove an item from a list store in an object slot
 */

static int slot_list_delq( obj owner, UINT_32 slot, obj key )
{
  obj p, prev = FALSE_OBJ;
  p = gvec_ref( owner, slot );
  
  while (PAIR_P( p )) {
    if (EQ( pair_car( p ), key )) {
      if (EQ( prev, FALSE_OBJ )) {
        gvec_set( owner, slot, pair_cdr( p ) );
      } else {
        pair_set_cdr( prev, pair_cdr( p ) );
      }
      return 1;
    }
    prev = p;
    p = pair_cdr( p );
  }
  return 0;
}

/*
 *  Remove an item from a thread's join list 
 */

static int thread_joins_list_delq( obj thread, obj item )
{
  return slot_list_delq( thread, THREAD_JOINS, item );
}

static int process_status_list_delq( obj proc, obj item )
{
  return slot_list_delq( proc, PROCESS_STATUS_WAITERS, item );
}

static int future_waiters_list_delq( obj proc, obj item )
{
  return slot_list_delq( proc, FUTURE_FUTURE_WAITERS, item );
}


/*
 *  Force the thread into the suspend state, removing it
 *  from whatever it may be queued on.  Increment suspend
 *  count.
 */

void kthread_unqueue_suspend( obj t )
{
  enum thread_state s = fx2int( gvec_ref( t, THREAD_STATE ) );
  obj blkd_on = gvec_ref( t, THREAD_BLOCKED_ON );
  int did_rm = 0;

  if (EQ( blkd_on, ZERO )) {
    /* 
     *  The thread is not blocked on anything...  It might be
     *  in a waiting queue, though.
     */
    if (s == TSTATE_WAITING) {
      obj q = thread_queue[ fx2int( gvec_ref( t, THREAD_PRIORITY ) ) ];
      int n = dequeue_delq( q, t );
      assert( n == 1 );
      did_rm = 1;
    }
  } else if (FIXNUM_P( blkd_on )) {
    /*
     *  The thread is blocked on a system event (which must necessarily
     *  be a timer event)
     */
    struct sys_event *e = OBJ_TO_SYS_EVENT( blkd_on );
    assert( (s == TSTATE_SUSPEND) || (s == TSTATE_SLEEPING) );
    free_time_event( e );
    did_rm = 1;
  } else if (OBJ_ISA_PTR( blkd_on )) {
    /*
     *  The thread is blocked on some kind of heap object
     */
    obj tbo_class = CLASSOF_PTR( blkd_on );
    obj q;

    assert( (s == TSTATE_SUSPEND) || (s == TSTATE_BLOCKED) );

    if (EQ( tbo_class, mailbox_class )) {
      /*  It's a <mailbox>, which looks like a dequeue.
       *  However, we have to update the `has-data?' slot
       *  if we are removing the last waiting thread.
       */
      q = blkd_on;
      dequeue_delq( q, t );
      if (dequeue_empty( q )) {
        gvec_write_non_ptr( q, MAILBOX_HAS_DATA_Q, TRUE_OBJ );
      }
      did_rm = 1;
    } else if (subclass_p( tbo_class, semaphore_class )) {
      /*
       *  It's a <semaphore>, which looks like a dequeue.
       *  However, we have to update the semaphore-count,
       *  since that describes the number of waiting threads
       */
      obj n;

      q = blkd_on;
      dequeue_delq( q, t );
      n = gvec_ref( q, SEMAPHORE_COUNT );
      gvec_write_non_ptr( q, SEMAPHORE_COUNT, ADD1(n) );
      did_rm = 1;
    } else if (subclass_p( tbo_class, thread_class )) {
      /*  It's a thread, which has a waiters list */
      thread_joins_list_delq( blkd_on, t );
      did_rm = 1;
    } else if (subclass_p( tbo_class, qout_class )) {
      /*  It's a queued output port, which has a pending-writes dequeue */
      q = gvec_ref( blkd_on, QOUT_PENDING_WRITES );
      assert( instance_p( q, dequeue_class ) );
      dequeue_delq( q, t );
      did_rm = 1;
    } else if (EQ( tbo_class, initiator_socket_class )) {
      /* 
       *  It's an <initiator-socket>, so the thread is waiting for
       *  the socket to complete it's connection protocol
       */
      obj ev = gvec_read( blkd_on, INITIATOR_SOCKET_EVENT );
      struct sys_event *e = OBJ_TO_SYS_EVENT( ev );
      assert( e->type == SYS_EVENT_FILE_CONNECT );
      free_connect_event( e );
      gvec_write_non_ptr( blkd_on, INITIATOR_SOCKET_WAITER, FALSE_OBJ );
      did_rm = 1;
    } else if (EQ( tbo_class, process_class )) {
      /*
       *  It's a <process>, so we appear in the PROCESS_STATUS_WAITERS list
       */
      process_status_list_delq( blkd_on, t );
      did_rm = 1;
    } else if (EQ( tbo_class, future_class )) {
      future_waiters_list_delq( blkd_on, t );
      did_rm = 1;
    } else {
      scheme_error( "Thread ~s blocked on ~s ?", 2, t, blkd_on );
    }
  } else {
    scheme_error( "Thread ~s blocked on ~s ???", 2, t, blkd_on );
  }
  if (did_rm) {
    did_remove_from_queue( t );
  }
  UNBLOCK_THREAD( t );

  gvec_write_non_ptr( t, THREAD_STATE, int2fx( TSTATE_SUSPEND ) );
  gvec_write_non_ptr( t, 
                      THREAD_SUSPEND_COUNT,
                      SUB1( gvec_read( t, THREAD_SUSPEND_COUNT ) ) );
}


static int do_mailbox_send( obj mbox, obj item, int front_q )
{
  obj state;

try_again_from_top:

  state = gvec_ref( mbox, MAILBOX_HAS_DATA_Q );
  if (EQ( state, TRUE_OBJ )) {
    /* the mbox queue contains data... */

    if (DEBUG_THREAD_MBOX)
      printf( " mailbox{%#lx}: inserting another item{%#lx}\n",
              VAL(mbox), VAL(item) );
    if (front_q)
      dequeue_push_front( mbox, item );
    else
      dequeue_push_back( mbox, item );
    return 0;
  } else if (EQ( state, FALSE_OBJ )) {
    obj top;

      /* the mbox queue contains threads... */

  try_again_more:
    /*  note that we remove the longest-waiting thread 
       *  independent of whether or not this is a "prepend"
       *  data item...  I hope that's right...!
       */
    top = dequeue_pop_front(mbox);
    /*
       *  We don't want to deliver an object to a suspended
       *  thread, so if we try to remove it from the queue but
       *  it's suspended, then skip it instead and try the
       *  next one.
       */
    if (!did_remove_from_queue( top ))
      {
        if (dequeue_empty(mbox))
          {
            gvec_write_non_ptr( mbox, MAILBOX_HAS_DATA_Q, TRUE_OBJ );
            goto try_again_from_top;
          }
        else
          {
            /* more left in this queue... try another */
            goto try_again_more;
          }
      }

    if (DEBUG_THREAD_MBOX)
      printf( " mailbox{%#lx}: delivering item{%#lx} (%ld left waiting)\n",
              VAL(mbox), VAL(item),
              fx2int( dequeue_count(mbox) ));
    send_item_to_thread( mbox, top, item );
    return 0;
  } else {
    return -EPIPE;
  }
}

int close_mailbox( obj mbox, obj leave )
{
  obj top;

  if (EQ( gvec_ref( mbox, MAILBOX_HAS_DATA_Q ), FALSE_OBJ )) {
    while (!dequeue_empty( mbox )) {
      top = dequeue_pop_front( mbox );
      if (did_remove_from_queue( top )) {
        send_item_to_thread( mbox, top, NOVALUE_OBJ );
      }
    }
    gvec_write_non_ptr( mbox, MAILBOX_HAS_DATA_Q, TRUE_OBJ );
  }
  if (EQ( gvec_ref( mbox, MAILBOX_HAS_DATA_Q ), TRUE_OBJ )) {
    gvec_write( mbox, MAILBOX_HAS_DATA_Q, leave );
    return 0;
  } else {
    return -EPIPE;
  }
}

int ksend_mailbox( obj mbox, obj item )
{
  return do_mailbox_send( mbox, item, 0 );
}

int ksend_mailbox_pre( obj mbox, obj item )
{
  return do_mailbox_send( mbox, item, 1 );
}

/*
 *  Let any threads blocked on this one know that
 *  this thread is now complete
 */

void krelease_joiners( obj t )
{
  obj p;

  for (p=gvec_ref( t, THREAD_JOINS ); !NULL_P(p); p=pair_cdr(p))
   {
     obj jt = pair_car(p);
     assert( EQ( gvec_ref( jt, THREAD_BLOCKED_ON ), t ));
     UNBLOCK_THREAD( jt );

     store_resume_value( jt, REG0 );
     mark_thread_ready( jt );
   }
  gvec_write_non_ptr( t, THREAD_JOINS, NIL_OBJ );
}
