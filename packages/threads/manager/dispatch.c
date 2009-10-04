#line 2 "dispatch.c"
#include <sys/time.h>
#include "rs_sys_threads_manager_p.h"

struct sys_time thread_start;

obj thread_sys_root[NUM_THREAD_ROOTS];

obj make_dequeue_sub( obj sub_class )
{
  return make3( sub_class,
		make_gvec( vector_class, SLOT(5), FALSE_OBJ ),
		ZERO,
		ZERO );
}

void init_threads( obj classes, obj kproc, obj thnk_template )
{
  int i;

  if (DEBUG_THREADS)
    printf( " initializing system...\n" );

  current_thread = ZERO;
  for (i=0; i<10; i++)
    thread_queue[i] = make_dequeue_sub( gvec_ref( classes, SLOT(0) ) );

  kernel_proc = kproc;
  thunkifier_template = thnk_template;

  assert( SIZEOF_PTR( classes ) == SLOT(9) );

  thunkifier_class = gvec_ref( classes, SLOT(1) );
  thread_class = gvec_ref( classes, SLOT(2) );
  semaphore_class = gvec_ref( classes, SLOT(3) );
  qout_class = gvec_ref( classes, SLOT(4) );
  mailbox_class = gvec_ref( classes, SLOT(5) );
  initiator_socket_class = gvec_ref( classes, SLOT(6) );
  process_class = gvec_ref( classes, SLOT(7) );
  future_class = gvec_ref( classes, SLOT(8) );

  target_vec = make_empty_vector( 0 );

  init_events();
}

static void load_stack( obj cc )
{
  continuation_reg = cc;
}

static obj flush_stack( void )
{
  flush_stack_cache();
  return continuation_reg;
}

static void save_thread_vars( void )
{
  gvec_set( current_thread, THREAD_VARS, thread_state_reg );
  gvec_set( current_thread, THREAD_DYNAMIC_STATE, dynamic_state_reg );
}

static void load_thread_vars( void )
{
  thread_state_reg = gvec_ref( current_thread, THREAD_VARS );
  dynamic_state_reg = gvec_ref( current_thread, THREAD_DYNAMIC_STATE );
}

static void bill_time_to_thread( obj t )
{
  struct sys_time now;
  get_sys_time( &now );

  accum_time( PTR_TO_SYS_TIME( gvec_ref( t, THREAD_TIME ) ),
	      thread_start,
	      now );
  if (rsprof_active) {
    rsprof_prof_mark( 8,        /* 8 = switch off thread: billing amount */
                      fx2int( gvec_ref( current_thread, THREAD_NUMBER ) ), 
                      diff_time_us( thread_start, now ) );
  }
  thread_start = now;
}


static jump_addr dispatch_to_thread( obj t )
{
  int ms;

  current_thread = t;
  load_thread_vars();
  load_stack( gvec_ref( t, THREAD_STACK ) );

  /* clobber the slots in the thread object, so we can GC it if possible */
  gvec_write_non_ptr( t, THREAD_STACK, UNINITIALIZED_OBJ );
  gvec_write_non_ptr( t, THREAD_VARS, UNINITIALIZED_OBJ );
  gvec_write_non_ptr( t, THREAD_DYNAMIC_STATE, UNINITIALIZED_OBJ);

  /* update the thread state */
  gvec_write_non_ptr( t, THREAD_STATE, int2fx(TSTATE_RUNNING) );

  os_set_sigenable( YES );
  get_sys_time( &thread_start );

  ms = compute_time_slice(t);
  if (ms <= 0)
    ms = 1;

  rsprof_prof_mark( 5, fx2int( gvec_ref( t, THREAD_NUMBER ) ), ms );

  if (DEBUG_THREAD_SWITCH)
    printf( " [%s] running thread for %d ms...\n", thread_name(t), ms );
  os_set_timer( ms );
  RETURN0();
}

/*  Called when a thread is removed from a queue.
 *  Updates THREAD_SUSPEND_COUNT and returns 1 if thread
 *  is ready to go now.  Returns 0 if the thread
 *  is currently suspended and therefore cannot
 *  accept it's dequeued responsibility (for example, 
 *  it can't receive a message from a mailbox even
 *  when it's finally popped off the front of the 
 *  thread list if it is suspended.
 */

int did_remove_from_queue( obj th )
{
  obj suspend_count = gvec_ref( th, THREAD_SUSPEND_COUNT );

  if (EQ( suspend_count, ZERO ))
    {
      return 1;
    }
  else
    {
      if (DEBUG_THREAD_SWITCH)
	printf( " [%s] dequeued thread has been suspended\n", 
		thread_name( th ) );

      assert( FX_GT( suspend_count, ZERO ) );

      /* mark it as "not queued" */
      gvec_write_non_ptr( th, 
			  THREAD_SUSPEND_COUNT, 
			  FX_SUB( ZERO, suspend_count ) );

      /* elect somebody else */
      return 0;
    }
}



jump_addr dispatch_to_next_thread( void )
{
  obj kernel_activities = check_for_events(NO);
  int i;

  os_set_sigenable( YES );

  if (DEBUG_THREAD_SWITCH) {
    if (rssig_ready)
      printf( " invoking kernel to force signal delivery...\n" );
    else
      printf( " dispatching to next thread...\n" );
  }

again:
  if (NULL_P(kernel_activities) && !rssig_ready)
    {
      for (i=0; i<10; i++)
	{
	  obj q = thread_queue[i];
	try_again:
	  if (!dequeue_empty(q))
	    {
	      obj th = dequeue_pop_front( q );
	      if (DEBUG_THREAD_SWITCH)
		printf( " found thread at priority %d\n", i );
	      
	      if (did_remove_from_queue( th ))
		return dispatch_to_thread( th );
	      else
		goto try_again;  /* try this queue again */
	    }
	}
      if (DEBUG_THREAD_SWITCH)
	printf( " no threads to run\n" );
      /* no threads ready to run... */
      kernel_activities = check_for_events(YES);
      goto again;
    }
  else
    {
      REG0 = kernel_activities;
      os_halt_timer();
      current_thread = ZERO;
      rsprof_prof_mark( 5, 0, 0 );
      APPLYF(1, kernel_proc);
    }
}

/* TO DO: generate an interrupt (halt the timer) if the priority
 * is higher than that of the current thread
 */

void mark_thread_ready( obj thr )
{
  obj q = thread_queue[ fx2int( gvec_ref(thr,THREAD_PRIORITY) ) ];
  
  /* must not be blocked on anything */
  assert( EQ( gvec_ref( thr, THREAD_BLOCKED_ON ), ZERO ) );

  /*  and must not be suspended (whoever is calling this should
   *  have noticed that we are suspended and done something else
   *  instead)
   */
  assert( EQ( gvec_ref( thr, THREAD_SUSPEND_COUNT ), ZERO ) );
  
  if (DEBUG_THREAD_SWITCH)
    printf( " [%s] thread is now waiting to run\n", thread_name(thr) );
  gvec_write_non_ptr( thr, THREAD_STATE, int2fx(TSTATE_WAITING) );
  dequeue_push_back( q, thr );
}

/*
 *  this mechanism relies on the caller having created a top-most
 *  continuation which will receive the value in its SAVED REG0
 *  (see CR 677)
 */

void store_resume_value( obj thr, obj item )
{
  obj top_frame = gvec_ref( thr, THREAD_STACK );
  /* put the value in REG0 for the thread to pick it up when it resumes */
  gvec_set( top_frame, SLOT(4), item );
}

void mark_thread_ready_1( obj thr, obj item )
{
  UNBLOCK_THREAD( thr );
  store_resume_value( thr, item );
  if (did_remove_from_queue( thr )) {
    mark_thread_ready( thr );
  }
}

static jump_addr kswitch_thread( obj waiting_on, enum thread_state new_status )
{
  save_thread_vars();
  gvec_write_non_ptr( current_thread, THREAD_STATE, int2fx(new_status) );
  gvec_write( current_thread, THREAD_BLOCKED_ON, waiting_on );

  /* only update the stack if the thread isn't complete
   * (because we store the thread results in the stack slot
   */
  if (new_status != TSTATE_COMPLETE)
    gvec_write_ptr( current_thread, THREAD_STACK, flush_stack() );

  bill_time_to_thread( current_thread );
  return dispatch_to_next_thread();
}

jump_addr did_timeout( void )
{
  mark_thread_ready( current_thread );
  return kswitch_thread( ZERO, TSTATE_WAITING );
}

jump_addr switch_thread( obj waiting_on, enum thread_state new_status )
{
  /* note: the 1000-mailbox-send test takes 109 ms with this halt_timer,
   *       and 73 ms without
   */
  os_halt_timer();
  return kswitch_thread( waiting_on, new_status );
}

#if 0
/* TODO... finish this, and use it */
/*
 *  Validate the state of a thread
 */

void check_thread( obj t )
{
  /* Note that anything (except COMPLETE) can get moved to
   * SUSPEND state, and BLOCKED_ON is not updated
   */

  obj blkd_on = gvec_ref( t, THREAD_BLOCKED_ON );
  int s_count = fx2int( gvec_ref( t, THREAD_SUSPEND_COUNT ) );
  enum thread_state s = fx2int( gvec_ref( t, THREAD_STATE ) );

  switch (s) {
  case TSTATE_SUSPEND:
    /* in suspend state, we could still be blocked */
    assert( EQ( blkd_on, ZERO ) );
    /* also, we should not be marked as being on a queue */
    assert( s_count < 0 );
    break;

  case TSTATE_WAITING:
    /* we should never be suspended in the waiting state */
    assert( s_count == 0);
    /* in waiting state, we should not be blocked */
    assert( EQ( blkd_on, ZERO ) );
    break;

  case TSTATE_RUNNING:
    /* we should never be suspended and also in the run state */
    assert( s_count == 0);
    /* in the run state, we should not be blocked */
    assert( EQ( blkd_on, ZERO ) );
    break;

  case TSTATE_BLOCKED:
    /* now we get complicated... */
    
  case TSTATE_SLEEPING:
    /* in the sleep state, we could be suspended, */
    

  case TSTATE_COMPLETE:
    /* we should never be suspended in the complete state */
    assert( s_count == 0);
    /* we should not be blocked on anything, either */
    assert( EQ( blkd_on, ZERO ) );
    break;
  }
}
#endif

