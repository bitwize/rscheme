#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include "rs_sys_threads_manager_p.h"

#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#if HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#define EVENT_LOOP_HOOK_SUPPORT (0)

extern int sigq_notify_fd;   /* from osglue.c */
extern unsigned num_sig;     /* from osglue.c */

static int max_num_fds = 0;
int time_slice = 15;
struct sys_time next_select_time = { 0, 0 };
static struct sys_event *event_block_free_list = NULL;
static int next_event_id;

static int notify_pipe[2];

#define notify_read_side notify_pipe[0]
#define notify_write_side notify_pipe[1]


static obj next_event_num = ZERO;

#define EVENT_NUMBER(e) ((int)(e->event_vec_ix/SLOT(1)))

int get_current_thread_id( void )
{
  obj t = current_thread;
  if (EQ( t, ZERO )) {
    return -1;
  } else {
    return fx2int( gvec_ref( t, THREAD_NUMBER ) );
  }
}

static void free_event( struct sys_event *e )
{
  if (e)
    {
      e->next = event_block_free_list;
      event_block_free_list = e;
      if (DEBUG_THREAD_EVENTS)
	printf( "freeing event #%d {%p}\n", EVENT_NUMBER(e), e );
    }
}

static struct sys_event *alloc_event( enum sys_event_type type )
{
  struct sys_event *e = event_block_free_list;

  if (e)
    {
      event_block_free_list = e->next;
      e->type = type;
      if (DEBUG_THREAD_EVENTS)
	printf( "allocating event #%d {%p} (pop'ing from free list)\n",
		EVENT_NUMBER(e), e );
      return e;
    }

  /*
   *  grow the event ID space, which includes the corresponding
   *  target vector in the scheme heap
   */

  e = (struct sys_event *)malloc( sizeof(struct sys_event) );
  e->event_vec_ix = SLOT(next_event_id);

  if (SLOT(next_event_id) >= SIZEOF_PTR(target_vec))
    {
      obj d, s = target_vec;
      UINT_32 i, n = SIZEOF_PTR(target_vec) * 2;
      if (n < SLOT(3))
	n = SLOT(3);

      if (DEBUG_THREAD_EVENTS)
	printf( "allocating event #%d {%p} (growing vec from %lu => %lu)\n",
		next_event_id, 
		e,
		SIZEOF_PTR(target_vec)/SLOT(1),
		n/SLOT(1));

      d = target_vec = alloc( n, vector_class );
      for (i=0; i<SIZEOF_PTR(s); i+=SLOT(1))
	gvec_write_init( d, i, gvec_ref(s,i) );
      for (; i<n; i+=SLOT(1))
	{
	  gvec_write_init_non_ptr( d, i, ZERO );
	}
    }
  else
    {
      if (DEBUG_THREAD_EVENTS)
	printf( "allocating event #%d {%p} (no need to grow vec)\n",
		next_event_id, e );
    }
  next_event_id++;
  e->type = type;
  return e;
}

static void assert_class_nm( struct sys_event *evt, 
			     obj target, 
			     char *class_nm )
{
  const char *is = symbol_text( class_name( object_class( target ) ) );

  if (strcmp( class_nm, is ) != 0)
    {
      fprintf( stderr,
	       "event #%d {%p} -- class mismatch in target_backptr_index\n",
	       EVENT_NUMBER(evt), evt );
      fprintf( stderr, "got a %s (", is );
      fprinto( stderr, target );
      fprintf( stderr, ") expected a %s\n", class_nm );
      abort();
    }
}

/* returns the index to the target object's `event' slot */

static UINT_32 target_backptr_index( struct sys_event *evt, obj target )
{
#define assert_class_name(str) assert_class_nm( evt, target, str )

  switch (evt->type)
    {
      /* data is an fd, target is an <mbox-input-port> */
    case SYS_EVENT_FILE_READ:
      assert_class_name( "<mbox-input-port>" );
      return MBOX_INPUT_PORT_EVENT;

    case SYS_EVENT_FILE_RECV:
      assert_class_name( "<packet-input-port>" );
      return PACKET_PORT_EVENT;

    case SYS_EVENT_FILE_ACCEPT:
      assert_class_name( "<service>" );
      return SERVICE_EVENT;

    case SYS_EVENT_FILE_CONNECT:
      assert_class_name( "<initiator-socket>" );
      return INITIATOR_SOCKET_EVENT;

    case SYS_EVENT_FILE_WRITE:
      assert_class_name( "<queued-output-port>" );
      return QOUT_EVENT;

    case SYS_EVENT_FILE_X_READ:
      assert_class_name( "<x-event-queue>" );
      return XQUEUE_EVENT;

    case SYS_EVENT_FILE_EXCEPTION:
      break;

    /* target is a <thread> */

    case SYS_EVENT_FILE_SELECT:
      assert_class_name( "<select-event>" );
      return SELECT_EVENT_EVENT;

    case SYS_EVENT_REALTIME:
      assert_class_name( "<thread>" );
      return THREAD_BLOCKED_ON;
      
    case SYS_EVENT_REALTIME_INTERVAL:
    case SYS_EVENT_THREADTIME:
    case SYS_EVENT_THREADTIME_INTERVAL:
      /* do not return; break out and abort  */
      break;
    }
  abort();
  return 0;
}

static void setup_target_biptr( struct sys_event *evt, obj target )
{
  UINT_32 ix = target_backptr_index(evt, target);

  assert( EQ(gvec_ref(target, ix),ZERO) );
  assert( EQ(EVENT_TARGET(evt), ZERO ) );

  gvec_write_non_ptr( target, ix, RAW_PTR_TO_OBJ(evt) );
  gvec_write_ptr( target_vec, evt->event_vec_ix, target );

  if (DEBUG_THREAD_EVENTS)
    printf( "assigning event #%d {%p} for target {%#lx}\n",
	    EVENT_NUMBER(evt), evt, VAL(target) );
}

static void clear_target_biptr( struct sys_event *evt )
{
  UINT_32 ix;
  obj t = EVENT_TARGET(evt);

  ix = target_backptr_index(evt, t);

  assert( !EQ(t, ZERO ) );
  assert( !EQ(gvec_ref(t, ix),ZERO) );
  gvec_write_non_ptr( t, ix, ZERO );
  gvec_write_non_ptr( target_vec, evt->event_vec_ix, ZERO );

  if (DEBUG_THREAD_EVENTS)
    printf( "unassigning event #%d {%p}, from target {%#lx}\n",
	    EVENT_NUMBER(evt), evt, VAL(t) );
}

void init_events( void )
{
  if (pipe( notify_pipe ) < 0)
    os_error( "pipe", 0 );
  if (notify_read_side >= max_num_fds)
    max_num_fds = notify_read_side+1;
}

struct file_event_list {
  fd_set            set;
  struct sys_event *list;
};

static struct sys_event *select_list;
static struct file_event_list read_list, write_list, except_list;
static struct sys_event *next_realtime_event = NULL;

static obj handle_exception_event( struct sys_event *e, obj to_do );
static obj handle_read_event( struct sys_event *e, obj to_do );
static obj handle_realtime_event( struct sys_event *e, obj to_do );
static obj handle_connect_event( struct sys_event *e, obj to_do );
static obj handle_write_event( struct sys_event *e, obj to_do );

#define MALLOC(t) ((t *)malloc(sizeof(t)))

static struct sys_event *event_list_rm( struct sys_event *e,
					struct sys_event *head )
{
  /* pop it out of the list */
  if (e->prev)
    {
      e->prev->next = e->next;
    }
  else
    {
      head = e->next;
    }

  if (e->next)
    {
      e->next->prev = e->prev;
    }
  return head;
}

static struct sys_event *insert_time_event( struct sys_event *e,
					    struct sys_event *head )
{
  if (!head)
    {
      e->prev = e->next = NULL;
      return e;
    }
  if (time_lt( e->data.time.next_time, head->data.time.next_time ))
    {
      head->prev = e;
      e->next = head;
      if (DEBUG_THREAD_EVENTS)
	printf( "inserting event at %ld.%06ld in head of list\n",
		e->data.time.next_time.sec,
		e->data.time.next_time.usec );
      return e;
    }
  else
    {
      struct sys_event *prev, *p;
      prev = head;
      for (p=head->next; p; p=p->next)
	{
	  if (time_lt( e->data.time.next_time, p->data.time.next_time ))
	    {
	      /* insert it before p and after prev */
	      if (DEBUG_THREAD_EVENTS)
		printf( "inserting event at %ld.%06ld just before one at %ld.%06ld\n",
			e->data.time.next_time.sec,
			e->data.time.next_time.usec,
			p->data.time.next_time.sec,
			p->data.time.next_time.usec );
	      prev->next = e;
	      p->prev = e;
	      e->next = p;
	      e->prev = prev;
	      return head;
	    }
	  prev = p;
	}
      /* insert it at the end */
      if (DEBUG_THREAD_EVENTS)
	printf( "inserting event at %ld.%06ld at end of list\n",
		e->data.time.next_time.sec,
		e->data.time.next_time.usec );
      prev->next = e;
      e->next = NULL;
      e->prev = prev;
      return head;
    }
}

struct sys_event *register_time_event( struct sys_time next_time, 
				       obj target,
				       enum sys_event_type t )
{
  struct sys_event *e = alloc_event(t);
  e->next = e->prev = NULL;
  e->data.time.next_time = next_time;
  e->data.time.delta_time.sec = 0;
  e->data.time.delta_time.usec = 0;
  setup_target_biptr( e, target );

  next_realtime_event = insert_time_event( e, next_realtime_event );
  return e;
}

struct sys_event *make_time_event( struct sys_time next_time, obj target )
{
  return register_time_event( next_time, target, SYS_EVENT_REALTIME );
}

void free_select_event( struct sys_event *e )
{
  assert( e->type == SYS_EVENT_FILE_SELECT );

  select_list = event_list_rm( e, select_list );
  clear_target_biptr( e );
  free_event( e );
}

void free_time_event( struct sys_event *e )
{
  assert( (e->type == SYS_EVENT_REALTIME) 
	  || (e->type == SYS_EVENT_REALTIME_INTERVAL) );

  next_realtime_event = event_list_rm( e, next_realtime_event );
  clear_target_biptr(e);
  free_event(e);
}

struct sys_event *make_time_interval_event( struct sys_time next_time, 
					    struct sys_time interval,
					    obj target )
{
  struct sys_event *e;
  e = register_time_event( next_time, target, SYS_EVENT_REALTIME_INTERVAL );
  e->data.time.delta_time = interval;
  return e;
}



static void free_file_event( struct sys_event *e, struct file_event_list *fel )
{
  FD_CLR( e->data.fd, &fel->set );
  fel->list = event_list_rm( e, fel->list );
  clear_target_biptr(e);
  free_event(e);
}

static struct sys_event *register_file_event( int fd, 
					      obj target, 
					      struct file_event_list *fel,
					      enum sys_event_type type )
{
  struct sys_event *s = alloc_event(type);

  FD_SET( fd, &fel->set );

  if (fd >= max_num_fds)
    max_num_fds = fd+1;

  s->data.fd = fd;

  s->next = fel->list;
  s->prev = NULL;
  if (fel->list)
    fel->list->prev = s;
  fel->list = s;
  setup_target_biptr( s, target );
  return s;
}

#define SELECT_EVENT_HAS_EXPIRES        (0x1)
#define SELECT_EVENT_HAS_READ           (0x2)
#define SELECT_EVENT_HAS_WRITE          (0x4)
#define SELECT_EVENT_HAS_EXCEPTION      (0x8)

static int merge_fdset( fd_set *dest, fd_set *src )
{
  int k, n = 0;

  FD_ZERO( dest );

  for (k=0; k<FD_SETSIZE; k++) {
    if (FD_ISSET( k, src )) {
      if (k+1 > n) {
        n = k+1;
      }
      FD_SET( k, dest );
    }
  }
  return n;
}


struct sys_event *make_select_event( fd_set *sets,
                                     struct sys_time *expire,
                                     obj mboxes,        /* not implemented */
                                     obj target )
{
  struct sys_event *s = alloc_event( SYS_EVENT_FILE_SELECT );
  fd_set r, w, x;
  int mr, mw, mx;

  mr = merge_fdset( &s->data.select.r, &sets[0] );
  mw = merge_fdset( &s->data.select.w, &sets[1] );
  mx = merge_fdset( &s->data.select.x, &sets[2] );

  if (mr >= max_num_fds) max_num_fds = mr;
  if (mw >= max_num_fds) max_num_fds = mw;
  if (mx >= max_num_fds) max_num_fds = mx;

  s->data.select.flags = 0;

  if (mr) {
    s->data.select.flags |= SELECT_EVENT_HAS_READ;
  }

  if (mw) {
    s->data.select.flags |= SELECT_EVENT_HAS_WRITE;
  }

  if (mx) {
    s->data.select.flags |= SELECT_EVENT_HAS_EXCEPTION;
  }
  
  if (expire) {
    s->data.select.flags |= SELECT_EVENT_HAS_EXPIRES;
    s->data.select.expire_time = *expire;
  }

  s->next = select_list;
  s->prev = NULL;
  if (select_list) {
    select_list->prev = s;
  }
  select_list = s;
  setup_target_biptr( s, target );
  return s;
}

struct sys_event *make_read_event( int fd, obj target )
{
  return register_file_event( fd, target, &read_list, SYS_EVENT_FILE_READ );
}

struct sys_event *make_recv_event( int fd, obj target )
{
  return register_file_event( fd, target, &read_list, SYS_EVENT_FILE_RECV );
}

struct sys_event *make_read_x_event( int fd, obj target )
{
  return register_file_event( fd, target, &read_list, SYS_EVENT_FILE_X_READ );
}

struct sys_event *make_accept_event( int fd, obj target )
{
  return register_file_event( fd, target, &read_list, SYS_EVENT_FILE_ACCEPT );
}

struct sys_event *make_connect_event( int fd, obj target )
{
  /* when a socket finishes connection processing,
   * it becomes writable..
   */
  return register_file_event( fd, target, &write_list, 
                              SYS_EVENT_FILE_CONNECT );
}

struct sys_event *make_write_event( int fd, obj target )
{
  return register_file_event( fd, target, &write_list, SYS_EVENT_FILE_WRITE );
}

struct sys_event *make_exception_event( int fd, obj target )
{
  return register_file_event( fd, target, &except_list, 
			      SYS_EVENT_FILE_EXCEPTION );
}


void free_read_event( struct sys_event *e )
{
  free_file_event( e, &read_list );
}

void free_accept_event( struct sys_event *e )
{
  free_file_event( e, &read_list );
}

void free_write_event( struct sys_event *e )
{
  free_file_event( e, &write_list );
}

void free_connect_event( struct sys_event *e )
{
  free_file_event( e, &write_list );
}

void free_exception_event( struct sys_event *e )
{
  free_file_event( e, &except_list );
}


/* how long until next event?
 * it's the smaller of the current time slice,
 * this thread's next CPU timer,
 * and the next real-time timer
 */
  
int compute_time_slice( obj t )
{
  int ms = time_slice;
  struct sys_event *e;

  e = OBJ_TO_SYS_EVENT(gvec_ref( t, THREAD_NEXT_THREADTIME_EVENT ));

  if (next_realtime_event)
    {
      int c = diff_time_ms( thread_start, 
			    next_realtime_event->data.time.next_time );
      if (DEBUG_THREAD_EVENTS)
	printf( " compute-time-slice: (real) time event is %d ms away\n", c );
      if (c < ms)
	ms = c;
    }
  if (e)
    {
      int c = diff_time_ms( thread_start, e->data.time.next_time );
      if (DEBUG_THREAD_EVENTS)
	printf( " compute-time-slice: (thread) time event is %d ms away\n",c );
      if (c < ms)
	ms = c;
    }
  if (ms < 0)
    ms = 0;
  return ms;
}


int bad_fd_q( struct sys_event *evt )
{
  int rc;

  rc = fcntl(evt->data.fd, F_GETFL);
  if (rc < 0)
    {
      if (DEBUG_UNCOMMON_CASES)
	{
	  printf( "fd %d is bad (%s)\n", evt->data.fd, strerror(errno) );
	}
      return 1;
    }
  return 0;
}

static void find_and_notify_bad_fd( void )
{
  struct sys_event *p;

  /* a crude approach -- linear search through all the fd's
     (however, go ahead and assume that usually only one fd will
     become bad at a time, so stop when we find the first one)
     */
  for (p=read_list.list; p; p=p->next)
    {
      if (bad_fd_q(p))
	{
	  obj mbox = gvec_ref( EVENT_TARGET(p), MBOX_INPUT_PORT_MBOX );
	  ksend_mailbox( mbox, 
			 make_os_error( "fcntl", 1, int2fx(p->data.fd) ));
	  free_read_event( p );
	  return;
	}
    }
  if (DEBUG_UNCOMMON_CASES)
    {
      printf( "didn't find bad fd by searching read list!\n" );
    }
}

obj handle_select_event( struct sys_event *e, 
                         fd_set *r, 
                         fd_set *w, 
                         fd_set *x,
                         obj to_do, 
                         int *num_fd )
{
  int i, match = 0;
  obj rlist = NIL_OBJ, wlist = NIL_OBJ, xlist = NIL_OBJ;
  obj se = EVENT_TARGET(e);
  obj thr = gvec_ref( se, SELECT_EVENT_WAITER );

  if (!num_fd) {
    /* it's a timeout... */
    match = 1;
    goto short_circuit;
  }

  /* OUCH! Expensive... */

  if (e->data.select.flags & SELECT_EVENT_HAS_READ) {
    for (i=0; i<FD_SETSIZE; i++) {
      if (FD_ISSET( i, &e->data.select.r ) && FD_ISSET( i, r )) {
        rlist = cons( int2fx( i ), rlist );
        match = 1;
        (*num_fd)--;
        if ((*num_fd) == 0) goto short_circuit;
      }
    }
  }

  if (e->data.select.flags & SELECT_EVENT_HAS_WRITE) {
    for (i=0; i<FD_SETSIZE; i++) {
      if (FD_ISSET( i, &e->data.select.w ) && FD_ISSET( i, w )) {
        wlist = cons( int2fx( i ), wlist );
        match = 1;
        (*num_fd)--;
        if ((*num_fd) == 0) goto short_circuit;
      }
    }
  }
  if (e->data.select.flags & SELECT_EVENT_HAS_EXCEPTION) {
    for (i=0; i<FD_SETSIZE; i++) {
      if (FD_ISSET( i, &e->data.select.x ) && FD_ISSET( i, x )) {
        xlist = cons( int2fx( i ), xlist );
        match = 1;
        (*num_fd)--;
        if ((*num_fd) == 0) goto short_circuit;
      }
    }
  }
  
 short_circuit:;
  if (match) {
    obj suspend_count = gvec_ref( thr, THREAD_SUSPEND_COUNT );
    
    free_select_event( e );

    mark_thread_ready_1( thr,
                         make4( vector_class, 
                                rlist, wlist, xlist, 
                                num_fd ? FALSE_OBJ : TRUE_OBJ ) );
  }
  return to_do;
}


#if EVENT_LOOP_HOOK_SUPPORT
void *(*rs_event_loop_pre_hook)( struct timeval **timeout ) = NULL;
void (*rs_event_loop_post_hook)( void *carry, int n ) = NULL;
#endif

static void FD_UNION( fd_set *lvalue, fd_set *rvalue )
{
  unsigned i,
    *l = (unsigned *)lvalue, 
    *r = (unsigned *)rvalue;

  for (i=0; i<(sizeof(fd_set)/sizeof(unsigned)); i++) {
    *l++ |= *r++;
  }
}

obj check_for_events( rs_bool block_q )
{
  struct timeval *tp, t;
  fd_set r, w, x;
  int tsel = -1, n;
  obj to_do = NIL_OBJ;
  void *hook_carry = NULL;

  get_sys_time( &thread_start );

  if (!block_q && time_le( thread_start, next_select_time ))
    {
      /* don't bother to check but every 1.5 ms or so,
       * (except always check when blocking)
       */
      if (DEBUG_THREAD_EVENTS)
	printf( "check for events (%s)... next select time is %dus away\n", 
		block_q ? "block" : "don't block",
		diff_time_us( thread_start, next_select_time ) );
      return to_do;
    }

  tp = NULL;

  r = read_list.set;
  w = write_list.set;
  x = except_list.set;

  if (select_list) {
    struct sys_event *sp;
    for (sp=select_list; sp; sp=sp->next) {
      if (sp->data.select.flags & SELECT_EVENT_HAS_READ) {
        FD_UNION( &r, &sp->data.select.r );
      }
      if (sp->data.select.flags & SELECT_EVENT_HAS_WRITE) {
        FD_UNION( &w, &sp->data.select.w );
      }
      if (sp->data.select.flags & SELECT_EVENT_HAS_EXCEPTION) {
        FD_UNION( &x, &sp->data.select.x );
      }
      if (block_q && (sp->data.select.flags & SELECT_EVENT_HAS_EXPIRES)) {
        tsel = diff_time_ms( thread_start, sp->data.select.expire_time );
        if (tsel < 0) {
          tsel = 0;
        }
      }
    }
  }

  if (!block_q)
    {
      tsel = 0;
    }
  else if ((tsel != 0) && next_realtime_event)
    {
      int ms;

      ms = diff_time_ms( thread_start,
			 next_realtime_event->data.time.next_time );
      if ((tsel < 0) || (ms < tsel)) {
        tsel = (ms < 0) ? 0 : ms;
      }
    }

  if (tsel == 0) {
    t.tv_sec = t.tv_usec = 0;
    tp = &t;
  } else if (tsel > 0) {
    t.tv_sec = tsel / 1000;
    t.tv_usec = (tsel % 1000) * 1000;
    tp = &t;
  }

  if (DEBUG_THREAD_EVENTS)
    {
      printf( "select %d fds, block = %s, delay = ", 
	      max_num_fds, 
	      block_q ? "yes" : "no" );
      if (tp)
	printf( "%g s\n", tp->tv_usec / 1000000.0 + tp->tv_sec );
      else
	printf( "INFINITE\n" );
    }

  sigq_notify_fd = notify_write_side;
  FD_SET( notify_read_side, &r );

  if (rssig_ready) {
    t.tv_sec = t.tv_usec = 0;
    tp = &t;
  } else {
    rsprof_timepoint( 3001 );
  }

  if (tp) {
    if ((tp->tv_sec == 0) && (tp->tv_usec == 0)) {
      rsprof_timepoint( 3001 ); /* 0-wait select() */
    } else {
      rsprof_timepoint( 3002 ); /* non-zero wait select() */
    }
  } else {
    rsprof_timepoint( 3003 );   /* blocking wait */
  }

#if EVENT_LOOP_HOOK_SUPPORT
  if (rs_event_loop_pre_hook) {
    hook_carry = rs_event_loop_pre_hook( &tp );
  }
#endif

  n = select( max_num_fds, &r, &w, &x, tp );

#if EVENT_LOOP_HOOK_SUPPORT
  if (rs_event_loop_post_hook) {
    rs_event_loop_post_hook( hook_carry, n );
  }
#endif

  sigq_notify_fd = -1;

  if (n < 0)
    {
      rsprof_timepoint( 3009 );
      if (DEBUG_UNCOMMON_CASES)
	{
	  printf( "select returns error (%s)\n", strerror(errno) );
	}
      /* errors... */
      if (errno == EBADF)
	{
	  /* seek out the bad FDs and send it an error signal */
	  find_and_notify_bad_fd();
	}
    }
  else
    {
      if (DEBUG_THREAD_EVENTS)
	printf( "select returns %d\n", n );
      rsprof_timepoint( 3100+n );
      if (n)
	{
	  struct sys_event *p, *nxt = NULL;

          for (p=select_list; p && n; p=nxt) {
            to_do = handle_select_event( p, &r, &w, &x, to_do, &n );
          }

	  for (p=read_list.list; p && n; p=nxt)
	    {
	      nxt = p->next;
	      if (FD_ISSET(p->data.fd, &r))
		{
		  n--;
		  if (DEBUG_THREAD_EVENTS)
		    printf( "  fd %d readable\n", p->data.fd );
		  to_do = handle_read_event( p, to_do );
		}
	    }
	  if (FD_ISSET(notify_read_side, &r))
	    {
	      int nb;
	      char temp[11];
	      n--;

	      nb = read( notify_read_side, temp, 10 );
              rsprof_prof_mark( 4, 0, nb );

	      if ((nb > 0) && DEBUG_UNCOMMON_CASES)
		{
		  int i;

		  temp[nb] = 0;
		  printf( "  notification pipe: [" );

		  for (i=0; i<nb; i++)
		    {
		      char *s = NULL;

		      switch ((enum RSSIGNUM)(temp[i]-'A'))
			{
			case RSSIG_USER_INTR:   s = "user-intr"; break;
			case RSSIG_TIMEOUT:     s = "timer"; break;
			case RSSIG_CHILD_EXITED:s = "child-exited"; break;
			case RSSIG_FINALIZE:    s = "finalize"; break;
			case RSSIG_GC_FLIP:     s = "gc-flip"; break;
			case RSSIG_C_SIGNAL:    s = "c-signal"; break;
			case RSSIG_CALL0:       s = "call0"; break;
			case RSSIG_CALL1:       s = "call1"; break;
			case RSSIG_CALL2:       s = "call2"; break;
			}
		      if (i > 0)
			printf( " " );
		      printf( "%c:%s", temp[i], s );
		    }
		  printf( "]  %d signals queued, %s\n",
			  num_sig, 
			  rssig_ready ? "RSSIG READY" : "RSSIG NOT ready" );
		}
	    }
	  for (p=write_list.list; p && n; p=nxt)
	    {
	      nxt = p->next;
	      if (FD_ISSET(p->data.fd, &w))
		{
		  n--;
		  if (DEBUG_THREAD_EVENTS)
		    printf( "  fd %d writable\n", p->data.fd );
		  to_do = handle_write_event( p, to_do );
		}
	    }
	  for (p=except_list.list; p && n; p=nxt)
	    {
	      nxt = p->next;
	      if (FD_ISSET(p->data.fd, &x))
		{
		  n--;
		  if (DEBUG_THREAD_EVENTS)
		    printf( "  fd %d exceptional\n", p->data.fd );
		  to_do = handle_exception_event( p, to_do );
		}
	    }
	  if ((n > 0) && DEBUG_UNCOMMON_CASES)
	    printf( "  select returned %d more things than we could handle\n",
		    n );
	}
    }
  
  get_sys_time( &thread_start );

  next_select_time.usec = thread_start.usec + ALLOW_SELECT_LATENCY;
  next_select_time.sec = thread_start.sec;
  if (next_select_time.usec > 1000000)
    {
      next_select_time.usec -= 1000000;
      next_select_time.sec++;
    }
  
  while (next_realtime_event 
	 && time_le( next_realtime_event->data.time.next_time,
		     thread_start ))
    {
      if (DEBUG_THREAD_EVENTS)
	printf( "  timer\n" );
      to_do = handle_realtime_event( next_realtime_event, to_do );
    }

  if (select_list) {
    struct sys_event *p = select_list, *n;
    while (p) {
      n = p->next;
      if ((p->data.select.flags & SELECT_EVENT_HAS_EXPIRES) 
          && time_le( p->data.select.expire_time, thread_start )) {
        to_do = handle_select_event( p, NULL, NULL, NULL, to_do, NULL );
      }
      p = n;
    }
  }

  return to_do;
}

static obj handle_exception_event( struct sys_event *e, obj to_do )
{
  return to_do;
}

static obj handle_write_event( struct sys_event *e, obj to_do )
{
  switch (e->type) {

  case SYS_EVENT_FILE_WRITE:
    return handle_std_write_event( e, to_do );

  case SYS_EVENT_FILE_CONNECT:
    return handle_connect_event( e, to_do );

  default:
    return to_do;       /* ignore it; shouldn't get here, though */
  }
}


static obj handle_realtime_event( struct sys_event *e, obj to_do )
{
  if (e->type == SYS_EVENT_REALTIME)
    {
      obj thr = EVENT_TARGET(e);
      obj suspend_count = gvec_ref( thr, THREAD_SUSPEND_COUNT );

      if (DEBUG_THREAD_EVENTS)
	printf( " realtime event triggered (after %d us delay)\n",
		diff_time_us( e->data.time.next_time, thread_start ) );
      free_time_event( e );
      if (did_remove_from_queue( thr ))
	{
	  mark_thread_ready( thr );
	}
      else
	{
	  /*  Negate the suspend count, and clear `blocked_on'.
	   *
	   *  When (and if) the thread is resumed, it will
	   *  look like it was waiting to run but had been
	   *  removed from the run queue, so it'll be immediately
	   *  requeued.
	   */
          UNBLOCK_THREAD( thr );
	}
    }
  else
    {
      /* ignore it */
    }
  return to_do;
}

#if !HAVE_SOCKLEN_T
typedef int socklen_t;
#endif

static obj handle_connect_event( struct sys_event *e, obj to_do )
{
  obj waiter;
  int rc, err, x;
  socklen_t errlen = sizeof(int);

  /* get the error code */
  rc = getsockopt( e->data.fd, SOL_SOCKET, SO_ERROR, &err, &errlen );
  x = errno;

  waiter = gvec_read( EVENT_TARGET(e), INITIATOR_SOCKET_WAITER );
  gvec_write_non_ptr( EVENT_TARGET(e), INITIATOR_SOCKET_WAITER, FALSE_OBJ );

  free_connect_event( e );

  mark_thread_ready_1( waiter,
                       (rc == 0) 
                       ? int2fx( err ) 
                       : cons( int2fx( 1 ), int2fx( x ) ) );
  return to_do;
}


static obj handle_accept_event( struct sys_event *e, obj to_do )
{
  int fd;
  socklen_t n;
  char temp[64];
  obj item, mbox = gvec_read( EVENT_TARGET(e), SERVICE_MBOX );

  n = 64;

  fd = accept( e->data.fd, (struct sockaddr *)temp, &n );
  if (fd >= 0)
    {
      obj addr;

      addr = bvec_alloc( n, byte_vector_class );
      memcpy( PTR_TO_DATAPTR(addr), temp, n );
      item = cons( int2fx(fd), addr );
    }
  else
    {
      if (DEBUG_UNCOMMON_CASES)
        printf( "accept: %s\n", strerror(errno) );
      item = make_os_error("accept", 1, int2fx(e->data.fd));
      free_accept_event(e);
    }
  ksend_mailbox( mbox, item );
  return to_do;
}

static char recv_buffer[65536];

static obj handle_recv_event( struct sys_event *e, obj to_do )
{
  int n;
  socklen_t fromlen;
  obj mbox = gvec_ref( EVENT_TARGET(e), PACKET_PORT_MBOX );
  obj sockaddr_class = gvec_ref( EVENT_TARGET(e), PACKET_PORT_SOCKADDR_CLASS );
  unsigned char fromaddr[1024];

  fromlen = sizeof( fromaddr );
  n = recvfrom( e->data.fd, 
                recv_buffer, sizeof( recv_buffer ),
                0,
                (struct sockaddr *)&fromaddr,
                &fromlen );

  if (n < 0)
    {
      if ((errno == EAGAIN) || (errno == EWOULDBLOCK))
        {
          /* do nothing */
        }
      else
        {
          /* error condition... */
          ksend_mailbox( mbox,
                         make_os_error( "recvfrom",
                                        1,
                                        int2fx( e->data.fd )) );
          /* deallocate the event... if the file should still be
           * read from, then it will have to be re-registered
           */
          free_file_event( e, &read_list );
        }
    }
  else if (n == 0)
    {
      /* EOF */
      ksend_mailbox( mbox, FALSE_OBJ );
      free_file_event( e, &read_list );
    }
  else
    {
      obj str = bvec_alloc( n+1, string_class );
      obj sender = bvec_alloc( fromlen, sockaddr_class );
      obj item;

      memcpy( string_text(str), recv_buffer, n );
      if (DEBUG_THREAD_EVENTS)
        printf( " event #%d {%p} -- read %d bytes from fd %d\n",
                EVENT_NUMBER(e), e, n, e->data.fd );
         

      memcpy( PTR_TO_DATAPTR(sender), fromaddr, fromlen );
      item = make2( vector_class, str, sender );
      ksend_mailbox( mbox, item );
    }
  return to_do;
}

static obj handle_std_read_event( struct sys_event *e, obj to_do )
{
  char temp[8192];
  int n;
  obj mbox = gvec_ref( EVENT_TARGET(e), MBOX_INPUT_PORT_MBOX );

  while (1)
    {
      n = read( e->data.fd, temp, 8192 );
      if (n < 0)
	{
	  if ((errno == EAGAIN) || (errno == EWOULDBLOCK))
	    {
	      return to_do;
	    }
	  else
	    {
	      /* error condition... */
	      ksend_mailbox( mbox,
			     make_os_error("read",1,int2fx(e->data.fd)) );
	      /* deallocate the event... if the file should still be
	       * read from, then it will have to be re-registered
	       */
	      free_file_event( e, &read_list );
	      return to_do;
	    }
	}
      else if (n == 0)
	{
	  /* EOF */
	  ksend_mailbox( mbox, FALSE_OBJ );
	  free_file_event( e, &read_list );
	  return to_do;
	}
      else
	{
	  obj str = bvec_alloc( n+1, string_class );
	  memcpy( string_text(str), temp, n );
	  if (DEBUG_THREAD_EVENTS)
	    printf( " event #%d {%p} -- read %d bytes from fd %d\n",
		    EVENT_NUMBER(e), e, n, e->data.fd );
	  ksend_mailbox( mbox, str );
	  /* loop to try to eat some more */
#if PERMIT_BLOCKING_READS
          /* if we are using/permitting blocking reads, don't loop -- wait for
	   * select() to tell use we have some more data ready
	   */
          return to_do;
#endif	  
	}
    }
}

static obj handle_read_event( struct sys_event *e, obj to_do )
{
  switch (e->type)
    {
    case SYS_EVENT_FILE_X_READ:
      return handle_read_x_event( e, to_do );
    case SYS_EVENT_FILE_ACCEPT:
      return handle_accept_event( e, to_do );
    case SYS_EVENT_FILE_RECV:
      return handle_recv_event( e, to_do );
    default:
      return handle_std_read_event( e, to_do );
    }
}
