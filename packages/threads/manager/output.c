#include <sys/time.h>
#include <unistd.h>
#include <string.h>
#include "rs_sys_threads_manager_p.h"

#define GET_FD(port)  ((int)fx2int( gvec_ref( port, QOUT_FD ) ))
#define HAS_EVENT_Q(port) (!EQ( gvec_ref( port, QOUT_EVENT ), ZERO ))
#define GET_BLOCK_SIZE(port) fx2int( gvec_ref( port, QOUT_BLOCK_SIZE ) )
#define GET_BUFFER(port) gvec_ref( port, QOUT_BUFFER )
#define GET_BUFFER_IX(port) fx2int( gvec_ref( port, QOUT_BUFFER_INDEX ) )
#define SET_BUFFER(port,b) gvec_write_ptr( port, QOUT_BUFFER, b )
#define SET_BUFFER_IX(port,ix) gvec_write_non_ptr( port, QOUT_BUFFER_INDEX, \
                                                   int2fx(ix) )
#define GET_PENDING_WRITES(port) gvec_ref( port, QOUT_PENDING_WRITES )
#define CREATE_EVENT_THRESHOLD(port) (GET_BLOCK_SIZE(port)/2)
#define GET_FLUSH_ON_NL(port)  (gvec_ref( port, QOUT_FLUSH_ON_NL ))

/*
 *  Returns the number of bytes successfully written,
 *  or:
 *
 *     0 => could not write because it would block
 *
 *    -1 => the write failed
 *
 *  If 0 is returned, the caller should arrange to wait until 
 *  select() indicates that the fd is writable again before
 *  trying to write again.
 */

static int do_write( obj port, UINT_8 *bytes, int len )
{
  int n;

  if (len == 0) {
    /* special-case empty writes */
    return 0;
  }

  if (DEBUG_QUEUED_WRITES)
    printf( " fd %d: writing %d bytes...", GET_FD(port), len );

  /*
   *  Note:  It may so happen that the data we're trying to
   *         write is coming out of a large string object
   *         in the persistent store.  Hence, we need to make sure
   *         to touch at least every 4K-th byte or so...
   */
  ensure_memory_mapped( bytes, len );

  n = write( GET_FD(port), bytes, len );

  if ((n < 0) && ((errno == EAGAIN) || (errno == EWOULDBLOCK)))
    {
      if (DEBUG_QUEUED_WRITES)
	printf( " WOULD BLOCK\n" );
      return 0;
    }

  if (n < 0)
    {
      if (DEBUG_QUEUED_WRITES)
	printf( " error\n" );
      if (DEBUG_UNCOMMON_CASES)
	printf( " fd %d: error writing %d bytes: %d\n", 
		GET_FD(port), len, errno );
      if (!HAS_EVENT_Q(port))
	os_error( "write", 1, int2fx(GET_FD(port)) );
      return -1;
    }
  if (DEBUG_QUEUED_WRITES)
    printf( " wrote %d\n", n );

  return n;
}

enum flush_state {
  DONT_CARE,
  LOOK_FOR_NL,
  SAW_NL
};

struct OutputCursor {
  UINT_8 *ptr;
  UINT_8 *lim;
  obj     port;
  enum flush_state flush;
};

static rs_bool curbuf_empty( struct OutputCursor *c )
{
  return c->ptr == byte_string_text( GET_BUFFER(c->port) );
}

static void open_cursor( struct OutputCursor *c, obj port )
{
  UINT_8 *p = byte_string_text( GET_BUFFER(port) );
  c->port = port;
  c->ptr = p + GET_BUFFER_IX(port);
  c->lim = p + SIZEOF_PTR( GET_BUFFER(port) );
  if (truish( GET_FLUSH_ON_NL(port) ))
    {
      c->flush = LOOK_FOR_NL;
    }
  else
    {
      c->flush = DONT_CARE;
    }
}

static void close_cursor( struct OutputCursor *c )
{
  int ix = c->ptr - byte_string_text( GET_BUFFER(c->port) );
  SET_BUFFER_IX( c->port, ix );
}

static inline obj alloc_buf( obj port )
{
  int blksiz = GET_BLOCK_SIZE( port );
  return bvec_alloc( blksiz, string_class );
}

static void new_buffer( struct OutputCursor *c )
{
  obj buf = alloc_buf( c->port );

  SET_BUFFER( c->port, buf );
  c->ptr = byte_string_text(buf);
  c->lim = c->ptr + SIZEOF_PTR(buf) - 1;
}

static void enq_wr( obj port, obj buf, obj from, obj to )
{
  dequeue_push_back( GET_PENDING_WRITES(port),
		     make3( vector_class, buf, from, to ) );
}

static void mk_event( obj port )
{
  make_write_event( GET_FD(port), port );
}

/*
 *  do a write in order to flush the current buffer
 *
 *  If `flush_q' is true, then this function ensures that the output
 *  cursor's buffer is empty, even if it has to create an event to do
 *  so.
 *
 *  Otherwise, it ensures that at least 
 *     (BLOCK_SIZE() - CREATE_EVENT_THRESHOLD())
 *  bytes are available in the buffer.
 */

static rs_bool write_to_flush( struct OutputCursor *c, rs_bool flush_q )
{
  int n;
  obj buf = GET_BUFFER(c->port);
  int len;

  len = c->ptr - byte_string_text(buf);

  n = do_write( c->port, byte_string_text(buf), len );
  if (n < len)
    {
      int left = len - n;
      
      /* didn't write it all  -- create an event if the amount
       * left is above the threshold *OR* if the caller is a
       * a `flush' command, which is going to block until everything
       * is written (or maybe the caller is someone who is going
       * to enqueue a large write, in which case we need to take
       * what's currently in the cursor and make it pending)
       */
      if ((left >= CREATE_EVENT_THRESHOLD(c->port)) || flush_q)
	{
          if (DEBUG_QUEUED_WRITES) {
            printf( "  creating event: %d bytes at %d\n", len, n );
          }
	  mk_event( c->port );
          /* transfer the current buffer to the PENDING_WRITES queue */
	  enq_wr( c->port, buf, int2fx(n), int2fx(len) );
          /* and create a new current buffer */
	  new_buffer( c );
	}
      else
	{
          if (DEBUG_QUEUED_WRITES) {
            printf( "  shifting output buffer to eat %d bytes\n", n );
          }
	  memmove( byte_string_text(buf),
		   byte_string_text(buf) + n,
		   len - n );
	  c->ptr = byte_string_text(buf) + len - n;
	}
      return NO;
    }
  else
    {
      /* wrote it all -- reuse the buffer */
      c->ptr = byte_string_text(buf);
      return YES;
    }
}

/*
 *  flush the current buffer
 *
 *  If there is no current event, then this involves trying to
 *  write() the filled-in portion of the buffer.  Any unwritten
 *  portion is queued (or left in the buffer, if there isn't
 *  much), and in either case the cursor's buffer has at least
 *  (BLOCK_SIZE() - CREATE_EVENT_THRESHOLD()) bytes available
 *  upon return.
 *
 *  Return
 *    YES if we're sure the cursor (current buffer) is empty
 *
 *    NO if the cursor might not be empty
 *    (e.g., if we call write_to_flush() the cursor may be
 *    empty if there write_to_flush() queued some new stuff)
 */

static rs_bool flush_buffer( struct OutputCursor *c, rs_bool wflush )
{
  obj buf = GET_BUFFER(c->port);

  if (c->ptr == byte_string_text(buf)) {
    /* nothing to do; the current buffer is empty */
    return YES;
  }

  if (HAS_EVENT_Q(c->port))
    {
      int len = c->ptr - byte_string_text(buf);
      if (DEBUG_QUEUED_WRITES) {
        printf( "  flush_buffer: %d bytes at +0\n", len );
      }
      enq_wr( c->port, buf, ZERO, int2fx(len) );
      new_buffer( c );
      return YES;
    }
  else
    {
      /* Note that write_to_flush() only returns YES if the the
       * output is completely empty: nothing in the buffer, and
       * nothing in the pending queue
       */
      return write_to_flush( c, wflush );
    }
}

/* q_big_write() allows writing a substring of a larger string */

static void q_big_write( struct OutputCursor *c, 
			 obj str, UINT_8 *ptr, UINT_32 len )
{
  obj from, to;

  if (EQ(str,FALSE_OBJ))
    {
      str = bvec_alloc( len+1, string_class );
      memcpy( byte_string_text(str), ptr, len );
      ptr = byte_string_text(str);
      from = ZERO;
      to = int2fx(len);
    }
  else
    {
      from = int2fx( ptr - byte_string_text(str) );
      to = FX_ADD( from, int2fx(len) );
    }
  enq_wr( c->port, str, from, to );
}

/*
 *  This function is called when the string to be written
 *  (`len' bytes starting at `ptr') will not fit in what's
 *  left of the output cursor (the output cursor is at
 *  position `c->ptr' and can only go up to `c->lim')
 *
 *  If the string to be written is larger than the block size
 *  for the port, then the buffer is flushed and the data (`str')
 *  is written directly.
 *
 *  Otherwise, whatever fits into the rest of the buffer is copied
 *  into the buffer (thereby filling it), the buffer is flushed, and
 *  the remaining portion of the string is buffered.
 */

static void write_bytes_ov( struct OutputCursor *c, 
			    obj str, UINT_8 *ptr, UINT_32 len )
{
  if (len >= GET_BLOCK_SIZE(c->port))
    {
      /*
       *  Even if the current buffer doesn't have enough stuff to
       *  force an event (due to the CREATE_EVENT_THRESHOLD),
       *  we still need to make queue it.  Hence, we call flush_buffer
       *  with wflush=YES
       */
      flush_buffer( c, YES );
      
      /*
       *  After doing so, there will be no event queue if and only if
       *  the output is completely empty (nothing pending, nothing
       *  buffered)
       */
      if (HAS_EVENT_Q(c->port))
	{
	  q_big_write( c, str, ptr, len );
	}
      else
	{
	  int n;

	  n = do_write( c->port, ptr, len );
	  if (n < len)
	    {
	      int left = len - n;
	      if (left >= CREATE_EVENT_THRESHOLD(c->port))
		{
		  mk_event( c->port );
		  q_big_write( c, str, ptr + n, len - n );
		}
	      else
		{
		  memcpy( c->ptr, ptr + n, len - n );
		  c->ptr += len - n;
		}
	    }
	}
    }
  else
    {
      UINT_32 m = c->lim - c->ptr;

      do {
	/* `m' is the space we have before the end of the buffer;
	 * we are guaranteed that (m < len) because of the condition
	 * for calling write_bytes_ov()
	 */
	
	memcpy( c->ptr, ptr, m );
	c->ptr = c->lim; /* we have now filled the buffer;
			  * `c->ptr = c->lim' is equivalent to `c->ptr += m'
			  */
        
        /* flush_buffer() will ensure that at least BLOCK_SIZE() -
         * CREATE_EVENT_THRESHOLD() bytes are available, so we're
         * guaranteed to make progress here
         */
	flush_buffer( c, NO ); 
	
	/* having flushed the buffer, we have made progress.  So,
	 * recursively write the rest unless it fits in the buffer
	 * as is
	 */
	len -= m;
	ptr += m;
	
	m = c->lim - c->ptr;  /* new amount of space available */
	/* keep flushing until we get enough space to copy the rest in
	 * (since CREATE_EVENT_THRESHOLD is 1/2 the buffer size, we
	 * will usually just go through this once.  But looping gives
	 * is the flexibility of having a bigger event threshold)
	 */
      } while (len > m);

      memcpy( c->ptr, ptr, len );
      c->ptr += len;
    }
}

static _rs_inline void write_bytes( struct OutputCursor *c, 
                                    obj str, UINT_8 *ptr, UINT_32 len )
{
  if (c->flush == LOOK_FOR_NL) {
    if (memchr( ptr, '\n', len )) {
      c->flush = SAW_NL;
    }
  }

  if (c->ptr + len > c->lim)
    {
      write_bytes_ov( c, str, ptr, len );
    }
  else
    {
      memcpy( c->ptr, ptr, len );
      c->ptr += len;
    }
}

static _rs_inline void write_str( struct OutputCursor *c, obj str )
{
  write_bytes( c, str, string_text(str), string_length(str) );
}

static _rs_inline void write_byte( struct OutputCursor *c, UINT_8 ch )
{
  if (c->ptr >= c->lim) {
    /* flush_buffer() will ensure that at least BLOCK_SIZE() -
     * CREATE_EVENT_THRESHOLD() bytes are available, so we're
     * guaranteed to have room for the new char
     */
    flush_buffer( c, NO );
  }

  *(c->ptr)++ = ch;

  if ((c->flush == LOOK_FOR_NL) && (ch == '\n'))
    c->flush = SAW_NL;
}

static void c_writev( struct OutputCursor *c, obj vec )
{
  UINT_32 i;

  for (i=0; i<SIZEOF_PTR(vec); i+=SLOT(1))
    {
      obj item = gvec_ref( vec, i );
      if (STRING_P(item))
	{
	  write_str( c, item );
	}
      else if (BYTE_CHAR_P(item))
	{
	  write_byte( c, ASCII_CHAR_VALUE(item) );
	}
      else if (SYMBOL_P(item))
	{
	  write_str( c, symbol_str( item ) );
	}
      else if (FIXNUM_P(item))
	{
	  char temp[30];
	  int n;

	  n = sprintf( temp, "%ld", fx2int(item) );
	  write_bytes( c, FALSE_OBJ, temp, n );
	}
      else if (VECTOR_P(item))
	{
	  c_writev( c, item );
	}
      else if (BVEC_P(item))
	{
	  write_bytes( c, item, PTR_TO_DATAPTR(item), SIZEOF_PTR(item) );
	}
      else
	{
	  scheme_error( "writev: invalid item ~s", 1, item );
	}
    }
}

/***********************************************************************
 *
 *  interface to queued output manipulation
 *
 ***********************************************************************/
 
rs_bool qout_write_bytes( obj port, obj bvec, INT_32 offset, INT_32 len )
{
  struct OutputCursor c;

  open_cursor( &c, port );

  assert( offset >= 0 );
  assert( len >= 0 );
  assert( (offset + len) <= SIZEOF_PTR( bvec ) );

  write_bytes( &c, bvec, PTR_TO_DATAPTR( bvec ) + offset, len );
  close_cursor( &c );

  if (c.flush == SAW_NL)
    {
      return qout_flush(port);
    }
  else
    {
      return YES; /* ok to continue */
    }
}

rs_bool qout_writev( obj port, obj vec )
{
  struct OutputCursor c;
  open_cursor( &c, port );
  c_writev( &c, vec );
  close_cursor( &c );
  if (c.flush == SAW_NL)
    {
      return qout_flush(port);
    }
  else
    {
      return YES; /* ok to continue */
    }
}

rs_bool qout_flush( obj port )
{
  struct OutputCursor c;

  if (HAS_EVENT_Q(port))
    {
      int n = GET_BUFFER_IX(port);

      /*  If there is anything in the current buffer, 
       *  add it to the pending writes  */

      if (n) {
        if (DEBUG_QUEUED_WRITES) {
          printf( "  qout_flush: %d bytes at +0\n", n );
        }
        enq_wr( port, GET_BUFFER(port), ZERO, int2fx(n) );
        SET_BUFFER( port, alloc_buf( port ) );
        SET_BUFFER_IX( port, 0 );
      }
      dequeue_push_back( GET_PENDING_WRITES(port), current_thread );
      return NO;
    }
  else
    {
      open_cursor( &c, port );
      if (!write_to_flush( &c, YES ))
	{
	  close_cursor( &c );
	  dequeue_push_back( GET_PENDING_WRITES(port), current_thread );
	  return NO;
	}
      else
	{
	  close_cursor( &c );
	  return YES;
	}
    }
}

#define DEQ_STATE(deq)  gvec_ref((deq),SLOT(0))
#define DEQ_FRONT(deq)  gvec_ref((deq),SLOT(1))
#define DEQ_BACK(deq)  gvec_ref((deq),SLOT(2))

static obj dequeue_top( obj deq )
{
  return gvec_ref( DEQ_STATE(deq), FXWORDS_TO_RIBYTES(DEQ_FRONT(deq)) );
}

static void dispatch_write_error( obj port, obj err )
{
  obj top, deq = GET_PENDING_WRITES(port);

  while (!dequeue_empty(deq))
    {
      top = dequeue_pop_front(deq);

      if (VECTOR_P(top))
	{
	  if (DEBUG_QUEUED_WRITES)
	    {
	      obj str = gvec_ref( top, SLOT(0) );
	      int from, to, len;

	      from = fx2int( gvec_ref( top, SLOT(1) ) );
	      to = fx2int( gvec_ref( top, SLOT(2) ) );
	      
	      len = to - from;
	      
	      if (DEBUG_QUEUED_WRITES)
		printf( "   DISCARDING %d bytes -- [%d,%d) -- from {%#lx}\n",
			len, from, to, VAL(str) );
	    }
	}
      else
	{
	  /* not blocked on us any more */
          int rq = did_remove_from_queue( top );
          UNBLOCK_THREAD( top );

	  store_resume_value( top, err );
	  if (rq) {
            /* ready to run, in fact */
            mark_thread_ready( top );
          }
	  /*  otherwise, it's been marked blocked-on-nothing,
	   *  so when it gets resumed, it will continue on.
	   */
	}
    }
}

obj handle_std_write_event( struct sys_event *e, obj to_do )
{
  obj port = EVENT_TARGET(e);
  obj deq;

  deq = GET_PENDING_WRITES(port);

  if (DEBUG_QUEUED_WRITES)
    printf( " fd %d is writable: %ld write items pending\n",
	    GET_FD(port), fx2int( dequeue_count(deq) ) );

  assert( !dequeue_empty(deq) );

  /*  The pending writes queue has two kinds of constituents;
   *
   *    (1)  Buffers waiting to be written, represented as:
   *           #(<string> <from-offset> <to-offset>)
   *
   *    (2)  Threads waiting for the data to be flushed.
   *           (ie, blocked in qout-flush)
   */

  while (1)
    {
      obj top = dequeue_top(deq);

      if (VECTOR_P(top))
	{
	  int n;
	  obj str;
	  int from, to, len;
	  UINT_8 *p;

	  str = gvec_ref( top, SLOT(0) );
	  from = fx2int( gvec_ref( top, SLOT(1) ) );
	  to = fx2int( gvec_ref( top, SLOT(2) ) );

	  p = byte_string_text(str);
	  len = to - from;

	  if (DEBUG_QUEUED_WRITES)
	    printf( "   pending item ==> %d bytes -- [%d,%d) -- from {%#lx}\n",
		    len, from, to, VAL(str) );

	  n = do_write( port, p + from, len );
	  if (n < len)
	    {
	      if (n < 0)
		{
		  obj proc = gvec_ref( port, QOUT_ERROR_PROC );
		  obj err;

		  if (DEBUG_QUEUED_WRITES)
		    printf( "                    ERROR IN WRITE\n" );

		  err = make_os_error( "write", 1, int2fx(e->data.fd) );
		  free_write_event(e);

		  if (EQ(proc,FALSE_OBJ))
		    {
		      dispatch_write_error( port, err );
		    }
		  else
		    {
		      to_do = cons( thunkify_2( proc, port, err ), to_do );
		    }
		  return to_do;
		}
	      
	      if (DEBUG_QUEUED_WRITES)
		printf( "                    only wrote %d of %d bytes.\n",
			n, len );
	      gvec_write_non_ptr( top, SLOT(1), int2fx(from+n) );
	      return to_do;
	    }
	  /*  wrote the whole thing... pop this pending item and loop
	   *  around for another one (unless the queue is empty... see below)
	   */
	}
      else
	{
	  obj suspend_count = gvec_ref( top, THREAD_SUSPEND_COUNT );

	  if (DEBUG_QUEUED_WRITES)
	    printf( "   pending item ==> thread [%s] blocked on flush\n",
		    thread_name(top) );

	  /* the thread is no longer blocked on us... */

	  store_resume_value( top, FALSE_OBJ );
	  if (did_remove_from_queue( top )) {
            UNBLOCK_THREAD( top );
            /* if it wasn't suspended, mark it ready to rock-n-roll */
            mark_thread_ready( top );
          } else {
            UNBLOCK_THREAD( top );
          }

	  /*  otherwise, it's been marked blocked-on-nothing,
	   *  so when it gets resumed, it will continue on.
	   */
	}

      dequeue_pop_front(deq);
      if (dequeue_empty(deq))
	{
	  free_write_event(e);
	  return to_do;
	}
    }
}


