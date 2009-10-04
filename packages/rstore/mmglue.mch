#include <mach/mach.h>
#include <mach/message.h>
#include <mach/exception.h>
#include <mach/mach_error.h>
#include <pthread.h>

/*
 *  I have no idea where to find these prototypes...
 *  I am hand-copying them from the docs in apple xnu/osfmk/man/
 */

extern boolean_t exc_server( mach_msg_header_t *request_msg,
                             mach_msg_header_t *reply_msg );

/*  OK, that's it */

#define DEBUG_0  (1)

/*** Exception Server ***/

typedef struct {
    port_t old_exc_port;
    port_t clear_port;
    port_t exc_port;
} ports_t;
static ports_t         ports;

volatile boolean_t  pass_on;

kern_return_t catch_exception_raise( port_t exception_port, 
                                     port_t thread, 
                                     port_t task, 
                                     int exception, 
                                     int code, 
                                     int subcode )
{
    if (exception == EXC_BAD_ACCESS)
    {
#ifdef DEBUG_0
	printf( "****[EXC_BAD_ACCESS]**** %#x: kern_return_t = %s (%d)\n",
		(unsigned) subcode,
		mach_error_string(code), 
		code );
#endif DEBUG_0
        mmc_access_failed( (void *)subcode );
	pass_on = FALSE;
    }
    /*  Returning KERN_SUCCESS means that the thread
     *  which encountered the exception should continue
     *  from the point at which the exception was raised
     */
    return KERN_SUCCESS;
}


/* Listen on the exception port. */

static void *exc_thread( void *targ )
{
ports_t *port_p = (ports_t *)targ;
kern_return_t   r;
char           *msg_data[2][64];
mach_msg_header_t   *imsg = (mach_msg_header_t *)msg_data[0],
	   	    *omsg = (mach_msg_header_t *)msg_data[1];

    /* Wait for ANY exceptions. */
    
again:
    pass_on = TRUE;
    imsg->msgh_size = 64;
    imsg->msgh_local_port = port_p->exc_port;
    r = mach_msg_receive(imsg);

    if (r == MACH_MSG_SUCCESS) 
    {
	/* Give the message to the Mach exception server. */
	if (exc_server(imsg, omsg)) 
	{
	    /* Send the reply message that exc_serv gave us. */
	    r = mach_msg_send(omsg);
	    if (r != MACH_MSG_SUCCESS) 
	    {
		mach_error("msg_send", r);
		exit(1);
	    }
	}
	else 
	{ /* exc_server refused to handle imsg. */
	    printf("exc_server didn't like the message\n");
	    exit(2);
	}
    }
    else 
    { /* msg_receive() returned an error. */
    
	mach_error( "msg_receive", r );
	exit(3);
    }

    /* Pass the message to old exception handler, if necessary. */
    if (pass_on == TRUE) 
    {    
	imsg->msgh_remote_port = port_p->old_exc_port;
	imsg->msgh_local_port = port_p->clear_port;
	r = mach_msg_send(imsg);
	if (r != MACH_MSG_SUCCESS) 
	{
	    mach_error( "mach_msg_send to old_exc_port", r);
	    exit(4);
	}
    }
    goto again;
}

void init_mm( void )
{
  kern_return_t r;
  int old_exccnt = 10;
  exception_mask_t old_maskv[10];
  mach_port_t old_portv[10];
  exception_behavior_t old_behaviorv[10];
  thread_state_flavor_t old_flavorv[10];


  /* Create a new exception port for this task. */
  r = mach_port_allocate( mach_task_self(), 
                          MACH_PORT_RIGHT_RECEIVE,
                          &(ports.exc_port) );
  if (r != KERN_SUCCESS) {
    mach_error("port_allocate 0", r);
    exit(1);
  }
  printf( "new port (rc %d) => 0x%x\n", r, ports.exc_port );
    
    /*  Only set up an exception handler for the main thread.
     *  Exceptions on other threads should be handled normally
     *  (note that "other threads" includes the thread that is
     *  going to actually read in the faulted data)
     */

    r = thread_swap_exception_ports( mach_thread_self(),
                                     EXC_MASK_BAD_ACCESS,
                                     ports.exc_port,
                                     EXCEPTION_DEFAULT,     /* behavior */
                                     0,                     /* flavor */
                                     old_maskv,
                                     &old_exccnt,
                                     old_portv,
                                     old_behaviorv,
                                     old_flavorv );


    printf( "swap (rc %d:%d:%d)\n", 
            err_get_system(r),
            err_get_sub(r),
            err_get_code(r) );
    printf( "   exc cnt %d\n", old_exccnt );
    { 
      int i;
      for (i=0; i<old_exccnt; i++) {
        printf( "   [%d] mask 0x%08x  port 0x%x  behavior %d  flavor %d\n",
                i,
                old_maskv[i],
                old_portv[i],
                old_behaviorv[i],
                old_flavorv[i] );
      }
      printf( "ok.\n" );
    }

#if 0
    /* Save the old exception port for this task. */
    r = task_get_exception_port(task_self(), &(ports.old_exc_port));
    if (r != KERN_SUCCESS) {
        mach_error("task_get_exception_port", r);
        exit(1);
    } 

    r = task_set_exception_port( mach_task_self(), (ports.exc_port));
    if (r != KERN_SUCCESS) {
        mach_error("task_set_exception_port", r);
        exit(1);
    }
#endif

    {
      pthread_t t;
      pthread_create( &t, NULL, exc_thread, (void *)&ports );
      pthread_detach( t );
      ports.clear_port = mach_thread_self();
    }
}


void mm_set_prot( void *page_start, size_t bytes, enum mm_mode prot )
{
kern_return_t rc;
static vm_prot_t prots[3] = { 0, VM_PROT_READ, VM_PROT_READ|VM_PROT_WRITE };

    rc = vm_protect( mach_task_self(), 
                     (vm_address_t)page_start,
		     bytes,
		     /* set_max */ FALSE,
		     prots[prot] );
    if (rc) {
      mach_error( "vm_protect", rc );
    }
}


static void raw_mm_free( void *base, size_t bytes )
{
  vm_deallocate( mach_task_self(), (vm_address_t)base, bytes );
}

void *mm_alloc( size_t bytes, enum mm_mode mode )
{
kern_return_t rc;
vm_address_t addr;

    rc = vm_allocate( mach_task_self(), &addr, 
		      bytes, /* anywhere */ TRUE );
    if (mode != MM_MODE_READ_WRITE)
	mm_set_prot( (void *)addr, bytes, mode );
    return (void *)addr;
}


#ifdef UNIT_TEST

void *p_base;

void mmc_access_failed( void *addr )
{
  printf( "FAULT at %p (delta = %d)\n", addr, (char *)addr - (char *)p_base );
  exit(3);
}

int main( int argc, char *const *argv )
{
  void *p;
  unsigned *w;

  init_mm();

  p = mm_alloc( 10*PAGE_SIZE, 0 );
  p_base = p;
  printf( "p => %p\n", p );
  w = (unsigned *)p;

  mprotect( p, 10*PAGE_SIZE, 0 );

  printf( "ready?\n" );
  w[100] = 132;
  printf( "all's well that ends well\n" );
  return 0;
}

#endif
