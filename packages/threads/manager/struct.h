#ifndef _H_STRUCT
#define _H_STRUCT

#include <rscheme.h>
#include <rscheme/vinsns.h>

enum thread_state {
  TSTATE_SUSPEND = 0,
  TSTATE_WAITING,
  TSTATE_RUNNING,
  TSTATE_BLOCKED,
  TSTATE_SLEEPING,
  TSTATE_COMPLETE
};

#define THREAD_STATE                  SLOT(0)
#define THREAD_STACK                  SLOT(1)
#define THREAD_VARS                   SLOT(2)
#define THREAD_DYNAMIC_STATE          SLOT(3)
#define THREAD_TIME		      SLOT(4)
#define THREAD_GROUP		      SLOT(5)
#define THREAD_PRIORITY		      SLOT(6)
#define THREAD_BLOCKED_ON	      SLOT(7)
#define THREAD_NEXT_THREADTIME_EVENT  SLOT(8)
#define THREAD_JOINS                  SLOT(9)
#define THREAD_NAME                  SLOT(10)
#define THREAD_NUMBER                SLOT(11)
#define THREAD_SUSPEND_COUNT         SLOT(12)

#define MBOX_INPUT_PORT_MBOX          SLOT(3)
#define MBOX_INPUT_PORT_EVENT         SLOT(4)

#define PACKET_PORT_MBOX              SLOT(0)
#define PACKET_PORT_EVENT             SLOT(1)
#define PACKET_PORT_SOCKADDR_CLASS    SLOT(2)

#define SERVICE_MBOX                  SLOT(0)
#define SERVICE_EVENT                 SLOT(1)

#define QOUT_BUFFER                   SLOT(0)
#define QOUT_BUFFER_INDEX             SLOT(1)
#define QOUT_PENDING_WRITES           SLOT(2)
#define QOUT_EVENT                    SLOT(3)
#define QOUT_FD                       SLOT(4)
#define QOUT_BLOCK_SIZE               SLOT(5)
#define QOUT_ERROR_PROC               SLOT(6)
#define QOUT_FLUSH_ON_NL              SLOT(7)

#define DEQ_LEN                       SLOT(3)

#define MAILBOX_HAS_DATA_Q            DEQ_LEN + SLOT(0)
#define MAILBOX_LEN                   DEQ_LEN + SLOT(1)

#define SEMAPHORE_COUNT               DEQ_LEN + SLOT(0)
#define SEMAPHORE_LEN                 DEQ_LEN + SLOT(1)

#define XQUEUE_EVENT                  MAILBOX_LEN + SLOT(0)
#define XQUEUE_DISPLAY                MAILBOX_LEN + SLOT(1)
#define XQUEUE_LEN                    MAILBOX_LEN + SLOT(2)

#define INITIATOR_SOCKET_FILEDES      SLOT(0)
#define INITIATOR_SOCKET_SADDR        SLOT(1)
#define INITIATOR_SOCKET_EVENT        SLOT(5)
#define INITIATOR_SOCKET_WAITER       SLOT(6)

#define SELECT_EVENT_EVENT            SLOT(1)
#define SELECT_EVENT_WAITER           SLOT(2)

#define FUTURE_FUTURE_WAITERS         SLOT(0)

/* thread variables and thread system variables */
/* (note that we want to store pointers to internal structures
    in the roots list rather than in regular variables, in case
    we have a copying GC or other object movement capability)
*/

#define NUM_THREAD_ROOTS (23)

extern obj thread_sys_root[NUM_THREAD_ROOTS];

#define current_thread  thread_sys_root[0]
#define target_vec      thread_sys_root[1]
#define thread_queue    (thread_sys_root+2)  /* 10 values */
#define kernel_proc     thread_sys_root[12]
#define thunkifier_template thread_sys_root[13]
#define thunkifier_class    thread_sys_root[14]
#define x_event_class       thread_sys_root[15]

/*   need these classes for telling what to do on resume
 *   (see sync.c:requeue())
 *
 *   these roots are initialized by *thread-sys-classes*
 *   in cooperation with init_threads()
 */

#define thread_class            thread_sys_root[16]
#define semaphore_class         thread_sys_root[17]
#define qout_class              thread_sys_root[18]
#define mailbox_class           thread_sys_root[19]
#define initiator_socket_class  thread_sys_root[20]
#define process_class           thread_sys_root[21]
#define future_class            thread_sys_root[22]
/*
 *   classes for signalling exceptions
 */

/*...*/

/* primitives */

jump_addr switch_thread( obj waiting_on, enum thread_state new_status );
jump_addr dispatch_to_next_thread( void );
jump_addr did_timeout( void );

/*  helper procedure for suspensions; 

    if the suspend count is 0,
      does nothing except return 1

    if the suspend count is nonzero,
      then it is presumed to be positive
           (ie, it's being removed from the queue, 
           hence it must have been in it to start),
           and it is made negative to mark the
	   fact that the thread is not longer queued
*/	   

int did_remove_from_queue( obj thr );

void mark_thread_ready( obj thr );
void mark_thread_ready_1( obj thr, obj item );

void mark_thread_ready_soon( obj thr );
void store_resume_value( obj thr, obj item );

void mark_thread_suspended( obj th );
void mark_thread_resumed( obj th );

#define SWITCH_THREAD(w,s) return switch_thread( w, s )

/* mark a thread as not blocked on anything */
#define UNBLOCK_THREAD(w)  do { \
              gvec_write_non_ptr( (w), THREAD_BLOCKED_ON, ZERO ); \
              assert( FX_LE( gvec_ref( (w), THREAD_SUSPEND_COUNT ), ZERO ) );\
            } while (0)

void init_threads( obj classes, obj kproc, obj thnk_template );

int get_current_thread_id( void );

#define DEBUG_THREADS         (0)
#define DEBUG_THREAD_SWITCH   (DEBUG_THREADS)
#define DEBUG_THREAD_EVENTS   (DEBUG_THREADS)
#define DEBUG_THREAD_MBOX     (DEBUG_THREADS)
#define DEBUG_UNCOMMON_CASES  (0)
#define DEBUG_QUEUED_WRITES   (0)
#define DEBUG_SUBPROCESS      (0)

#define SUPPORT_X             (0)

/* set to TRUE if we are willing to let read() block when called 
 * without the precondition of select() having returned READY
 * (ie, if TRUE, we won't try to do a fcntl NDELAY)
 */

#define PERMIT_BLOCKING_READS (1)

/*  amount of time (in us) we are willing to forgo doing a select(),
 *  in order to save some context switch time
 */

#define ALLOW_SELECT_LATENCY  (200)

#endif /* _H_STRUCT */
