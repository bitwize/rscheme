/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/intrs.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.13
 * File mod date:    2004-03-25 13:28:22
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Scheme-level interrupt handling
 *------------------------------------------------------------------------*/

#ifndef _H_INTRS
#define _H_INTRS

#include <rscheme/obj.h>

enum RSSIGNUM {
  RSSIG_USER_INTR = 0,
  RSSIG_TIMEOUT = 1,
  RSSIG_CHILD_EXITED = 2,
  RSSIG_FINALIZE = 3,
  RSSIG_GC_FLIP = 4,
  RSSIG_C_SIGNAL = 5,
  RSSIG_CALL0 = 6,
  RSSIG_CALL1 = 7,
  RSSIG_CALL2 = 8
};

struct RSSIG_info {
  enum RSSIGNUM  signum;
  union {
    struct {
      UINT_32  process_id;
      int      status;
    } child_exited;
    struct {
      int      signal_number;
    } c_signal;
    struct {
      obj      finalize_list;
      UINT_32  count;
    } finalize;
    struct {
      obj       proc;
      obj       data[2];
    } call;
  } data;
#if TRACE_SIGNAL_LATENCY
  struct RSTime  queued_time;  /* when the signal was queued */
#endif
};

#define RSSIG_LATENCY_TOLERANCE  (2)  /* in milliseconds */

/*
    This function gets called to handle an interrupt.

    It should save the state of the currently executing
    thread, set up the call to the scheme-level signal handler,
    and return its address.

    (It gets called "soon" whenever rssig_ready = YES)
*/

jump_addr dispatch_interrupt( jump_addr would_be_next );

/*
    Initialize interrupts
    Install handlers for ^C, etc.
*/

void init_interrupts( void );
void enable_subprocess_capture( void ); /* init SIGCHLD handling */

int rs_c_signal_num( obj name );
void rs_init_c_signals( void );

#ifdef __cplusplus
extern "C" {
#endif
void c_signal_catcher( int sig );   /* generic C signal handler
				     * (enqueues RSSIG_C_SIGNAL + n)
				     */
#ifdef __cplusplus
}
#endif

#ifndef cast_to_signal_handler
#define cast_to_signal_handler(x) x
#endif

#ifdef SIGUSR_HOOKS
extern int sigusr1_flag, sigusr2_flag;
void run_sigusr_hook( int sigusr_num );  /* 1 or 2 */
#endif /* SIGUSR_HOOKS */

#endif /* _H_INTRS */
