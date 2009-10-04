/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/intrs.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.21
 * File mod date:    2004-03-25 13:27:37
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Interrupt handling interface
 *------------------------------------------------------------------------*/

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <rscheme/intrs.h>
#include <rscheme/smemory.h>
#include <rscheme/vinsns.h>
#include <rscheme/runtime.h>
#include <rscheme/scheme.h>
#include <rscheme/osglue.h>
#if HAVE_WAIT
#include <sys/types.h>
#include <sys/wait.h>
#endif

#ifdef SIGUSR_HOOKS
int sigusr1_flag, sigusr2_flag;
#endif /* SIGUSR_HOOKS */

#ifdef __cplusplus
extern "C" {
#endif

/* static void set_interrupt_flag( int sig, int code, 
   struct sigcontext *scp, 
   char *addr )*/

static void set_interrupt_flag( int x )	/* signal handler */
{
  struct RSSIG_info sig;
  sig.signum = RSSIG_USER_INTR;
  os_enqueue_sig_from_handler( &sig );
}

#if HAVE_WAIT
#ifdef PLATFORM_NEXT
typedef int pid_t;
#endif

static void capture_child_exit_status( int x )
{
  struct RSSIG_info sig;
  pid_t proc;
  int status;

  while (1)
    {
      /* drain the queue of waiting children */

#if !HAVE_WAITPID
      proc = wait3( (void *)&status, WNOHANG, NULL );
#else
      proc = waitpid( -1, &status, WNOHANG );
#endif
      if (proc <= 0)
	break;

      sig.signum = RSSIG_CHILD_EXITED;
      sig.data.child_exited.process_id = proc;
      sig.data.child_exited.status = status;
      
      os_enqueue_sig_from_handler( &sig );
    }
}
#endif

#ifdef SIGUSR_HOOKS
static void set_sigusr_flag( int x )	/* signal handler */
{
  if (x == SIGUSR1)
    sigusr1_flag = 1;
  else if (x == SIGUSR2)
    sigusr2_flag = 1;
}
#endif /* SIGUSR_HOOKS */

/* used by corelib/intrglue.scm//setup-signal-handler */

void c_signal_catcher( int signum )
{
  struct RSSIG_info sig;

  sig.signum = RSSIG_C_SIGNAL;
  sig.data.c_signal.signal_number = signum;
  
  os_enqueue_sig_from_handler( &sig );
}

#ifdef __cplusplus
}
#endif

void init_interrupts( void )
{
  os_set_sigenable( NO );  /* start up with interrupts disabled;
			    * the scheme `start' procedure will enable
			    * them.  Note that this means from now until
			    * the start function begins, this process
			    * will effectively be ignoring SIGINT's
			    * (a better way to deal with this would
			    * be to NOT install the handler just yet;
			    * wait until a scheme-level handler is set)
			    */
  os_register_signal_handler( SIGINT, set_interrupt_flag );
#ifdef SIGUSR_HOOKS
  os_register_signal_handler( SIGUSR1, set_sigusr_flag );
  os_register_signal_handler( SIGUSR2, set_sigusr_flag );
#endif /* SIGUSR_HOOKS */
}

void enable_subprocess_capture( void )
{
#if HAVE_WAIT
  os_register_signal_handler( SIGCHLD, capture_child_exit_status );
#endif
}

void rscheme_intr_call0( obj thunk )
{
  struct RSSIG_info s;
  s.signum = RSSIG_CALL0;
  s.data.call.proc = thunk;
  os_enqueue_sig( &s );
}

void rscheme_intr_call1( obj proc, obj arg )
{
  struct RSSIG_info s;
  s.signum = RSSIG_CALL1;
  s.data.call.proc = proc;
  s.data.call.data[0] = arg;
  os_enqueue_sig( &s );
}

void rscheme_intr_call2( obj proc, obj arg1, obj arg2 )
{
  struct RSSIG_info s;
  s.signum = RSSIG_CALL2;
  s.data.call.proc = proc;
  s.data.call.data[0] = arg1;
  s.data.call.data[1] = arg2;
  os_enqueue_sig( &s );
}

jump_addr dispatch_interrupt( jump_addr would_be_next )
{
jump_addr f;
struct RSSIG_info nxt;
unsigned n = 0;
obj proc;

    save_cont( arg_count_reg, would_be_next );

    literals_reg = continue_intr_tmpl;

    f = OBJ_TO_JUMP_ADDR( gvec_read( literals_reg, SLOT(0) ) );

    /* PUSH_PARTCONT_ADDR() assumes it is at the beginning of a block... */
    {
    PUSH_PARTCONT_ADDR(f,0);
    }

    /* `os_get_next_sig' disables interrupts, too */

    nxt = os_get_next_sig();
    proc = gvec_ref( interrupt_handlers, SLOT(nxt.signum) );
    switch (nxt.signum)
      {
      case RSSIG_USER_INTR:
      case RSSIG_TIMEOUT:
      case RSSIG_GC_FLIP:
	n = 0;
	break;
      case RSSIG_FINALIZE:
	REG0 = nxt.data.finalize.finalize_list;
	n = 1;
	break;
      case RSSIG_CALL0:
        proc = nxt.data.call.proc;
        n = 0;
        break;
      case RSSIG_CALL1:
        proc = nxt.data.call.proc;
        REG0 = nxt.data.call.data[0];
        n = 1;
        break;
      case RSSIG_CALL2:
        proc = nxt.data.call.proc;
        REG0 = nxt.data.call.data[0];
        REG1 = nxt.data.call.data[1];
        n = 2;
        break;
      case RSSIG_C_SIGNAL:
	{
	  int sig = nxt.data.c_signal.signal_number;
	  char temp[30];
	  obj name;

	  if (SLOT(sig) < SIZEOF_PTR(c_signal_names))
	    name = gvec_ref( c_signal_names, SLOT(sig) );
	  else
	    name = FALSE_OBJ;
	  if (EQ(name,FALSE_OBJ))
	    {
	      sprintf( temp, "unknown-%d", sig );
	      name = intern( make_string(temp) );
	    }
	  REG0 = name;
	  n = 1;
	  break;
	}
      case RSSIG_CHILD_EXITED:
#if HAVE_WAIT
	{
#if _BSD >= 43
          union wait stat;
          stat.w_status = nxt.data.child_exited.status;
#else
#ifdef PLATFORM_NEXT

#  define WEXITSTATUS(s) ((s).w_retcode)
#  define WTERMSIG(s) ((s).w_termsig)
	  union wait stat;
	  stat.w_status = nxt.data.child_exited.status;
#else
	  int stat = nxt.data.child_exited.status;
#endif
#endif
	  REG0 = int2fx( nxt.data.child_exited.process_id );
	  if (WIFSIGNALED(stat))
	    {
	      REG1 = lookup_symbol( "signalled" );
	      REG2 = int2fx( WTERMSIG(stat) );
	    }
	  else
	    {
	      /*  we didn't say WUNTRACED in wait(), so WIFEXITED
	       *  should be the only other possibility
	       */
	      assert( WIFEXITED(stat) );
	      REG1 = lookup_symbol( "exited" );
	      REG2 = int2fx( WEXITSTATUS(stat) );
	    }
	  n = 3;
	}
#else
      fprintf( stderr, "CHILD_EXITED but didn't wait()" );
      abort();
#endif
      break;
      }
    arg_count_reg = n;
    assert( FUNCTION_P(proc) );
    return apply(proc);
}

static struct {
  char *signame;
  int signum;
} the_signals[] = {
#ifdef SIGHUP
  { "SIGHUP", SIGHUP },
#endif
#ifdef SIGINT
  { "SIGINT", SIGINT },
#endif
#ifdef SIGQUIT
  { "SIGQUIT", SIGQUIT },
#endif
#ifdef SIGILL
  { "SIGILL", SIGILL },
#endif
#ifdef SIGTRAP
  { "SIGTRAP", SIGTRAP },
#endif
#ifdef SIGABRT
  { "SIGABRT", SIGABRT },
#endif
#ifdef SIGIOT
  { "SIGIOT", SIGIOT },
#endif
#ifdef SIGBUS
  { "SIGBUS", SIGBUS },
#endif
#ifdef SIGFPE
  { "SIGFPE", SIGFPE },
#endif
#ifdef SIGKILL
  { "SIGKILL", SIGKILL },
#endif
#ifdef SIGUSR1
  { "SIGUSR1", SIGUSR1 },
#endif
#ifdef SIGSEGV
  { "SIGSEGV", SIGSEGV },
#endif
#ifdef SIGUSR2
  { "SIGUSR2", SIGUSR2 },
#endif
#ifdef SIGPIPE
  { "SIGPIPE", SIGPIPE },
#endif
#ifdef SIGALRM
  { "SIGALRM", SIGALRM },
#endif
#ifdef SIGTERM
  { "SIGTERM", SIGTERM },
#endif
#ifdef SIGSTKFLT
  { "SIGSTKFLT", SIGSTKFLT },
#endif
#ifdef SIGCHLD
  { "SIGCHLD", SIGCHLD },
#endif
#ifdef SIGCONT
  { "SIGCONT", SIGCONT },
#endif
#ifdef SIGSTOP
  { "SIGSTOP", SIGSTOP },
#endif
#ifdef SIGTSTP
  { "SIGTSTP", SIGTSTP },
#endif
#ifdef SIGTTIN
  { "SIGTTIN", SIGTTIN },
#endif
#ifdef SIGTTOU
  { "SIGTTOU", SIGTTOU },
#endif
#ifdef SIGURG
  { "SIGURG", SIGURG },
#endif
#ifdef SIGXCPU
  { "SIGXCPU", SIGXCPU },
#endif
#ifdef SIGXFSZ
  { "SIGXFSZ", SIGXFSZ },
#endif
#ifdef SIGVTALRM
  { "SIGVTALRM", SIGVTALRM },
#endif
#ifdef SIGPROF
  { "SIGPROF", SIGPROF },
#endif
#ifdef SIGWINCH
  { "SIGWINCH", SIGWINCH },
#endif
#ifdef SIGIO
  { "SIGIO", SIGIO },
#endif
#ifdef SIGPWR
  { "SIGPWR", SIGPWR },
#endif
#ifdef SIGUNUSED
  { "SIGUNUSED", SIGUNUSED },
#endif
#ifdef SIGDANGER
  { "SIGDANGER", SIGDANGER },
#endif
  { NULL, 0 } };

int rs_c_signal_num( obj name )
{
  int i, n;

  n = SIZEOF_PTR(c_signal_names);

  for (i=0; i<n; i+=SLOT(1))
    if (EQ(name, gvec_ref(c_signal_names,i)))
      return i/SLOT(1);

  return -1;
}

void rs_init_c_signals( void )
{
  int i, num, max_sig;
  obj name_vec;

  max_sig = 0;
  for (i=0; the_signals[i].signame; i++)
    {
      if (the_signals[i].signum > max_sig)
	max_sig = the_signals[i].signum;
    }
  name_vec = alloc( SLOT(max_sig+1), vector_class );

  for (i=0; i<=max_sig; i++)
    gvec_write_init_non_ptr( name_vec, SLOT(i), FALSE_OBJ );

  for (i=0; the_signals[i].signame; i++)
    {
      gvec_write_fresh_ptr( name_vec, 
			    SLOT(the_signals[i].signum),
			    intern( make_string(the_signals[i].signame) ) );
    }
  c_signal_names = name_vec;
}
