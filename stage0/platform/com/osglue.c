/*-----------------------------------------------------------------*-C-*---
 * File:    handc/platform/com/osglue.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.40
 * File mod date:    2007-05-30 06:50:15
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Low-level OS glue for most systems
 *------------------------------------------------------------------------*
 * Notes:
 *      This file relies on configure to set a bunch of #define flags
 *      (see dist/config.h.in for the template).  Those flags control
 *      the compilation of the various parts of this file.
 *------------------------------------------------------------------------*/

#include <stdlib.h>
#include <stdarg.h>

#ifdef __cplusplus
extern "C" {
#endif

#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#ifdef BSD_UNIX
#include <sys/param.h>
#endif
#include <errno.h>
#include <unistd.h>
#include <string.h>

#ifdef PLATFORM_AIX
 int setitimer( int, const struct itimerval *, struct itimerval * );
 int getitimer( int, struct itimerval * );
#endif

#ifdef __cplusplus
}
#endif

#include <rscheme/scheme.h>
#include <rscheme/intrs.h>
#include <rscheme/regs.h>
#include <rscheme/osglue.h>
#include <rscheme/allocns.h>
#include <rscheme/profile.h>
#include <rscheme/runtime.h>

static void fprintsi( FILE *f, struct RSSIG_info *rssig );

/*
 *  this is an optional file descriptor for delivering notice of
 *  signals.  If >= 0, then a character is written to the fd
 *  when a signal is inserted into the sig_queue
 */

int sigq_notify_fd = -1;


rs_bool rssig_ready;

static rs_bool rssig_enable = YES;
static struct RSSIG_info *sig_queue = NULL;
static unsigned max_sig;
unsigned num_sig = 0;

void os_prof_mark( void )
{
  rsprof_prof_mark( 200, num_sig, max_sig );
  rsprof_prof_mark( 201, 
                    (rssig_ready ? 10 : 0) + (rssig_enable ? 1 : 0), 
                    0 );
}

struct RSTime os_current_time( void )
{
  struct timeval tv;
  struct RSTime t;

  gettimeofday( &tv, NULL );
  t.rstime[0] = tv.tv_sec;
  t.rstime[1] = tv.tv_usec;
  return t;
}

struct RSTime rstime_sub( struct RSTime a, struct RSTime b )
{
  struct RSTime d;

  d.rstime[0] = a.rstime[0] - b.rstime[0];
  d.rstime[1] = a.rstime[1] - b.rstime[1];
  if (d.rstime[1] < 0)
    {
      d.rstime[1] += 1000000;
      d.rstime[0] -= 1;
    }
  return d;
}

IEEE_64 rstime_to_sec( struct RSTime a )
{
  return (a.rstime[0] * 1.0) + (a.rstime[1] * 1.0e-6);
}


/* returns true if something was found; updates the index
   to support incremental scanning (though there is an atomicity
   presumption, which says that when doing the scan-to-flip, nothing
   of interest will be inserted between the time this is first called
   with index->0 and when it returns false (0)
*/

int os_scan_sig_queue( unsigned *index, obj *item )
{
  unsigned i, n = num_sig;
  obj candidate;

  /* nothing will be dequeued during this scan, and
   * nothing of interest will be inserted, so we don't
   * need a critical section
   */
  for (i=*index; i<n; i++)
    {
      candidate = FALSE_OBJ;
      if (sig_queue[i].signum == RSSIG_FINALIZE)
	  candidate = sig_queue[i].data.finalize.finalize_list;
      
      if (OBJ_ISA_PTR(candidate))
	{
	  *index = i+1;
	  *item = candidate;
	  return 1;
	}
    }
  *index = i;
  return 0;
}

obj vmake_os_error( const char *fn, int num_args, va_list va )
{
  int i, e = errno;
  obj argv, props;

  argv = alloc( SLOT(num_args), vector_class );

  for (i=0; i<num_args; i++)
    gvec_write_init( argv, SLOT(i), va_arg(va,obj) );

  props = cons( cons( lookup_symbol( "stack" ), 
                      make_exception_stack() ), 
                NIL_OBJ );

  return make4( os_error_class, 
                props,
                int2fx(e), 
                make_string( fn ),
                argv );
}

obj make_os_error( const char *fn, int num_args, ... )
{
  obj e;

  va_list va;
  va_start( va, num_args );
  e = vmake_os_error( fn, num_args, va );
  va_end( va );
  return e;
}

void os_error( const char *fn, int num_args, ... )
{
  obj e;

  va_list va;
  va_start( va, num_args );
  e = vmake_os_error( fn, num_args, va );
  va_end( va );
  raise_error( e );
}

obj os_errormsg( int err_code )
{
  return make_string( strerror(err_code) );
}

const char *os_getenv( const char *key )
{
#if HAVE_GETENV
const char *v;

    OUT_CALL( v = getenv( key ); );
    return v;
#else
    return NULL;
#endif
}

/* convert a unix-standard pathname to local OS format */

obj os_path( obj path )
{
    return path;
}

void os_mkdir( const char *path )
{
#if HAVE_MKDIR
  int rc;

  OUT_CALL( rc = mkdir( path, 0777 ); );
  if (rc < 0)
    os_error( "mkdir", 1, make_string(path) );
#else
  errno = ENOSYS;
  os_error( "mkdir", 1, make_string(path) );
#endif
}

/* check to see if a file exists */

rs_bool os_file_exists_p( const char *path )
{
#if HAVE_STAT
int rc;
struct stat info;

  OUT_CALL( rc = stat( path, &info ); );
  return rc ? NO : YES;
#else
FILE *f = fopen( path, "r" );

   if (f)
     {
       fclose(f);
       return YES;
     }
   else
     return NO;
#endif
}

/* open a file using unix-std pathnames */

FILE *os_fopen( const char *path, const char *mode )
{
const char *home;

    if (path[0] == '~' 
	&& (path[1] == '/' || path[1] == 0)
	&& (home = getenv("HOME")))
    {
      char *buf, temp[500];
      FILE *f;
      int len = strlen(path) + strlen(home) + 1;

      if (len < 500)
	buf = temp;
      else 
	buf = (char *)malloc(len);

      strcpy( buf, home );
      strcat( buf, path+1 );
      f = fopen( buf, mode );
      if (len >= 500)
	free( buf );
      return f;
    }
    else
	return fopen( path, mode );
}

static void set_timer_flag( void )
{
  struct RSSIG_info sig;
  sig.signum = RSSIG_TIMEOUT;
  os_enqueue_sig( &sig );
}

/* the real-time timer... */

void os_set_timer( UINT_32 msec )
{
  if (msec == 0)
    {
      /* this is easy! */
      set_timer_flag();
    }
  else
    {
#ifdef TIMER_IS_MONOTONE_COUNTER
      system_timeout = msec;
#else
      struct itimerval newt;
      
      newt.it_interval.tv_sec = 0;
      newt.it_interval.tv_usec = 0;
      newt.it_value.tv_sec = msec / 1000;
      newt.it_value.tv_usec = (msec % 1000) * 1000;
      setitimer( ITIMER_REAL, &newt, NULL );
#endif
    }
}

UINT_32 os_get_time_remaining( void )
{
#ifdef TIMER_IS_MONOTONE_COUNTER
    return system_timeout;
#else
struct itimerval t;

    getitimer( ITIMER_REAL, &t );
    return (t.it_value.tv_sec * 1000) + (t.it_value.tv_usec / 1000);
#endif
}

#if HAVE_GETTIMEOFDAY
UINT_32 os_sleep( UINT_32 msec )
{
  struct timeval t, t1, t2;

  gettimeofday( &t1, NULL );
  t.tv_usec = (msec % 1000) * 1000;
  t.tv_sec = msec / 1000;
  select( 0, NULL, NULL, NULL, &t );
  gettimeofday( &t2, NULL );

  return (t2.tv_sec - t1.tv_sec) * 1000 + (t2.tv_usec - t1.tv_usec) / 1000;
}
#endif

#ifndef TIMER_IS_MONOTONE_COUNTER
static void timeout_handler( int sig )
{
  set_timer_flag();
}
#endif

int os_register_signal_handler( int sig, void (*proc)( int ) )
{
#if !USE_POSIX_SIGNALS
  struct sigvec sv;
  
  sv.sv_handler = proc;
  sv.sv_mask = ~0;
  sv.sv_flags = 0;
  return sigvec( sig, &sv, NULL );
#else
  struct sigaction sa;
  
  if (proc) {
    sa.sa_handler = proc;
  } else {
    sa.sa_handler = SIG_IGN;    /* NULL => ignore */
  }
  sigfillset( &sa.sa_mask );

#ifdef SA_RESTART /* SunOS 4.1 doesn't have SA_RESTART */
  sa.sa_flags = SA_RESTART;
#else
  sa.sa_flags = 0;
#endif

  return sigaction( sig, &sa, NULL );
#endif
}

void init_os( void )
{
  set_sigqueue_size( 20 );
#ifndef TIMER_IS_MONOTONE_COUNTER
  os_register_signal_handler( SIGALRM, timeout_handler );
#endif
}

/*
 *  Provide a "checked" malloc() that will abort if
 *  it can't get the memory
 */

void *os_malloc( size_t size )
{
  void *ptr = malloc( size );
  if (!ptr) {
    fprintf( stderr, "could not malloc(%lu)\n", (unsigned long)size );
    abort();
  }
  return ptr;
}

void *os_malloc_or_null( size_t size )
{
  return malloc( size );
}


static obj cis( const char *str, obj rest )
{
  return cons( intern( make_string(str) ), rest );
}

#if PLATFORM_ARCH_PPC
#define PLATFORM_ARCH_STRING "powerpc"
#elif PLATFORM_ARCH_I386
#define PLATFORM_ARCH_STRING "i386"
#elif PLATFORM_ARCH_MIPS
#define PLATFORM_ARCH_STRING "mips"
#elif PLATFORM_ARCH_SPARC
#define PLATFORM_ARCH_STRING "sparc"
#elif PLATFORM_ARCH_M68K
#define PLATFORM_ARCH_STRING "sparc"
#elif PLATFORM_ARCH_ALPHA
#define PLATFORM_ARCH_STRING "alpha"
#elif PLATFORM_ARCH_S390
#define PLATFORM_ARCH_STRING "s390"
#elif PLATFORM_ARCH_X86_64
#define PLATFORM_ARCH_STRING "x86-64"
#endif

/*****************************************************************
 *  Returns a list of symbols describing the current system.
 *  The scheme procedure `(os-type)' calls this function.
 *
 *  We don't expect this to be called frequently, so we don't
 *  bother to try to cache the results or anything.
 *****************************************************************/

obj os_type( void )
{
  char bits[20];
  obj tail = NIL_OBJ;

#if CPP_SUPPORTS_ISYSTEM
  tail = cis( "cpp-isystem", tail );
#endif

#ifdef INLINES
  tail = cis( "inlines", tail );
#endif

#if FULL_NUMERIC_TOWER
  tail = cis( "full-numeric-tower", tail );
#endif

#ifdef TIMER_IS_MONOTONE_COUNTER
  tail = cis( "fake-time", tail );
#endif

#if defined(PLATFORM_IS_BIG_ENDIAN)
  tail = cis( "big-endian", tail );
#elif defined(PLATFORM_IS_LITTLE_ENDIAN)
  tail = cis( "little-endian", tail );
#endif

  sprintf( bits, "word-size-%d", WORD_SIZE_BITS );
  tail = cis( bits, tail );

#ifdef PLATFORM_ARCH_STRING
  tail = cis( PLATFORM_ARCH_STRING, tail );
#endif

  return cis( "unix", cis( "posix", cis( PLATFORM_TYPE, tail ) ) );
}

#if HAVE_GETCWD
obj os_getwd( void )
{
  char temp[1024];

  if (getcwd( temp, 1024 ))
    {
      return make_string(temp);
    }
  else if (errno == ERANGE)
    {
      /* the real path is too big to fit in our tiny array */
      char *p = (char *)getcwd( NULL, 0 );
      obj r;
      r = make_string(p);
      free(p);
      return r;
    }
  else
    {
      os_error( "getcwd", 0 );
      return FALSE_OBJ;
    }
}
#else
#if HAVE_GETWD

#ifndef MAXPATHLEN
#define MAXPATHLEN (1024+1)
#endif

obj os_getwd( void )
{
  char temp[MAXPATHLEN];

  if (getwd( temp ))
    {
      return make_string(temp);
    }
  else
    {
      os_error( "getcwd", 0 );
      return FALSE_OBJ;
    }
}
#else
obj os_getwd( void )
{
  /* minimal implementation */
  return make_string( "." );
}
#endif /* HAVE_GETWD */
#endif /* HAVE_GETCWD */

/* change the current working directory,
   or signal an error on failure 
*/

void os_setwd( const char *path )
{
#if HAVE_CHDIR
  if (chdir(path) < 0)
    os_error( "chdir", 1, make_string(path) );
#else
  errno = ENOSYS;
  os_error( "chdir", 1, make_string(path) );
#endif
}

#if USE_POSIX_SIGNALS

/* new-style (POSIX) */

#define BEGIN_CRITICAL() do { sigset_t new_sig, old_sig; \
			     sigfillset( &new_sig ); \
			     sigemptyset( &old_sig ); \
			     sigprocmask( SIG_BLOCK, &new_sig, &old_sig )
#define END_CRITICAL() sigprocmask( SIG_SETMASK, &old_sig, NULL ); } while (0)

#else

/* old-style (used on NeXTSTEP 3.0, and in strict ANSI C) */

#define BEGIN_CRITICAL() do { int old_mask = sigsetmask( ~0 )
#define END_CRITICAL() sigsetmask( old_mask ); } while (0)

#endif /* USE_POSIX_SIGNALS */

/* 
   NOTE: the RS sig queue need only be long enough to hold signals
   being delivered during the execution of the remainder of the current
   monotone, or (longer) during the execution of some scheme code
   that has interrupts disabled
*/

void set_sigqueue_size( int n )
{
  size_t new_sz;
  void *new_q = NULL;

  /* silently make sure there's at least 2, and at least enough
   * to hold the current queue size 
   */
  if (n < 2)
    n = 2;

  if (num_sig > n)
    n = num_sig;

  new_sz = sizeof(struct RSSIG_info) * n;

  BEGIN_CRITICAL();
  if (n < 1000000)
    {
      if (sig_queue)
	new_q = realloc( sig_queue, new_sz );
      else
	new_q = malloc( new_sz );
    }
  if (new_q)
    sig_queue = (struct RSSIG_info *)new_q;
  END_CRITICAL();

  if (!new_q)
    scheme_error( "could not malloc ~d-element sigqueue", 1, int2fx(n) );

  max_sig = n;
}


struct RSSIG_info os_get_next_sig( void )
{
  struct RSSIG_info nxt;
  unsigned i;

  assert(num_sig);

  BEGIN_CRITICAL();

  nxt = sig_queue[0];
  /* later version can use a circular queue instead... */
  for (i=1; i<num_sig; i++)
    {
      sig_queue[i-1] = sig_queue[i];
    }
  num_sig--;
  rssig_enable = NO;
  rssig_ready = NO;
  END_CRITICAL();

  rsprof_timepoint( 400 + nxt.signum );

#if TRACE_SIGNAL_LATENCY
  {
    struct RSTime t_diff = rstime_sub( os_current_time(), nxt.queued_time );
    if ((t_diff.rstime[0] > 0) 
	|| (t_diff.rstime[1] > (RSSIG_LATENCY_TOLERANCE*1000)))
      {
	fprintf( stderr, "** RSSIG latency of " );
	fprintsi( stderr, &nxt );
	fprintf( stderr, " = %u.%03u ms\n",
		 (unsigned)((t_diff.rstime[0] * 1000) 
			     + t_diff.rstime[1] / 1000),
		 (unsigned)t_diff.rstime[1] % 1000 );
      }
  }
#endif
  return nxt;
}

UINT_32 os_halt_timer( void )
{
#ifdef TIMER_IS_MONOTONE_COUNTER
UINT_32 t = system_timeout;

    system_timeout = 0;
    return t;
#else
struct itimerval newt, old;
UINT_32 ms;

    newt.it_interval.tv_sec = 0;
    newt.it_interval.tv_usec = 0;
    newt.it_value.tv_sec = 0;
    newt.it_value.tv_usec = 0;
    setitimer( ITIMER_REAL, &newt, &old );

    BEGIN_CRITICAL();
    ms = (old.it_value.tv_sec * 1000) + (old.it_value.tv_usec / 1000);
    if (ms == 0)
      {
	unsigned i, j;

	/* make sure it's not been queued
	 * (note: because we've just turned off the timer, we
	 * don't have to worry about missing the insertion
	 * of the sig after we start looking, e.g., if the compiler
	 * caches num_sig)  (Also, we know that os_flush_sig_queue()
	 * won't be called at interrupt level)
	 */

	for (i=0; i<num_sig; i++)
	  if (sig_queue[i].signum == RSSIG_TIMEOUT)
	    {
	      num_sig--;
	      for (j=i; j<num_sig; j++)
		{
		  sig_queue[j] = sig_queue[j+1];
		}
	      break;
	    }
	/* if we removed the last signal from the queue, then mark
	 * them as `not ready'
	 */
	if (num_sig == 0)
	  rssig_ready = NO;
      }
    END_CRITICAL();
    return ms;
#endif
}

static void fprintsi( FILE *f, struct RSSIG_info *rssig )
{
  switch (rssig->signum)
    {
    case RSSIG_USER_INTR: 
      fprintf( f, "USER_INTR" );
      break;
    case RSSIG_TIMEOUT: 
      fprintf( f, "TIMEOUT" );
      break;
    case RSSIG_FINALIZE: 
      fprintf( f, "FINALIZE(%u)", (unsigned)rssig->data.finalize.count );
      break;
    case RSSIG_GC_FLIP: 
      fprintf( f, "GC_FLIP" );
      break;
    case RSSIG_C_SIGNAL: 
      fprintf( f, "C_SIGNAL(%d)", rssig->data.c_signal.signal_number );
      break;
    case RSSIG_CHILD_EXITED: 
      fprintf( f, "CHILD_EXITED(%d,%d)",
	       (int)rssig->data.child_exited.process_id,
	       rssig->data.child_exited.status );
      break;
    default:
      fprintf( f, "<UNKNOWN>" );
    }
}

void os_enqueue_sig_from_handler( struct RSSIG_info *rssig )
{
  if (sigq_notify_fd >= 0)
    {
      char notify_code = 'A' + rssig->signum;
      write( sigq_notify_fd, &notify_code, 1 );
      rsprof_timepoint( 300 + rssig->signum );
    }
  if (num_sig >= max_sig)
    {
      unsigned i;
      static int dumped_q = 0;

      rsprof_timepoint( 350 + rssig->signum );
      fprintf( stderr, "sig_queue overflow (%d queued) dropping: ", num_sig );
      fprintsi( stderr, rssig );
      fprintf( stderr, "\n" );
      if (!dumped_q)
	{
	  dumped_q = 1;
	  fprintf( stderr, "Queued:" );
	  
	  for (i=0; i<num_sig; i++)
	    {
	      fprintf( stderr, "\n  [%d] =>", i );
	      fprintsi( stderr, &sig_queue[i] );
	    }
	  fprintf( stderr, "\n" );
	}
    }
  else
    {
      sig_queue[num_sig] = *rssig;
#if TRACE_SIGNAL_LATENCY
      sig_queue[num_sig].queued_time = os_current_time();
#endif
      num_sig++;
      if (rssig_enable)
	rssig_ready = YES;
    }
}

void os_enqueue_sig( struct RSSIG_info *rssig )
{
  BEGIN_CRITICAL();
  os_enqueue_sig_from_handler( rssig );
  END_CRITICAL();
}


rs_bool os_set_sigenable( rs_bool flag )
{
  rs_bool old;

  rsprof_prof_mark( 3, (flag ? 1 : 0) + (rssig_enable ? 2 : 0), num_sig );

  BEGIN_CRITICAL();

  old = rssig_enable;

  rssig_enable = flag;
  if (flag)
    {
      if (num_sig)
	rssig_ready = YES;
    }
  else
    {
      rssig_ready = NO;
    }
  END_CRITICAL();
  return old;
}

#ifdef PLATFORM_IS_LITTLE_ENDIAN
#if !HAVE_NTOHS
UINT_16 ntohs(UINT_16 x)
{
  return ((x & 0xFF) << 8) | ((x & 0xFF00) >> 8);
}
#endif

#if !HAVE_NTOHL
UINT_32 ntohl(UINT_32 x)
{
  return ((x & 0x000000ffU) << 24)
    |    ((x & 0x0000ff00U) <<  8)
      |  ((x & 0x00ff0000U) >>  8)
	|((x & 0xff000000U) >> 24);
}
#endif
#endif /* PLATFORM_IS_LITTLE_ENDIAN */
