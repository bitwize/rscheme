/*-----------------------------------------------------------------*-C-*---
 * File:    handc/platform/mac/osglue.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.13
 * File mod date:    1997-11-29 23:10:47
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          OS glue for Macintosh (CodeWarrior)
 *------------------------------------------------------------------------*/

#include "scheme.h"
#include "intrs.h"
#include "regs.h"

#include "runtime.h"
#include "osglue.h"

#include <Timer.h>
#include <stdlib.h>
#include <string.h>
#include <unix.h>

#define MAX_SIG (10)

rs_bool rssig_ready = NO;

struct sig_queue {
  rs_bool *ready;
  rs_bool enable;
  unsigned num_sig;
  struct RSSIG_info sig_queue[MAX_SIG];
};

struct timer_info {
  struct TMTask hdr;
  struct sig_queue *q;
};

static struct sig_queue the_sig_queue;
static rs_bool timer_task_installed_p = NO;
static struct timer_info the_timer;

static void sig_add( struct sig_queue *q, struct RSSIG_info *sig );

static void osglue_error( const char *fn, int rc )
{
    scheme_error( "OS glue error in ~s: rc ~d",
    		  2, make_string(fn), int2fx(rc) );
}


const char *os_getenv( const char *key )
{
    return NULL;
}

static char *make_macpath( char *temp, const char *path )
{
  if (strncmp( path, "./", 2 ) == 0)
    path += 2;
	
  if (strchr( path, '/' ))
    {
      char *p = temp;
      
      if (path[0] == '/')
	{
	  path++;
	}
      else
	{
	  *p++ = ':';
	}
      while (*path)
	{
	  if (*path == '/')
	    *p++ = ':';
	  else
	    *p++ = *path;
	  path++;
	}
      *p++ = 0;
    }
  else
    {
      strcpy( temp, path );
    }
  return temp;
}

/* check to see if a file exists */

rs_bool os_file_exists_p( const char *path )
{
  FILE *f;
  char temp[1000];

  f = fopen( make_macpath( temp, path ), "r" );
  /* printf( "[stat '%s']", temp ); */
	
  if (f)
    {
      fclose(f);
      return YES;
    }
  else
    return NO;
}

/* convert a unix-standard pathname to local OS format */

obj os_path( obj path )
{
  char temp[1000];
  return make_string( make_macpath( temp, string_text(path) ) );
}


/* open a file using unix-std pathnames */

FILE *os_fopen( const char *path, const char *mode )
{
  char temp[1000];
  return fopen( make_macpath( temp, path ), mode );
}


/* the real-time timer... */

UINT_32 os_sleep( UINT_32 msec )
{
  unsigned t = (msec + 500) / 1000;
  sleep( t );
  return t * 1000;
}

static void exit_handler( void )
{
  os_halt_timer();
}

static void timeout_handler( void *self )
{
  struct RSSIG_info s;
  s.signum = RSSIG_TIMEOUT;
  sig_add( ((struct timer_info *)self)->q, &s );
}

static void init_timer_task( void )
{
  the_timer.hdr.qLink = NULL;
  the_timer.hdr.qType = 0;
  the_timer.hdr.tmCount = 0;
  the_timer.hdr.tmWakeUp = 0;
  the_timer.hdr.tmReserved = 0;
  the_timer.hdr.tmAddr = NewTimerProc(timeout_handler);
  the_timer.q = &the_sig_queue;
}

void init_os( void )
{
  the_sig_queue.num_sig = 0;
  the_sig_queue.enable = YES;
  the_sig_queue.ready = &rssig_ready;

  atexit( &exit_handler );
  init_timer_task();
}


static obj cis( const char *str, obj rest )
{
  return cons( intern( make_string(str) ), rest );
}

obj os_type( void )
{
  return cis( "mac", NIL_OBJ );
}

obj os_getwd( void )
{
  char *p, temp[1000];
  
  getcwd( temp+1, 1000 );

  /* convert to unixy format */

  p = temp;
  *p++ = '/';
  while (*p)
    {
      if (*p == ':')
	*p = '/';
      p++;
    }

  /* strip off trailing '/' -- we know its a directory */

  if (p[-1] == '/')
    p[-1] = 0;

  return make_string( temp );
}

/* change the current working directory,
   or signal an error on failure 
*/

void os_setwd( const char *path )
{
  char temp[1000];

  if (chdir( make_macpath(temp, path) ) < 0)
    scheme_error( "os_setwd(~s) failed", 1, make_string(path) );
}



#define BEGIN_CRITICAL() do { /* what to do..? */
#define END_CRITICAL()  } while (0)

struct RSSIG_info os_get_next_sig( void )
{
  struct RSSIG_info temp;
  struct sig_queue *q = &the_sig_queue;
  
  BEGIN_CRITICAL();
  if (q->num_sig)
  {
    temp = q->sig_queue[--q->num_sig];
    q->enable = NO;
    rssig_ready = NO;
  }
  else
  { 
    fprintf( stderr, " ** os_get_next_sig() underflow\n" );
  }
  END_CRITICAL();
  return temp;
}
  

static void sig_add( struct sig_queue *q, struct RSSIG_info *sig )
{
  BEGIN_CRITICAL();
  if (q->num_sig < MAX_SIG)
    {
      q->sig_queue[q->num_sig++] = *sig;
    }
  else
    {
      fprintf( stderr, "interrupts not being handled in a timely manner\n" );
    }
  if (q->enable)
    *q->ready = YES;
  END_CRITICAL();
}

void os_enqueue_sig_from_handler( struct RSSIG_info *sig )
{
  sig_add( &the_sig_queue, sig );
}

void os_enqueue_sig( struct RSSIG_info *sig )
{
  sig_add( &the_sig_queue, sig );
}


rs_bool os_set_sigenable( rs_bool flag )
{
  rs_bool old;

  BEGIN_CRITICAL();

  old = the_sig_queue.enable;

  the_sig_queue.enable = flag;
  if (flag)
    {
      if (the_sig_queue.num_sig)
	*the_sig_queue.ready = YES;
    }
  else
    {
      *the_sig_queue.ready = NO;
    }
  END_CRITICAL();
  return old;
}

UINT_32 os_halt_timer( void )
{
  UINT_32 ms;

  if (timer_task_installed_p)
    {
      /* remove the timer task */
      timer_task_installed_p = NO;
      RmvTime( (QElemPtr)&the_timer );
    }
  else
    ms = 0;

  /* if tmCount is negative,
   * then it's just in units of microseconds 
   */

  ms = (the_timer.hdr.tmCount < 0) ?
    -(the_timer.hdr.tmCount/1000) :
      the_timer.hdr.tmCount;
  
  if (ms == 0)
      {
	unsigned i;
	rs_bool qd = NO;

	/* make sure it's been queued 
	 * (note: because we've just turned off the timer, we
	 * don't have to worry about missing the insertion
	 * of the sig after we start looking, e.g., if the compiler
	 * caches num_sig)  (Also, we know that os_flush_sig_queue()
	 * won't be called at interrupt level)
	 */

	for (i=0; i<the_sig_queue.num_sig; i++)
	  if (the_sig_queue.sig_queue[i].signum == RSSIG_TIMEOUT)
	    {
	      qd = YES;
	      break;
	    }
	if (!qd)
	  timeout_handler( &the_timer );
      }
   return ms;
}

void os_set_timer( UINT_32 msec )
{
  if (timer_task_installed_p)
    {
      RmvTime( (QElemPtr)&the_timer );
    }
  if (msec == 0)
  {
    timeout_handler( &the_timer );
  }
  else
  {
    timer_task_installed_p = YES;
    InsTime((QElemPtr) &the_timer);

    PrimeTime((QElemPtr) &the_timer, msec);
  }
}

UINT_32 os_get_time_remaining( void )
{
  long int remaining_time;
  
  if (timer_task_installed_p)
    {
      UINT_32 ms = os_halt_timer();
      os_set_timer(ms);
      return ms;
    }
  else
    return 0;
}

int os_register_signal_handler( int sig, void (*proc)( int ) )
{
  fprintf( stderr, "** os_register_signal_handler() not implemented\n" );
}

/* returns true if something was found; updates the index
   to support incremental scanning (though there is an atomicity
   presumption, which says that when doing the scan-to-flip, nothing
   of interest will be inserted between the time this is first called
   with index->0 and when it returns false (0)
*/

int os_scan_sig_queue( unsigned *index, obj *item )
{
  unsigned i, n = the_sig_queue.num_sig;
  obj candidate;

  /* nothing will be dequeued during this scan, and
   * nothing of interest will be inserted, so we don't
   * need a critical section
   */
  for (i=*index; i<n; i++)
    {
      candidate = FALSE_OBJ;
      if (the_sig_queue.sig_queue[i].signum == RSSIG_FINALIZE)
	  candidate = the_sig_queue.sig_queue[i].data.finalize.finalize_list;
      
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

obj os_errormsg( int error_num )
{
  return make_string( strerror(error_num) );
}

void set_sigqueue_size( int n )
{
  /* ignore the request */
}

void os_mkdir( const char *path )
{
  scheme_error( "os_mkdir(): not implemented", 0 );
}

