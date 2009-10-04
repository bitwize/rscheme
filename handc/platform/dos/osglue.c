/*-----------------------------------------------------------------*-C-*---
 * File:    handc/platform/dos/osglue.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.3
 * File mod date:    1997-11-29 23:10:47
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Low-level OS glue for DOS
 *------------------------------------------------------------------------*/

#include "scheme.h"
#include "intrs.h"
#include "regs.h"

#include "runtime.h"
#include "osglue.h"

#include <stdlib.h>
#include <string.h>

#define MAX_SIG (10)

rs_bool rssig_ready = NO;

struct sig_queue {
  rs_bool *ready;
  rs_bool enable;
  unsigned num_sig;
  int sig_queue[MAX_SIG];
};

static void sig_add( struct sig_queue *q, int id );

static struct sig_queue the_sig_queue;

static void osglue_error( const char *fn, int rc )
{
    scheme_error( "OS glue error in ~s: rc ~d",
    		  2, make_string(fn), int2fx(rc) );
}


const char *os_getenv( const char *key )
{
    return NULL;
}

static char *make_dospath( char *temp, const char *path )
{
  while (*path)
    {
      if (*path == '/')
	*temp++ = '\\';
      else
	*temp++ = *path++;
    }
  return temp;
}

/* check to see if a file exists */

rs_bool os_file_exists_p( const char *path )
{
  FILE *f;
  char temp[1000];

  f = fopen( make_dospath( temp, path ), "r" );
	
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
  return make_string( make_dospath( temp, string_text(path) ) );
}


/* open a file using unix-std pathnames */

FILE *os_fopen( const char *path, const char *mode )
{
  char temp[1000];
  return fopen( make_dospath( temp, path ), mode );
}


UINT_32 os_sleep( UINT_32 msec )
{
  unsigned t = (msec + 500) / 1000;
  sleep( t );
  return t * 1000;
}

void init_os( void )
{
  the_sig_queue.num_sig = 0;
  the_sig_queue.enable = YES;
  the_sig_queue.ready = &rssig_ready;
}


static obj cis( const char *str, obj rest )
{
  return cons( intern( make_string(str) ), rest );
}

obj os_type( void )
{
  return cis( "dos", NIL_OBJ );
}

obj os_getwd( void )
{
  scheme_error( "os_getwd: not implemented", 0 );
}

void os_setwd( const char *path )
{
  scheme_error( "os_setwd: not implemented", 0 );
}



#define BEGIN_CRITICAL() do { /* what to do..? */
#define END_CRITICAL()  } while (0)

static obj sig_flush( struct sig_queue *q )
{
  int siginfo;
  obj lst = NIL_OBJ;

  if (q->num_sig)
    {
      unsigned n;

      BEGIN_CRITICAL();
      n = q->num_sig;
      if (n > MAX_SIG)
	{
	  lst = cons( lookup_symbol( "signal-queue-overflow" ), lst );
	  n = MAX_SIG;
	}
      while (n > 0)
	{
	  lst = cons( int2fx( q->sig_queue[--n] ), lst );
	}
      q->num_sig = 0;
      *(q->ready) = NO;
      END_CRITICAL();
    }
  return lst;
}


obj os_flush_sig_queue( void )
{
  return sig_flush( &the_sig_queue );
}


static void sig_add( struct sig_queue *q, int id )
{
  BEGIN_CRITICAL();
  if (q->num_sig >= MAX_SIG)
    {
      q->sig_queue[q->num_sig] = id;
    }
  else
    {
      fprintf( stderr, "\ninterrupts not being handled in a timely manner\n" );
      fprintf( stderr, "** aborting scheme process **\n" );
      abort();
    }
  q->num_sig++;
  if (q->enable)
    *q->ready = YES;
  END_CRITICAL();
}

void os_enqueue_sig( int id )
{
  sig_add( &the_sig_queue, id );
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

/* the NOT-real-time timer... */

UINT_32 system_timeout;

UINT_32 os_halt_timer( void )
{
  UINT_32 ms;

  ms = system_timeout / 128;
  system_timeout = 0;

  if (ms == 0)
      {
	unsigned i;
	rs_bool qd = NO;

	/* make sure it's been queued */

	for (i=0; i<the_sig_queue.num_sig; i++)
	  if (the_sig_queue.sig_queue[i] == RSSIG_TIMEOUT)
	    {
	      qd = YES;
	      break;
	    }
	if (!qd)
	  os_enqueue_sig( RSSIG_TIMEOUT );
      }
   return ms;
}

void os_set_timer( UINT_32 msec )
{
  system_timeout = msec * 128;
}

UINT_32 os_get_time_remaining( void )
{
  return system_timeout / 128;
}
