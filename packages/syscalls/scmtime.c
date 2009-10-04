/*------------------------------------------------------------------------*
 * $Id: scmtime.c,v 1.3 2005/05/16 22:13:22 donovan Exp $
 *
 * Purpose:	Low-level functions for scheme time manipulation
 *
 * Notes:
 *------------------------------------------------------------------------*
 * $Log: scmtime.c,v $
 * Revision 1.2  1995/03/13  03:18:07  donovan
 * Various fixes and changes
 *
 *------------------------------------------------------------------------*/

#include "scmtime.h"
#include <string.h>

obj make_time_sec( int secs, obj t_class )
{
obj ptr;
struct scheme_time *t;

    ptr = alloc( sizeof(struct scheme_time), t_class );
    t = PTR_TO_SCMTIME(ptr);
    t->sec = secs;
    t->usec = 0;
    return ptr;
}

/*
 *   this function handles carry from the us position
 *   so that the various math fns don't have to
 */

obj make_time( struct scheme_time *s, obj t_class )
{
obj ptr;
struct scheme_time *t;

    ptr = alloc( sizeof(struct scheme_time), t_class );
    t = PTR_TO_SCMTIME(ptr);
    *t = *s;
 again:
    if (t->usec >= 1000000)
    {
	t->usec -= 1000000;
	t->sec++;
	goto again;
    }
    else if (t->usec < 0)
    {
	t->usec += 1000000;
	t->sec--;
	goto again;
    }
    return ptr;
}

struct tm *calendar_time( struct scheme_time *t, rs_bool localq )
{
  if (localq)
    {
      return localtime( (time_t *)&t->sec );
    }
  else
    {
      return gmtime( (time_t *)&t->sec );
    }
}

void os_xtime( struct timeval *tv, obj item )
{
  struct scheme_time *t = PTR_TO_SCMTIME( item );

  tv->tv_sec = t->sec;
  tv->tv_usec = t->usec;
}

obj os_time( struct timeval *tv, obj t_class )
{
struct scheme_time t;

    t.sec = tv->tv_sec;
    t.usec = tv->tv_usec;
    return make_time( &t, t_class );
}

void current_time( struct scheme_time *t )
{
  struct timeval tv;

  if (gettimeofday( &tv, NULL ) != 0)
    scheme_error( "gettimeofday failed", 0 );
  t->sec = tv.tv_sec;
  t->usec = tv.tv_usec;
}
