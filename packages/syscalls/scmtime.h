/*------------------------------------------------------------------------*
 * $Id: scmtime.h,v 1.5 2005/05/16 22:13:22 donovan Exp $
 *
 * Purpose:	Interface to low-level scheme time operations
 *
 * Notes:
 *------------------------------------------------------------------------*
 * $Log: scmtime.h,v $
 * Revision 1.2  1995/03/13  03:18:07  donovan
 * Various fixes and changes
 *
 *------------------------------------------------------------------------*/

#ifndef _H_SCMTIME
#define _H_SCMTIME

#include <time.h>
#include <sys/time.h>
#include <rscheme/scheme.h>
#include <rscheme/smemory.h>
#include <rscheme/osglue.h>

struct scheme_time {
    INT_32  sec;	/* seconds */
    INT_32  usec;	/* microseconds */
};

#define PTR_TO_SCMTIME(p) ((struct scheme_time *)PTR_TO_DATAPTR(p))

obj make_time( struct scheme_time *t, obj t_class );
obj make_time_sec( int t, obj t_class );

struct tm *calendar_time( struct scheme_time *t, rs_bool localq );
void current_time( struct scheme_time *t );

obj os_time( struct timeval *tv, obj t_class );
void os_xtime( struct timeval *tv, obj item );  /* extract OS time value */

#endif /* _H_SCMTIME */
