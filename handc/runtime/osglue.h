/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/osglue.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.20
 * File mod date:    2005-09-16 09:59:51
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          interface to standard OS/platform features
 *------------------------------------------------------------------------*/

/*
 *   Interface to the minimal functionality
 *   expected from the OS for normal
 *   operation
 */

#ifndef _H_RSCHEME_OSGLUE
#define _H_RSCHEME_OSGLUE

#include <stdio.h>
#include <stdarg.h>
#include <rscheme/obj.h>
#include <rscheme/intrs.h>

void init_os( void );

/*  allocate memory */

void *os_malloc( size_t size );         /* choke on error */
void *os_malloc_or_null( size_t size ); /* return NULL on error */

#define OS_MALLOC(type) ((type *)os_malloc( sizeof(type) ))

/*  open a file
    (path is in canonical form, so you can say
     os_fopen("foo/bar"), and on a Mac get fopen(":foo:bar"))
*/

FILE *os_fopen( const char *path, const char *mode );

const char *os_getenv( const char *key );

/* a minimal implementation can return "./",
   but see src/corelib/process.scm for caveats
*/

obj os_getwd( void );

/* change the current working directory,
   or signal an error on failure 
   (pathname in canonical form)
*/

void os_setwd( const char *path );

/*  return YES or NO accordingly as whether the named
    file exists 
    (pathname in canonical form)
*/

rs_bool os_file_exists_p( const char *path );

/*  create a directory 
    (part of base system so that compiler can be self-hosting)
*/

void os_mkdir( const char *path );

/* convert a canonical pathname to local OS format */

obj os_path( obj path );

/*
   arrange for some interrupts...
*/

/*
 *  if `msec' is 0, then signal an RSSIG_TIMEOUT immediately
 */

void os_set_timer( UINT_32 msec );
UINT_32 os_get_time_remaining( void );

/*
 *  os_halt_timer() must ensure that an RSSIG_TIMEOUT is not
 *  queued.  Usually, this only needs to be checked if
 *  it returns 0
 */

UINT_32 os_halt_timer( void );

/*
 * sleep for up to `msec' milliseconds
 * returns the amount of time actually slept
 * (may be larger msec, if the true real time
 *  is available and happens to be larger, or may
 *  be less if the sleep was interrupted, as by
 *  a signal)
 */

UINT_32 os_sleep( UINT_32 msec );
   
#if !HAVE_SETITIMER
#ifndef TIMER_IS_MONOTONE_COUNTER
#define TIMER_IS_MONOTONE_COUNTER
#endif
#endif

#ifdef TIMER_IS_MONOTONE_COUNTER
extern UINT_32 system_timeout;
#endif

/*
   return a list of symbols specifying the OS type
*/

obj os_type( void );

/*
 *  manage a bounded, ordered list of integers atomically,
 *  with a gate on rssig_ready
 */

/*
 *  this is YES if there is an RS signal waiting to be
 *  delivered via os_get_next_sig() 
 *  (in particular, it is NO if signals are disabled even
 *  if there is something in the internal queue)
 */

extern rs_bool rssig_ready;

void os_enqueue_sig( struct RSSIG_info *sig );

/* used to scan sig queue during root traversal */

int os_scan_sig_queue( unsigned *index, obj *item );

/* use the `_from_handler' form when interrupts are already disabled */
void os_enqueue_sig_from_handler( struct RSSIG_info *sig );

/* registers an interrupt handler that will run with all
 * interrupts disabled, and can make use of the `os_enqueue_sig_from_handler'
 * function.  Returns -1 if the signal is invalid
 */

int os_register_signal_handler( int sig, void (*proc)( int ) );

struct RSSIG_info os_get_next_sig( void ); /* disables rssig_ready */

rs_bool os_set_sigenable( rs_bool flag );
void set_sigqueue_size( int n );

/* signal an instance of <os-error>, capturing the current `errno' */

void os_error( const char *fn, int num_args, ... );
obj make_os_error( const char *fn, int num_args, ... );
obj vmake_os_error( const char *fn, int num_args, va_list va );

/* return a message describing the error */

obj os_errormsg( int error_num );

/* time manipulation functions */

struct RSTime os_current_time( void );
struct RSTime rstime_sub( struct RSTime a, struct RSTime b );
IEEE_64 rstime_to_sec( struct RSTime a );

/* profiling stats */

void os_prof_mark( void );

#endif /* _H_RSCHEME_OSGLUE */
