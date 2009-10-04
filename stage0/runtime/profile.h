/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/profile.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.9
 * File mod date:    2003-10-13 13:02:34
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#ifndef _H_RSCHEME_PROFILE
#define _H_RSCHEME_PROFILE

/*  the profile analyzer tool doesn't want all the runtime system's
 *  types and prototypes...
 *  (in which case, they are responsible for providing appropriate
 *   definitions for the RS_pr_tag and RS_pr_tstamp types)
 */

#ifndef PROFILING_RECORD_STRUCTS_ONLY

/*******************************************************************/

#include <rscheme/obj.h>

#if HAVE_STRUCT_TIMEVAL
#include <sys/time.h>
#else
struct timeval { 
  long int tv_sec;
  long int tv_usec;
};
#endif

typedef obj RS_pr_tag;


#if HAVE_LONG_LONG
typedef unsigned long long RS_pr_tstamp;
#define RS_PROF_TSTAMP_IS_LONGLONG (1)
#else
typedef struct { UINT_32 t1, t2; } RS_pr_tstamp;
#define RS_PROF_TSTAMP_IS_LONGLONG (0)
#endif

RS_pr_tstamp rsprof_time( void );
obj rsprof_tstamp_to_obj( RS_pr_tstamp tstamp, obj t_class );

extern obj collect_gf_cache_histogram( rs_bool reset_q );

#define STMT(body) do { body } while (0)

/************************ Profiling Support ************************/

#if PROFILING_HOOKS

/* declare profiling hooks we are going to use */
extern int rsprof_active;

/* hooks to indicate how the current monotone is being exited... */
void rsprof_mt_calls( obj proc, obj tmpl );
void rsprof_mt_returns( void );
void rsprof_mt_bjumps( void );
void rsprof_mt_jumps( void );
void rsprof_mt_fails( void );
void rsprof_mt_intr( void );

/* hooks to keep track of the stack state */
void rsprof_saves( void );
void rsprof_contn_captured( obj contn );
void rsprof_contn_restored( obj contn );

/* hooks to time the execution of monotones */
void rsprof_mt_start( jump_addr entry_pt );
void rsprof_mt_done( void );
void rsprof_gc_work( void );

/* hooks to monitor object allocation and lifetimes */

void rsprof_obj_alloced( obj thing, obj a_class, UINT_32 size );
void rsprof_obj_died( obj thing );

/* other misc. stuff */

void rsprof_prof_mark( int code, unsigned data1, unsigned data2 );

void rsprof_timepoint_emit( int id );

#define rsprof_timepoint(id) \
    STMT( if(rsprof_active) rsprof_timepoint_emit(id); )

#if 0 /* ...still provided by timeprof.h and friends... */
#define timepoint(id) STMT( if(rsprof_active) rsprof_timepoint(id); )
#endif

#else

#define rsprof_active            (0)

#define rsprof_mt_calls(x,y)     STMT(;)
#define rsprof_mt_returns()      STMT(;)
#define rsprof_mt_bjumps()       STMT(;)
#define rsprof_mt_jumps()        STMT(;)
#define rsprof_mt_fails()        STMT(;)
#define rsprof_mt_intr()         STMT(;)

#define rsprof_saves()           STMT(;)
#define rsprof_contn_captured(c) STMT(;)
#define rsprof_contn_restored(c) STMT(;)

#define rsprof_mt_start(e)       STMT(;)
#define rsprof_mt_done()         STMT(;)
#define rsprof_gc_work()         STMT(;)

#define rsprof_obj_alloced(t,c,s) STMT(;)
#define rsprof_obj_died(t)       STMT(;)

#define rsprof_timepoint(id)     STMT(;)
#define rsprof_prof_mark(c,d1,d2) STMT(;)

#endif
#endif /* PROFILING_RECORD_STRUCTS_ONLY */

/* these are available even in profiling is not enabled to support
 * profile dump post-processing by normal code (using the profile-parse-next
 * glue function)
 */

enum RS_profile_code {
  RSPROF_DECL_NAME = 61,  /* '=' */
  RSPROF_NOP = 0x40,
  RSPROF_MT_CALLS,   /* A */
  RSPROF_MT_RETURNS,
  RSPROF_MT_BJUMPS,
  RSPROF_MT_JUMPS,
  RSPROF_MT_FAILS,
  RSPROF_MT_INTR,
  RSPROF_MT_START,
  RSPROF_MT_DONE,
  RSPROF_GC_WORK,    /* I */
  RSPROF_SAVES,
  RSPROF_CAPTURED,
  RSPROF_RESTORED,
  RSPROF_CAL_START,  /* M */
  RSPROF_CAL_STOP,   /* N */
  RSPROF_OBJ_ALLOCED,/* O */
  RSPROF_OBJ_DIED,   /* P */
  RSPROF_CAL_REALTIME,
  RSPROF_PROF_MARK,
  RSPROF_TIMEPOINT
};

struct RS_pr_header {
  enum RS_profile_code  code : 8;
  unsigned         var_len   : 8;
  unsigned         rec_bytes : 16;
};

struct RS_pr_DECL_NAME {
  struct RS_pr_header  hdr;
  RS_pr_tag          item;
  unsigned char      name[4];
};

struct RS_pr_NOP {
  struct RS_pr_header  hdr;
};

struct RS_pr_MT_CALLS {
  struct RS_pr_header  hdr;
  RS_pr_tag          tmpl;
  unsigned short     argc;
};

struct RS_pr_MT_RETURNS {
  struct RS_pr_header  hdr;
};

struct RS_pr_MT_BJUMPS {
  struct RS_pr_header  hdr;
};

struct RS_pr_MT_JUMPS {
  struct RS_pr_header  hdr;
};

struct RS_pr_MT_FAILS {
  struct RS_pr_header  hdr;
};

struct RS_pr_MT_INTR {
  struct RS_pr_header  hdr;
};

struct RS_pr_MT_START {
  struct RS_pr_header  hdr;
  RS_pr_tstamp       tstamp;
  RS_pr_tag          tmpl;
};

struct RS_pr_MT_DONE {
  struct RS_pr_header  hdr;
  RS_pr_tstamp       tstamp;
};

struct RS_pr_GC_WORK {
  struct RS_pr_header  hdr;
  RS_pr_tstamp       tstamp;
};

struct RS_pr_SAVES {
  struct RS_pr_header  hdr;
};

struct RS_pr_CAPTURED {
  struct RS_pr_header  hdr;
  RS_pr_tag          contn;
};

struct RS_pr_RESTORED {
  struct RS_pr_header  hdr;
  RS_pr_tag          contn;
};

struct RS_pr_CAL_START {
  struct RS_pr_header  hdr;
  RS_pr_tstamp       tstamp;
};

struct RS_pr_CAL_STOP {
  struct RS_pr_header  hdr;
  RS_pr_tstamp       tstamp;
};

struct RS_pr_CAL_REALTIME {
  struct RS_pr_header   hdr;
  RS_pr_tstamp          tstamp;
  struct timeval        systime;
  unsigned              recnum;
};

struct RS_pr_OBJ_ALLOCED {
  struct RS_pr_header  hdr;
  RS_pr_tag          item;
  RS_pr_tag          item_class;
  UINT_32            bytes;
};

struct RS_pr_OBJ_DIED {
  struct RS_pr_header  hdr;
  RS_pr_tag          item;
};

struct RS_pr_PROF_MARK {
  struct RS_pr_header   hdr;
  int                   code;
  unsigned              data1;
  unsigned              data2;
};

struct RS_pr_TIMEPOINT {
  struct RS_pr_header   hdr;
  RS_pr_tstamp          tstamp;
  int                   id;
};  

#endif /* _H_RSCHEME_PROFILE */
