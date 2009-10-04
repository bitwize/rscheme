/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/profile.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.11
 * File mod date:    2003-10-13 13:02:34
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          User-level profiling support
 *------------------------------------------------------------------------*/

#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>

#include <signal.h>
#include <rscheme/runtime.h>
#include <rscheme/vinsns.h>
#include <rscheme/hashmain.h>
#include <rscheme/osglue.h>
#include "../hasht/htstruct.h"

#if ACCUM_GF_CACHE_HISTOGRAM
int gf_cache_hit_depth[100];

obj collect_gf_cache_histogram( rs_bool reset_q )
{
  obj v = alloc( SLOT(100), vector_class );
  unsigned i;

  for (i=0; i<100; i++)
    gvec_write_init_non_ptr( v, SLOT(i), int2fx(gf_cache_hit_depth[i]) );

  if (reset_q)
    for (i=0; i<100; i++)
      gf_cache_hit_depth[i] = 0;

  return v;
}
#else
obj collect_gf_cache_histogram( rs_bool reset_q )
{
  return make0(vector_class);
}
#endif

#if !PROFILING_HOOKS

void rsprof_start( const char *path, rs_bool append_q, rs_bool start_now )
{
  scheme_error( "profiling not enabled", 0 );
}

rs_bool rsprof_stop( void )
{
  rsprof_start(NULL,NO,NO);
  return NO;
}

void rsprof_collect_objects( obj setup, obj otbl )
{
  rsprof_start(NULL,NO,NO);
}

void rsprof_app_defn_rec( obj key, obj val )
{
  rsprof_start(NULL,NO,NO);
}

#else

int rsprof_active = 0;
static int proffd = -1;
static unsigned recnum = 0;

/*
 *  In `deferred_write' mode, the profiling buffers are not
 *  actually written to disk until profiling stops or when
 *  a signal (SIGHUP) arrives.  Instead, we keep the buffers 
 *  in memory, using two buffers so that we get at least 1/2
 *  of the data from the past.
 */
static int deferred_write = 1;

struct MPB {
  char *buff;
  char *buff_ptr;
  char *buff_lim;
  int has_name_def;
};

struct MPB myprof0, myprof1;

#define MYPROF_BUFF_SIZE (1024*1024)

static void alloc_mpb( struct MPB *b )
{
  b->buff = malloc( MYPROF_BUFF_SIZE + 128 );
  b->buff_ptr = b->buff;
  b->buff_lim = b->buff + MYPROF_BUFF_SIZE;
  b->has_name_def = 0;
}

static void rsprof_cal_start( void );
static void rsprof_cal_stop( void );
static void rsprof_cal_realtime( void );
static void futz_around( int n );
static void bflush( int for_sure );
static void bflush1( struct MPB *b );
static int scan_profile_file( const char *path, 
			      int (*proc)( struct RS_pr_header *rec, 
					   void *info ),
			      void *info );
static void init_name_def( obj item );

/* control items */

static void do_flush_profiling( int x )	/* signal handler */
{
  if (rsprof_active) {
    deferred_write = 0;
    rsprof_prof_mark( 1, recnum, 0 );
    os_prof_mark();
    /* only flush myprof1, since it should definitely be
     * in a consistent state (we may have gotten the signal
     * in the middle of updating myprof0...
     */
    bflush1( &myprof1 );
  }
}


void rsprof_start( const char *path, rs_bool append_q, rs_bool start_now )
{
  if (proffd >= 0)
    close( proffd );
  rsprof_active = 0;

  proffd = open( path, 
                 (append_q ? O_APPEND : O_TRUNC) + O_WRONLY + O_CREAT, 
                 0666 );
  if (proffd < 0)
    os_error( "open", 1, make_string(path) );

  recnum = 0;
  alloc_mpb( &myprof0 );
  alloc_mpb( &myprof1 );

  if (truish( profiler_name_map ) && !append_q) {
    /*
     *  Make sure various things are populated so we don't
     *  recurse indefinitely trying to emit a name or two...
     */
    template_scope( FALSE_OBJ );
    init_name_def( vector_class );
    init_name_def( gvec_ref( profiler_name_map, HASHTABLE_BUCKET_CLASS ) );
  }

  if (!append_q)
    rsprof_active = 1;
  
  if (rsprof_active)
    {
      os_register_signal_handler( SIGHUP, do_flush_profiling );
      rsprof_cal_realtime();
      futz_around(10);
      rsprof_cal_realtime();
      futz_around(10);
      rsprof_cal_realtime();
      rsprof_cal_start();
      rsprof_cal_stop();

      if (start_now) {
        deferred_write = 0;
      } else {
        deferred_write = 1;
      }
    }
}

rs_bool rsprof_stop( void )
{
  if (proffd >= 0)
    {
      if (rsprof_active)
	{
	  rsprof_cal_start();
	  rsprof_cal_stop();
	}
      bflush(1);
      rsprof_active = 0;

      if (close( proffd ) < 0)
	os_error( "close", 0 );
      proffd = -1;
      return YES;
    }
  else
    return NO;
}


/*
 *  scans a file, looking for objects that it would be nice to have
 *  named in the output.  In particular, <<class>> and <template> objects.
 *
 *  Note that it is up to the user to guarantee that any classes and
 *  templates used during the tracing run are still around!
 *
 *  As an alternative, the RScheme global profile_name_map can be
 *  set to non-#f, in which case it is presumed to be a transient
 *  object table which causes DECL_NAME records to be emitted on-the-fly.
 *  
 */

static int collect_named_objects( struct RS_pr_header *rec, void *info )
{
  obj tmp_o, otbl;
  otbl = *(obj *)info;

  switch (rec->code)
    {
    case RSPROF_MT_RETURNS:
    case RSPROF_MT_BJUMPS:
    case RSPROF_MT_JUMPS:
    case RSPROF_MT_FAILS:
    case RSPROF_MT_INTR:
    case RSPROF_MT_DONE:
    case RSPROF_GC_WORK:
    case RSPROF_RESTORED:
    case RSPROF_CAPTURED:
    case RSPROF_OBJ_DIED:
    case RSPROF_SAVES:
    case RSPROF_DECL_NAME:
    case RSPROF_NOP:
    case RSPROF_CAL_START:
    case RSPROF_CAL_STOP:
    case RSPROF_CAL_REALTIME:
    case RSPROF_PROF_MARK:
    case RSPROF_TIMEPOINT:
      break;
      
    case RSPROF_MT_CALLS:
      tmp_o = ((struct RS_pr_MT_CALLS *)rec)->tmpl;
      objecttable_insert( otbl, obj_hash(tmp_o), tmp_o, tmp_o );
      break;
      
    case RSPROF_MT_START:
      tmp_o = ((struct RS_pr_MT_START *)rec)->tmpl;
      objecttable_insert( otbl, obj_hash(tmp_o), tmp_o, tmp_o );
      break;
      
    case RSPROF_OBJ_ALLOCED:
      tmp_o = ((struct RS_pr_OBJ_ALLOCED *)rec)->item_class;
      objecttable_insert( otbl, obj_hash(tmp_o), tmp_o, tmp_o );
      break;
    }
  return 0;
}

void rsprof_collect_objects( obj setup, obj otbl )
{
  if (STRING_P(setup))
    {
      scan_profile_file( string_text( setup ), 
			 collect_named_objects,
			 &otbl );
    }
  else
    scheme_error( "rsprof_collect_templates: "
		  "argument ~s not a pathname string", 1, setup );
}

#define SCAN_BUF_SIZE (8192)

static int scan_profile_file( const char *path, 
			      int (*proc)( struct RS_pr_header *rec, 
					   void *info ),
			      void *info )
{
  FILE *f;
  char *p, *lim, *pre_lim, temp[SCAN_BUF_SIZE];
  int n, rc;

  f = fopen( path, "r" );
  if (!f)
    os_error( "fopen", 1, make_string(path) );
  p = pre_lim = temp;

  n = fread( temp, 1, SCAN_BUF_SIZE, f );

  if (n < 0)
    n = 0;
  lim = p + n;
  pre_lim = lim - 300;
  if (pre_lim < p)
    pre_lim = p;

  rc = 0;

  while ((rc == 0) && (p < lim))
    {
      if (p >= pre_lim)
	{
	  memmove( temp, p, lim - p );
	  lim = temp + (lim - p);
	  p = temp;
	  n = fread( lim, 1, SCAN_BUF_SIZE - (lim - p), f );
	  if (n > 0)
	    {
	      lim += n;
	      pre_lim = lim - 300;
	      if (pre_lim < p)
		pre_lim = lim;
	    }
	  else
	    pre_lim = lim;
	}
      /*printf( "scanning a type-%d record (%u bytes)\n", 
	      ((struct RS_pr_header *)p)->code,
	      ((struct RS_pr_header *)p)->rec_bytes );*/
      rc = proc( (struct RS_pr_header *)p, info );
      p += ((struct RS_pr_header *)p)->rec_bytes;
    }
  
  fclose(f);
  return rc;
}

static void breset1( struct MPB *b )
{
  b->buff_ptr = b->buff;
  b->has_name_def = 0;
}

static void bflush1( struct MPB *b )
{
  size_t n = b->buff_ptr - b->buff;

  if (n) {
    if (n != write( proffd, b->buff, n )) {
      os_error( "write", 0 );
    }
    breset1( b );
  }
}

static void bflush( int forsure )
{
  if (deferred_write && !forsure) {
    struct MPB t;

    if (myprof1.has_name_def) {
      bflush1( &myprof1 );
    }

    t = myprof1;
    myprof1 = myprof0;
    myprof0 = t;
    breset1( &myprof0 );
  } else {
    bflush1( &myprof1 );
    bflush1( &myprof0 );
  }
  rsprof_cal_realtime();
}


#define EMIT_RECORD_HR(v,t,hr) struct RS_pr_ ## t *v; \
                               do { \
                                 if ((myprof0.buff_ptr+(hr))>=myprof0.buff_lim)\
                                    bflush(0); \
                                 v = (struct RS_pr_ ## t *) myprof0.buff_ptr; \
				 myprof0.buff_ptr+=sizeof(struct RS_pr_ ## t);\
				 v->hdr.code = RSPROF_ ## t; \
				 v->hdr.var_len = 0; \
				 v->hdr.rec_bytes=sizeof(struct RS_pr_ ## t);\
                                 recnum++; \
                               } while (0)
#define EMIT_RECORD(v,t) EMIT_RECORD_HR(v,t,0)

static void issue_name_def( obj item )
{
  int n, b;
  EMIT_RECORD_HR( r, DECL_NAME, 120 );

  n = snprinto( &r->name[0], item, 100 );
  b = ((n+3) & ~3);
  r->hdr.rec_bytes = sizeof( struct RS_pr_DECL_NAME ) - 4 + b;
  r->item = item;
  r->hdr.var_len = n;
  myprof0.has_name_def = 1;
  myprof0.buff_ptr = ((char *)r) + r->hdr.rec_bytes;
}

static void init_name_def( obj item )
{
  issue_name_def( item );
  objecttable_insert( profiler_name_map, 
                      obj_hash( item ), 
                      item, 
                      TRUE_OBJ );
}

static void emit_name_def( obj item )
{
  obj m = profiler_name_map;

  if (truish( m )) {
    obj h = obj_hash( item );
    if (!objecttable_probe( m, h, item )) {
      issue_name_def( item );
      objecttable_insert( m, h, item, TRUE_OBJ );
    }
  }
}

/* hooks to indicate how the current monotone is being exited... */

void rsprof_mt_calls( obj proc, obj tmpl )
{
  emit_name_def( tmpl );
  {
    EMIT_RECORD(r, MT_CALLS);
    r->tmpl = tmpl;
    r->argc = arg_count_reg;
  }
}

void rsprof_mt_returns( void )
{
  EMIT_RECORD(r, MT_RETURNS);
}

void rsprof_mt_bjumps( void )
{
  EMIT_RECORD(r, MT_BJUMPS);
}

void rsprof_mt_jumps( void )
{
  EMIT_RECORD(r, MT_JUMPS);
}

void rsprof_mt_fails( void )
{
  EMIT_RECORD(r, MT_FAILS);
}

/* hooks to keep track of the stack state */

void rsprof_saves( void )
{
  EMIT_RECORD(r, SAVES);
}

void rsprof_contn_captured( obj contn )
{
  EMIT_RECORD(r, CAPTURED);
  r->contn = contn;
}

void rsprof_contn_restored( obj contn )
{
  EMIT_RECORD(r, RESTORED);
  r->contn = contn;
}

void rsprof_prof_mark( int code, unsigned data1, unsigned data2 )
{
  if (rsprof_active) {
    EMIT_RECORD( r, PROF_MARK );
    r->code = code;
    r->data1 = data1;
    r->data2 = data2;
  }
}

void rsprof_mt_intr( void )
{
  EMIT_RECORD(r, MT_INTR);
}

/*
 *  Called just before a monotone is invoked
 */

void rsprof_mt_start( jump_addr entry_pt )
{
  emit_name_def( literals_reg );
  {
    /*
     *  Give us some extra headroom so we probably won't need to flush
     *  during this monotone's execution (which would get unfairly
     *  billed to the monotone)
     */
    EMIT_RECORD_HR(r, MT_START, 120);
    r->tstamp = rsprof_time();
    r->tmpl = literals_reg;
  }
}

#define tstamped(op) EMIT_RECORD(r,op); r->tstamp = rsprof_time()

static void rsprof_cal_start( void )
{
  tstamped( CAL_START );
}

static void rsprof_cal_stop( void )
{
  tstamped( CAL_STOP );
}

static void rsprof_cal_realtime( void )
{
  tstamped( CAL_REALTIME );
  gettimeofday( &r->systime, NULL );
  r->recnum = recnum;
}

/*
 *  Called when a monotone returns
 */

void rsprof_mt_done( void )
{
  tstamped( MT_DONE );
}

void rsprof_gc_work( void )
{
  tstamped( GC_WORK );
}

void rsprof_obj_alloced( obj item, obj obj_class, UINT_32 bytes )
{
  emit_name_def( obj_class );
  {
    EMIT_RECORD(r,OBJ_ALLOCED);
    r->item = item;
    r->item_class = obj_class;
    r->bytes = bytes;
  }
}

void rsprof_obj_died( obj item )
{
  EMIT_RECORD(r,OBJ_DIED);
  r->item = item;
}

void rsprof_timepoint_emit( int id )
{
  if (rsprof_active) {
    EMIT_RECORD( r, TIMEPOINT );
    r->tstamp = rsprof_time();
    r->id = id;
  }
}

void rsprof_app_defn_rec( obj key, obj val )
{
  int len, bytes;

  if (proffd < 0)
    return;

  len = string_length(val);
  if (len > 250)
    len = 250;
  bytes = (len + 3) & ~3;

  {
    EMIT_RECORD_HR(r,DECL_NAME,300);
    r->hdr.rec_bytes = sizeof( struct RS_pr_DECL_NAME ) - 4 + bytes;
    r->item = key;
    r->hdr.var_len = len;
    memcpy( r->name, string_text(val), bytes );
    myprof0.has_name_def = 1;
    myprof0.buff_ptr = ((char *)r) + r->hdr.rec_bytes;
  }
}

#define M  (256)

static int cmp_int( const void *pa, const void *pb )
{
  int a = *(const int *)pa;
  int b = *(const int *)pb;

  if (a < b)
    return -1;
  else if (a == b)
    return 0;
  else
    return 1;
}

static void futz_around( int n )
{
  int i, temp[M];

  while (n > 0)
    {
      for (i=0; i<M; i++)
	temp[i] = rand();
      qsort( temp, M, sizeof(int), cmp_int );
      n--;
    }
}

#if USE_TIMEOFDAY_CLOCK
RS_pr_tstamp rsprof_time( void )
{
  struct timeval tv;
  RS_pr_tstamp r;

  gettimeofday( &tv, NULL );
#if RS_PROF_TSTAMP_IS_LONGLONG
  r = (((RS_pr_tstamp)tv.tv_sec) << 32) + ((RS_pr_tstamp)tv.tv_usec);
#else
  r.t1 = tv.tv_sec;
  r.t2 = tv.tv_usec;
#endif
  return r;
}
#endif

#if USE_i586_CLOCK
/*
 *  see, e.g., <http://www.cs.wm.edu/~kearns/001lab.d/rdtsc.html>
 *  or search around for "pentium rdtsc"
 */
RS_pr_tstamp rsprof_time( void )
{
  unsigned long long int t;
  /* the rdtsc instruction */
   __asm__ volatile( ".byte 0x0f,0x31" : "=A"(t) );
   return t;
}
#endif

#endif

obj rsprof_tstamp_to_obj( RS_pr_tstamp tstamp, obj t_class )
{
#if USE_i586_CLOCK
  INT_64 t;

  t.digits[0] = tstamp >> 48;
  t.digits[1] = tstamp >> 32;
  t.digits[2] = tstamp >> 16;
  t.digits[3] = tstamp >> 0;
  return int_64_compact( t );
#else
  obj ptr;
  struct timeval *tp;

  ptr = alloc( sizeof(struct timeval), t_class );
  tp = (struct timeval *)PTR_TO_DATAPTR( ptr );
  tp->tv_sec = (tstamp >> 32);
  tp->tv_usec = (tstamp & 0xFFFFFFFF);
  return ptr;
#endif
}

