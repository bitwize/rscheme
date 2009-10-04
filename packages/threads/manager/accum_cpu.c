#undef NDEBUG
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define USAGE_DIVS   (10)
#define USAGE_DELTA  (100*1000)  /* in usec */

/* the value of SCALE_FACTOR limits USAGE_DELTA 
 * (USAGE_DIVS * USAGE_DELTA * max[scale_table]) < MAX_INT) 
 */

#define SCALE_FACTOR (100)

static unsigned scale_table[USAGE_DIVS+1] = 
{
  /*  3, 5, 7, 11, 17, 27, 41, 63, 97, 149, 230 */
  3, 5, 7, 11, 17, 27, 41, 63, 97, 149, 230
};


/* the value of GAP_SEC is bounded by MAX_INT/1000000, ie, about 4000 sec */

#define GAP_SEC      ((2000000+(USAGE_DIVS*USAGE_DELTA))/1000000)

typedef int INT_32;
typedef unsigned UINT_32;

struct sys_time {
  INT_32 sec, usec;
};

struct thread_cpu_usage {
  struct sys_time t_base;
  struct sys_time total;
  UINT_32         history[ USAGE_DIVS ];
  UINT_32         accum;
};


void init_thread_cpu_usage( struct thread_cpu_usage *p )
{
  memset( p, 0, sizeof( struct thread_cpu_usage ) );
}

static inline int diff_us( struct sys_time t1, struct sys_time t0 )
{
  return (t0.sec - t1.sec) * 1000000 + (t0.usec - t1.usec);
}

static void accum_total( struct thread_cpu_usage *p, 
			 struct sys_time t0,
			 struct sys_time t1 )
{
  p->total.usec += (t1.usec - t0.usec);
  p->total.sec += (t1.sec - t0.sec);
  if (p->total.usec < 0)
    {
      /* wouldn't happen except for drift, so cancel it out... */
      p->total.usec = 0;
    }
  while (p->total.sec > 1000000)
    {
      p->total.sec++;
      p->total.usec -= 1000000;
    }
}

static void shift_window( struct thread_cpu_usage *p )
{
  unsigned i;

  for (i=1; i<USAGE_DIVS; i++)
    p->history[i-1] = p->history[i];

  /* p->accum could be larger than USAGE_DELTA if there are clock
   * skews that SHIFT the apparent time of an execution segment.
   * consider the following scenario:
   *
   *   current base time is 10, USAGE_DIV is 10
   *   thread runs from time 10 to time 15;  accum. 5
   *   clock skews by -2
   *   thread runs again (right away) from time 14 to time 20; accum. 6
   * 
   * thread total time is now 11 > USAGE_DIV
   * we could catch this by keeping track of the last executed time
   * and clamping to that, but doing the check here is probably cheaper 
   * and doesn't result in any larger error
   */
  if (p->accum > USAGE_DELTA)
    p->history[USAGE_DIVS-1] = USAGE_DELTA;
  else
    p->history[USAGE_DIVS-1] = p->accum;
  p->accum = 0;

  p->t_base.usec += USAGE_DELTA;
  if (p->t_base.usec >= 1000000)
    {
      p->t_base.usec -= 1000000;
      p->t_base.sec++;
      assert( p->t_base.usec < 1000000 );
    }
}

static void shift_usage( struct thread_cpu_usage *p, 
			 int fill_value,
			 struct sys_time new_base )
{
  unsigned i;

  p->t_base = new_base;
  for (i=0; i<USAGE_DIVS; i++)
    p->history[i] = fill_value;
  p->accum = 0;
}

void accum_cpu( struct thread_cpu_usage *p,
		struct sys_time t0,
		struct sys_time t1 )
{
  int t_start, t_end;

  if (t0.sec < p->t_base.sec)
    {
      t_start = 0;
    }
  else if (t0.sec >= p->t_base.sec + GAP_SEC)
    {
      shift_usage( p, 0, t0 );
      t_start = 0;
    }
  else
    {
      t_start = diff_us( p->t_base, t0 );
      if (t_start < 0)
	t_start = 0;
    }

  if (t1.sec < p->t_base.sec)
    {
      t_end = 0;
    }
  else if (t1.sec >= p->t_base.sec + GAP_SEC)
    {
      shift_usage( p, USAGE_DELTA, t1 );
      accum_total( p, t0, t1 );
      return;
    }
  else
    {
      t_end = diff_us( p->t_base, t1 );
      if (t_end < t_start)
	t_end = t_start;
    }

  assert( t_end >= t_start );

  /* shift the window back until the start is within the ACCUM */

  while (t_start >= USAGE_DELTA)
    {
      shift_window( p );
      t_start -= USAGE_DELTA;
      t_end -= USAGE_DELTA;
    }

  /* update the totals */

  assert( t_end <= t_start + USAGE_DELTA + 1000000 * GAP_SEC );

  p->total.usec += t_end - t_start;
  /*
  printf( "%d - %d, total = %d (gap = %d sec)\n",
	  t_start/1000, t_end/1000, 
	  t_end/1000 - t_start/1000, 
	  GAP_SEC );
  printf( "usec = %d\n", p->total.usec );
  */

  /* note that the number of iterations of this loop
   * is bounded at a small number by the GAP_SEC parameter
   */
  if( !(p->total.usec < (1000000 * GAP_SEC) ))
    {
      fprintf( stderr, "oops: %d >= %d\n",
	       p->total.usec, 1000000 * GAP_SEC );
      fprintf( stderr, "range %d - %d\n", t_start, t_end );
      assert(0);
    }
	       

  while (p->total.usec >= 1000000)
    {
      p->total.usec -= 1000000;
      p->total.sec++;
      /* printf( "decr, total usec => %d\n", p->total.usec ); */
    }

  /* shift the window for the end time, but this time accumulate time */
  while (t_end >= USAGE_DELTA)
    {
      assert( t_start < USAGE_DELTA );
      p->accum += USAGE_DELTA - t_start;
      /*printf( " += %d\n", USAGE_DELTA - t_start );*/
      shift_window( p );
      t_end -= USAGE_DELTA;
      t_start = 0;
    }
  assert( t_start <= t_end );
  /*printf( " += %d\n", t_end - t_start );*/
  p->accum += t_end - t_start;
}

/* computes the "load" in 1/1000 units */

int compute_usage_load( struct thread_cpu_usage *p, struct sys_time t_now )
{
  unsigned i, a = 0;
  int dt;

  for (i=0; i<USAGE_DIVS; i++)
    {
      a += p->history[i] * scale_table[i];
    }
  dt = diff_us( p->t_base, t_now );
  if (dt > SCALE_FACTOR)
    {
      unsigned r = (USAGE_DELTA * scale_table[USAGE_DIVS]) / dt;
      a += p->accum * r;
    }

  a /= USAGE_DIVS * 1000;
  if (a > 1000)
    return 1000;
  else
    return a;
}


#ifdef UNIT_TEST

void foo( void )
{
  usleep( 7000 );
}

int main( int argc, const char **argv )
{
  int i;
  struct thread_cpu_usage u;
  struct sys_time t0, t1;

  for (i=1; i<argc-1;)
    {
      t0.usec = (atoi(argv[i]) % 1000) * 1000;
      t0.sec = atoi(argv[i]) / 1000;
      i++;
      t1.usec = (atoi(argv[i]) % 1000) * 1000;
      t1.sec = atoi(argv[i]) / 1000;
      i++;

      accum_cpu( &u, t0, t1 );
    }
  printf( "t_base => %d.%03d\n", u.t_base.sec, u.t_base.usec / 1000 );
  printf( "total  => %d.%03d\n", u.total.sec, u.total.usec / 1000 );
  printf( "history => %03d", u.accum / 1000 );
  for (i=USAGE_DIVS; i>0;)
    printf( " %03d", u.history[--i] / 1000 );
  printf( "\n" );

  while (1)
    {
      int ld;
      gettimeofday( (struct timeval *)&t0, NULL );
      ld = compute_usage_load( &u, t0 );
      printf( "load = %3d.%d: ", ld/10, ld%10 );
      printf( " %04d ", u.accum );
      for (i=USAGE_DIVS; i>0;)
	printf( " %03d", u.history[--i]/1000 );
      printf( "  total %02d.%03d\n", u.total.sec, u.total.usec/1000 );
      gettimeofday( (struct timeval *)&t1, NULL );
      accum_cpu( &u, t0, t1 );
      foo();
    }
  return 0;
}
#endif
