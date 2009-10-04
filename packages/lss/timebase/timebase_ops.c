#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include "timebase.h"
#include "config.h"

#if HAS_NATIVE_TIMEBASE

static int calibrated = 0;
static double usec_per_tb;

#define M  10   /* on 233MHz G3, ==> about 4ms */
#define M2 128  /* on 233MHz G3, ==> about 400us */

int cmp_dbl( const void *p1, const void *p2 )
{
  const double *a = p1;
  const double *b = p1;
  if (*a < *b)
    return -1;
  else if (*b < *a)
    return 1;
  else
    return 0;
}

static void futz_around( int m )
{
  int i, j;
  double temp[M2];

  for (j=0; j<m; j++)
    {
      for (i=0; i<M2; i++)
	temp[i] = ((double)rand()) / (double)((rand() % 1000) + 1);
      qsort( temp, M2, sizeof(double), cmp_dbl );
    }
}

static double calibration_sample( int m, long *span )
{
  struct timeval tv0, tv1;
  timebase_t t0, t1;
  long dusec;
  double samp;

  gettimeofday( &tv0, NULL );
  t0 = read_timebase();
  futz_around( m );
  gettimeofday( &tv1, NULL );
  t1 = read_timebase();

  dusec = (tv1.tv_usec - tv0.tv_usec) + 1000000 * (tv1.tv_sec - tv0.tv_sec);
  *span = dusec;
  samp = (double)dusec / (double)(t1 - t0);
  /*printf( "m = %d: sample = %ld usec / %Ld tb = %g\n", m, dusec, t1-t0, samp );*/
  return samp;
}

static void calibrate_timebase( void )
{
  double c[7];
  int i, m;
  long span;

  /* adjust the duration of the calibration to about 4ms */

  /* first, load everything into cache */

  c[0] = calibration_sample( M, &span );

  /* then do a sampling run */

  c[0] = calibration_sample( M, &span );

  /* linear extrapolation should work, because we are controlling
   * a loop over futzing operations
   */

  m = (M * 4000) / span;
  if (m > M*2)
    m = M*2;
  else if (m < 2)
    m = 2;

  for (i=0; i<7; i++)
    c[i] = calibration_sample( m, &span );

  qsort( c, 7, sizeof(double), cmp_dbl );

  /* average together the middle three estimates.
   * this gives an opportunity for two very high samples
   * and two very low samples
   */

  usec_per_tb = (c[2] + c[3] + c[4])/3.0;

  /*printf( "timebase conversion = %g usec/tbu\n", usec_per_tb );*/
  calibrated = 1;
}

long timebase_diff_usec( timebase_t t0, timebase_t t1 )
{
  if (!calibrated)
    calibrate_timebase();
  return (long)((t0 - t1) * usec_per_tb);
}

#else /* HAS_NATIVE_TIMEBASE */


timebase_t read_timebase( void )
{
  struct timeval tv;
  gettimeofday( &tv, NULL );
  return (((timebase_t)tv.tv_sec) << 32) + ((timebase_t)tv.tv_usec);
}

long timebase_diff_usec( timebase_t t0, timebase_t t1 )
{
  struct timeval tv0, tv1;
  long du;

  tv0.tv_sec = t0 >> 32;
  tv0.tv_usec = t0;

  tv1.tv_sec = t1 >> 32;
  tv1.tv_usec = t1;

  du = tv0.tv_usec - tv1.tv_usec;
  if (tv0.tv_sec != tv1.tv_sec)
    du += (tv0.tv_sec - tv1.tv_sec) * 1000000;

  return du;
}

#endif /* HAS_NATIVE_TIMEBASE */
