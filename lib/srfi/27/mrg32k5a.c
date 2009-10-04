#include <stdlib.h>
#include <stdio.h>
#include "mrg32k5a.h"

/*
 *  See <http://www.iro.umontreal.ca/~lecuyer/myftp/papers/combmrg2.ps>
 */

typedef struct mrg32k5a_state RandomState;

#define norm          2.3283163396834613e-10
#define m1   4294949027.0
#define m2   4294934327.0
#define a12     1154721.0
#define a14     1739991.0
#define a15n    1108499.0
#define a21     1776413.0
#define a23      865203.0
#define a25n    1641052.0

int mrg32k5a_deserialize( RandomState *s, const char *src )
{
  RandomState temp;

  if (sscanf( src, "(mrg32k5a (%lf %lf %lf %lf %lf) (%lf %lf %lf %lf %lf))",
              &temp.s10,
              &temp.s11,
              &temp.s12,
              &temp.s13,
              &temp.s14,
              &temp.s20,
              &temp.s21,
              &temp.s22,
              &temp.s23,
              &temp.s24 ) == 10) {
    *s = temp;
    return 0;
  } else {
    return -1;
  }
}


int mrg32k5a_serialize( RandomState *s, char *buf, int len )
{
  return snprintf( buf, len,
                   "(mrg32k5a (%f %.0f %.0f %.0f %.0f) (%.0f %.0f %.0f %.0f %.0f))",
                   s->s10, s->s11, s->s12, s->s13, s->s14,
                   s->s20, s->s21, s->s22, s->s23, s->s24 );
}

int mrg32k5a_deserialize( struct mrg32k5a_state *s, const char *src );

static double mrg32k5a_unnorm( RandomState *s )
{
  long k;
  double p1, p2;

  /*
  printf( "mrg32k5a { %.0f %.0f %.0f %.0f %.0f   %.0f %.0f %.0f %.0f %.0f }\n",
          s->s10, s->s11, s->s12, s->s13, s->s14,
          s->s20, s->s21, s->s22, s->s23, s->s24 );
  */

  /* Component 1 */

  p1 = a12 * s->s13 - a15n * s->s10;
  if (p1 > 0.0) {
    p1 -= a14 * m1;
  }
  
  p1 += a14 * s->s11;
  k = p1 / m1;
  p1 -= k * m1;
  if (p1 < 0.0) {
    p1 += m1;
  }
  s->s10 = s->s11;
  s->s11 = s->s12;
  s->s12 = s->s13;
  s->s13 = s->s14;
  s->s14 = p1;
  
  /* Component 2 */
  p2 = a21 * s->s24 - a25n * s->s20;
  if (p2 > 0.0) {
    p2 -= a23 * m2;
  }
  p2 += a23 * s->s22;
  k = p2 / m2;
  p2 -= k * m2;
  if (p2 < 0.0) {
    p2 += m2;
  }
  s->s20 = s->s21;
  s->s21 = s->s22;
  s->s22 = s->s23;
  s->s23 = s->s24;
  s->s24 = p2;
  
  /* Combine components */
  if (p1 <= p2) {
    return (p1 - p2 + m1);
  } else {
    return (p1 - p2);
  }
}

unsigned mrg32k5a_get_int( RandomState *s, unsigned range )
{
  unsigned long clipn;
  double ylimit, y;
  
  clipn = (unsigned long)(m1 / (double)range);
  ylimit = (double)clipn * range;
  do {
    y = mrg32k5a_unnorm( s );
  } while (y >= ylimit);
  return (unsigned)(y / (double)clipn);
}

double mrg32k5a_get_float( RandomState *s )
{
  return mrg32k5a_unnorm( s ) * norm;
}

static void pull_bits( const char *src, int len, int *p, 
                       double *dest, double max )
{
  unsigned long k = 0xFFFFFFFF;
  int i = 0;

  if (*p < len) {
    while ((*p < len) && ((i < 4) || (k >= max))) {
      k = (k << 8) + (src[*p] & 0xFF);
      (*p)++;
      i++;
    }
    if (k >= max) {
      k -= max;
    }
    *dest = k;
  }
}

int mrg32k5a_init_str( RandomState *s, const char *src, int len )
{
  int ptr = 0;

  mrg32k5a_init( s, 0x1234567, 0x7654321 );

  pull_bits( src, len, &ptr, &s->s10, m1 );
  pull_bits( src, len, &ptr, &s->s11, m1 );
  pull_bits( src, len, &ptr, &s->s12, m1 );
  pull_bits( src, len, &ptr, &s->s13, m1 );
  pull_bits( src, len, &ptr, &s->s14, m1 );

  pull_bits( src, len, &ptr, &s->s20, m2 );
  pull_bits( src, len, &ptr, &s->s21, m2 );
  pull_bits( src, len, &ptr, &s->s22, m2 );
  pull_bits( src, len, &ptr, &s->s23, m2 );
  pull_bits( src, len, &ptr, &s->s24, m2 );
  return ptr;
}

void mrg32k5a_init( RandomState *s, unsigned seed1, unsigned seed2 )
{
  s->s10 = seed1 >> 16;
  s->s11 = seed1 & 0xFFFF;
  s->s12 = s->s13 = s->s14 = 7;

  s->s20 = seed2 >> 16;
  s->s21 = seed2 & 0xFFFF;
  s->s22 = s->s23 = s->s24 = 11;
}

#ifdef UNIT_TEST

int main( int argc, const char **argv )
{
  RandomState s;
  unsigned n = atoi( argv[1] );
  int i, m = 20;

  if (argc > 2) {
    m = atoi( argv[2] );
  }

  s.s10 = 100;
  s.s11 = s.s12 = s.s13 = s.s14 = 3;

  s.s20 = 200;
  s.s21 = s.s22 = s.s23 = s.s24 = 3;

  for (i=0; i<m; i++) {
    printf( "%u", random_int( &s, n ) );
    printf( "   %.4f\n", random_float( &s ) );
  }
  return 0;
}

#endif
