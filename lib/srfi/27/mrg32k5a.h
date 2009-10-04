#ifndef _H_MRG32K5A
#define _H_MRG32K5A

struct mrg32k5a_state {
  double s10, s11, s12, s13, s14;
  double s20, s21, s22, s23, s24;
};

void mrg32k5a_init( struct mrg32k5a_state *s, unsigned s1, unsigned s2 );
int mrg32k5a_init_str( struct mrg32k5a_state *s, const char *src, int len );

double mrg32k5a_get_float( struct mrg32k5a_state *s );
unsigned mrg32k5a_get_int( struct mrg32k5a_state *s, unsigned range );

int mrg32k5a_serialize( struct mrg32k5a_state *s, char *buf, int len );
int mrg32k5a_deserialize( struct mrg32k5a_state *s, const char *src );

#endif /* _H_MRG32K5A */

