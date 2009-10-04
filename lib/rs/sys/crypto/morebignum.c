#include <gmp.h>
#include "morebignum.h"

#define OBJ_TO_MPZ(mp, a) { (mp)[0]._mp_alloc = fx2int(gvec_ref((a), SLOT(0)));\
			    (mp)[0]._mp_size = fx2int(gvec_ref((a), SLOT(1)));\
			    (mp)[0]._mp_d = PTR_TO_DATAPTR(gvec_ref((a), SLOT(2)));}

#define MPZ_TO_OBJ(n) make3( bignum_class,                      \
                             int2fx( n[0]._mp_alloc ),          \
	             	     int2fx( n[0]._mp_size ),           \
	             	     DATAPTR_TO_PTR( n[0]._mp_d ) );

rs_bool rs_bignum_prob_primeq( obj n )
{
  mpz_t zn;

  OBJ_TO_MPZ( zn, n );

  if (mpz_probab_prime_p( zn, 11 )) {
    return YES;
  } else {
    return NO;
  }
}

obj rs_exp_modulo( obj base, obj exp, obj mod )
{
  mpz_t zrop, zbase, zexp, zmod;

  OBJ_TO_MPZ( zbase, base );
  OBJ_TO_MPZ( zexp, exp );
  OBJ_TO_MPZ( zmod, mod );
  mpz_init( zrop );

  mpz_powm( zrop, zbase, zexp, zmod );
  return MPZ_TO_OBJ( zrop );
}

obj rs_mod2exp( obj n, unsigned exp )
{
  mpz_t zn, zr;

  OBJ_TO_MPZ( zn, n );
  mpz_init( zr );

  mpz_fdiv_r_2exp( zr, zn, exp );
  return MPZ_TO_OBJ( zr );
}


obj rs_invertmod( obj n, obj mod )
{
  mpz_t zn, zmod, zr;

  OBJ_TO_MPZ( zn, n );
  OBJ_TO_MPZ( zmod, mod );
  mpz_init( zr );

  if (mpz_invert( zr, zn, zmod )) {
    return MPZ_TO_OBJ( zr );
  } else {
    return FALSE_OBJ;
  }
}


obj rs_export_bignum( obj n )
{
  mpz_t zn;
  size_t count;
  void *data;
  obj out;
  unsigned nbytes;

  OBJ_TO_MPZ( zn, n );

  nbytes = (mpz_sizeinbase( zn, 2 ) + 7) / 8;

  out = bvec_alloc( nbytes+1, string_class );
  mpz_export( PTR_TO_DATAPTR(out), &count, 1, 1, 1, 0, zn );
  return out;
}

obj rs_import_bignum( obj s )
{
  mpz_t zn;

  mpz_init( zn );
  mpz_import( zn, string_length(s), 1, 1, 1, 0, PTR_TO_DATAPTR(s) );
  
  return MPZ_TO_OBJ( zn );
}

