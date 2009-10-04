#include <rscheme/obj.h>

obj ber_parse_rec( unsigned char *src_base,
                   unsigned char **psrc_ptr,
                   unsigned char *src_limit,
                   obj raw_src,
                   obj specials );

unsigned ber_encode_fx( obj port, obj data );
unsigned ber_encode_str( obj port, obj data );
unsigned ber_encode_oid( obj port, obj vec, obj *hash );
unsigned ber_encode_header( obj port, int tag, unsigned len );

#define OBJ_TO_MPZ(mp, a) { (mp)[0]._mp_alloc = fx2int(gvec_ref((a), SLOT(0)));\
			    (mp)[0]._mp_size = fx2int(gvec_ref((a), SLOT(1)));\
			    (mp)[0]._mp_d = PTR_TO_DATAPTR(gvec_ref((a), SLOT(2)));}

#define MPZ_TO_OBJ(n) make3( bignum_class,                      \
                             int2fx( n[0]._mp_alloc ),          \
	             	     int2fx( n[0]._mp_size ),           \
	             	     DATAPTR_TO_PTR( n[0]._mp_d ) );

extern UINT_32 oid_hash_mix[256];

#define OID_HASH_MIX(seed,i) ((oid_hash_mix[(((seed) >> 24) ^ (i)) & 0xFF]\
                              ^ ((seed)<<8))+(i))

#define OID_HASH_TO_OBJ(h)    int2fx( (crc_hash_int( (h) ) & 0x1FFFFFFF) )
