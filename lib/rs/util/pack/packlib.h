#ifndef _H_PACKLIB
#define _H_PACKLIB

#include <rscheme/scheme.h>

#define DECLARE_PACK_TYPE(t) \
   void bvec_packb_ ## t ( obj bvec, UINT_32 offset, obj value ); \
   obj bvec_unpackb_ ## t ( obj bvec, UINT_32 offset ); \
   void bvec_packl_ ## t ( obj bvec, UINT_32 offset, obj value ); \
   obj bvec_unpackl_ ## t ( obj bvec, UINT_32 offset ); \
   void bvec_packn_ ## t ( obj bvec, UINT_32 offset, obj value ); \
   obj bvec_unpackn_ ## t ( obj bvec, UINT_32 offset )

#define DECLARE_PACK_SIGN_PAIR(s,u) \
   DECLARE_PACK_TYPE(s); \
   DECLARE_PACK_TYPE(u)

DECLARE_PACK_SIGN_PAIR( u16, s16 );
DECLARE_PACK_SIGN_PAIR( u32, s32 );
DECLARE_PACK_SIGN_PAIR( u64, s64 );

DECLARE_PACK_TYPE( f32 );
DECLARE_PACK_TYPE( f64 );

/* special-case these because their `l' and `b' variants are nops */

void bvec_packn_u8( obj bvec, UINT_32 offset, obj value );
void bvec_packn_s8( obj bvec, UINT_32 offset, obj value );
obj bvec_unpackn_u8( obj bvec, UINT_32 offset );
obj bvec_unpackn_s8( obj bvec, UINT_32 offset );

#define bvec_packl_u8(b,o,v) bvec_packn_u8(b,o,v)
#define bvec_packl_s8(b,o,v) bvec_packn_s8(b,o,v)
#define bvec_packb_u8(b,o,v) bvec_packn_u8(b,o,v)
#define bvec_packb_s8(b,o,v) bvec_packn_s8(b,o,v)

#define bvec_unpackl_u8(b,o) bvec_unpackn_u8(b,o)
#define bvec_unpackl_s8(b,o) bvec_unpackn_s8(b,o)
#define bvec_unpackb_u8(b,o) bvec_unpackn_u8(b,o)
#define bvec_unpackb_s8(b,o) bvec_unpackn_s8(b,o)

#endif /* _H_PACKLIB */
