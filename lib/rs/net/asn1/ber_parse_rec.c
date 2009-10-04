#include <gmp.h>
#include "ber.h"
#include <rscheme.h>

#define ASN_SET_CLASS(s)                (gvec_ref( s, SLOT(3) ))
#define ASN_PRINTABLE_STRING_CLASS(s)   (gvec_ref( s, SLOT(4) ))
#define ASN_UTCTIME_CLASS(s)            (gvec_ref( s, SLOT(5) ))
#define ASN_BITSTRING_CLASS(s)          (gvec_ref( s, SLOT(6) ))


static obj make_substring( obj orig,
                           unsigned char *ptr, 
                           unsigned char *limit )
{
  obj orig_str = gvec_ref( orig, SLOT(0) );
  unsigned char *orig_base = (unsigned char *)PTR_TO_DATAPTR( orig_str );
  unsigned orig_offset = fx2int( gvec_ref( orig, SLOT(1) ) );
  unsigned char *orig_ptr = orig_base + orig_offset;
 
  return make4( CLASSOF_PTR( orig ),
                orig_str,
                int2fx( ptr - orig_base ),
                int2fx( limit - ptr ),
                int2fx( limit - ptr ) );
}

static volatile void bail1( obj err_class,
                            int code,
                            obj raw_src,
                            unsigned char *ptr,
                            unsigned char *limit,
                            obj extra )
{
  raise_error( make4( err_class, 
                      NIL_OBJ, 
                      int2fx(code), 
                      make_substring( raw_src, ptr, limit ),
                      extra ) );
}

static volatile void bail0( obj err_class,
                            int code,
                            obj raw_src,
                            unsigned char *ptr,
                            unsigned char *limit )
{
  bail1( err_class, code, raw_src, ptr, limit, FALSE_OBJ );
}

static obj ber_parse_oid( unsigned char *ptr, 
                          unsigned len,
                          obj errc,
                          obj raw_src,
                          obj oid_class )
{
  obj q = make_dequeue();
  unsigned subid;
  unsigned char *lim = ptr + len;
  UINT_32 hash = 0x1234567;
  
  subid = ptr[0] / 40;
  dequeue_push_back( q, int2fx( subid ) );
  hash = OID_HASH_MIX( hash, subid );

  subid = ptr[0] % 40;
  dequeue_push_back( q, int2fx( subid ) );
  hash = OID_HASH_MIX( hash, subid );

  ptr++;
  len--;

  subid = 0;
  while (ptr < lim) {
    
    if (*ptr & 0x80) {
      subid = (subid | (*ptr & 0x7F)) << 7;
      if (subid == 0) {
        /* must have seen a first subid octet of 0x80 */
        bail0( errc, 7362, raw_src, ptr, ptr+len );
      }
    } else {
      subid |= *ptr & 0x7F;
      dequeue_push_back( q, uint_32_compact( subid  ) );
      hash = OID_HASH_MIX( hash, subid );
      subid = 0;
    }
    ptr++;
  }
  if (subid) {
    /* never saw the final subid octet */
    bail0( errc, 7361, raw_src, ptr, ptr+len );
  }
  return make3( oid_class,
                OID_HASH_TO_OBJ( hash ),
                dequeue_state( q ),
                FALSE_OBJ );
}


static obj ber_parse_int( unsigned char *ptr, 
                          unsigned len )
{
  unsigned tmp;

  switch (len) {

  case 0:
    return ZERO;

  case 1:
    tmp = ptr[0];

    if (tmp & 0x80) {
      tmp |= (-1 & ~0xFF);
    }
    return int2fx( tmp );
    
  case 2:
    tmp = (ptr[0] << 8) | ptr[1];
    if (tmp & 0x8000) {
      tmp |= (-1 ^ 0xFFFF);
    }
    return int2fx( tmp );
    
  case 3:
    tmp = (ptr[0] << 16) | (ptr[1] << 8) | ptr[2];
    if (tmp & 0x800000) {
      tmp |= (-1 ^ 0xFFFFFF);
    }
    return int2fx( tmp );
    
  case 4:
    tmp = (ptr[0] << 24) | (ptr[1] << 16) | (ptr[2] << 8) | ptr[3];
    if (tmp & 0x80000000) {
      return basic_minus( ZERO, uint_32_compact( -tmp ) );
    } else {
      return uint_32_compact( tmp );
    }

  default:
    {
      mpz_t zn;
      mpz_init( zn );
      mpz_import( zn, len, 1, 1, 1, 0, ptr );

      if (ptr[0] & 0x80) {
        mpz_t flip;
        mpz_init_set_ui( flip, 0 );
        mpz_setbit( flip, len*8 );
        mpz_sub( zn, zn, flip );
      }
      return MPZ_TO_OBJ( zn );
    }
  }
}

obj ber_parse_rec( unsigned char *src_base,
                   unsigned char **psrc_ptr,
                   unsigned char *src_limit,
                   obj raw_src,
                   obj specials )
{
  unsigned char *src_ptr = *psrc_ptr;
  unsigned char tag;
  unsigned long len;
  obj errc = gvec_ref( specials, SLOT(0) );
  obj unkc = gvec_ref( specials, SLOT(1) );

  /*  read the TAG octet  */
  
  if (src_ptr >= src_limit) {
    bail0( errc, 7301, raw_src, src_ptr, src_limit );
  }
  tag = *src_ptr++;

  /*  read the first octet of the LENGTH, which could
   *  encode either the entire length or just the
   *  number of octets *in* the length
   */
 
  if (src_ptr >= src_limit) {
    bail0( errc, 7302, raw_src, src_ptr, src_limit );
  }
  len = *src_ptr++;

  /*  read remaining octets, if any, of the LENGTH  */

  if (len & 0x80) {
    switch (len) {

      case 0x81:
           if (src_ptr+1 > src_limit) {
             bail0( errc, 7303, raw_src, src_ptr, src_limit );
           }
           len = src_ptr[0];
           src_ptr += 1;
           break;


      case 0x82:
           if (src_ptr+2 > src_limit) {
             bail0( errc, 7304, raw_src, src_ptr, src_limit );
           }
           len = (src_ptr[0] << 8) 
               + src_ptr[1];
           src_ptr += 2;
           break;

      case 0x83:
           if (src_ptr+3 > src_limit) {
             bail0( errc, 7305, raw_src, src_ptr, src_limit );
           }
           len = (src_ptr[0] << 16)
               + (src_ptr[1] << 8)
               + (src_ptr[2]);
           src_ptr += 3;
           break;

      case 0x84:
           if (src_ptr+4 > src_limit) {
             bail0( errc, 7306, raw_src, src_ptr, src_limit );
           }
           len = (src_ptr[0] << 24)
               + (src_ptr[1] << 16)
               + (src_ptr[2] << 8)
               + (src_ptr[3]);
           src_ptr += 4;
           break;

      case 0x80:
           /* this should not happen */
           bail0( errc, 7307, raw_src, src_ptr, src_limit );

      default:
           /* how many octets is the length going to be!? */
           /* theoretically, we should handle this case, since they are
              allowed to use more octets of encoding than are required */
           bail0( errc, 7308, raw_src, src_ptr, src_limit );
      }
   }

  if (src_ptr + len > src_limit) {
    bail1( errc, 7309, raw_src, src_ptr, src_limit, int2fx(len) );
  }

  switch (tag) {
  case 0x02:   /* INTEGER */
    *psrc_ptr = src_ptr + len;
    return ber_parse_int( src_ptr, len );

  case 0x03:    /* BIT STRING */
    *psrc_ptr = src_ptr + len;
    return make2( ASN_BITSTRING_CLASS( specials ),
                  int2fx( src_ptr[0] ),
                  make_substring( raw_src, src_ptr+1, src_ptr + len ) );

  case 0x04:   /* OCTET STRING */
    *psrc_ptr = src_ptr + len;
    return make_substring( raw_src, src_ptr, src_ptr + len );

  case 0x13:   /* PRINTABLE STRING */
    *psrc_ptr = src_ptr + len;
    return make1( ASN_PRINTABLE_STRING_CLASS( specials ),
                  make_substring( raw_src, src_ptr, src_ptr + len ) );

  case 0x17:   /* UTCTIME */
    *psrc_ptr = src_ptr + len;
    return make1( ASN_UTCTIME_CLASS( specials ),
                  make_substring( raw_src, src_ptr, src_ptr + len ) );
    
  case 0x05:   /* NULL */
    *psrc_ptr = src_ptr + len;
    return FALSE_OBJ;


  case 0x06:   /* OBJECT IDENTIFIER */
    *psrc_ptr = src_ptr + len;
    return ber_parse_oid( src_ptr, len, errc, raw_src, 
                          gvec_ref( specials, SLOT(2) ) );

  case 0x31:   /* SET */
  case 0x30:   /* SEQUENCE */
    {
      obj q = make_dequeue();
      unsigned char *p, *lim;

      p = src_ptr;
      lim = src_ptr + len;
      while (p < lim) {
        obj item = ber_parse_rec( src_base, &p, lim, raw_src, specials );
        dequeue_push_back( q, item );
      }
      assert( p == lim );
      *psrc_ptr = p;

      if (tag == 0x31) {
        return make1( ASN_SET_CLASS(specials), dequeue_state( q ) );
      }
      return dequeue_state( q );
    }

  default:
    *psrc_ptr = src_ptr + len;
    return make2( unkc,
                  int2fx( tag ),
                  make_substring( raw_src, src_ptr, src_ptr + len ) );
  }
  return FALSE_OBJ;
}

UINT_32 oid_hash_mix[256] = {
  0x00000000U, 0x04c11db7U, 0x09823b6eU, 0x0d4326d9U, 0x130476dcU, 0x17c56b6bU,
  0x1a864db2U, 0x1e475005U, 0x2608edb8U, 0x22c9f00fU, 0x2f8ad6d6U, 0x2b4bcb61U,
  0x350c9b64U, 0x31cd86d3U, 0x3c8ea00aU, 0x384fbdbdU, 0x4c11db70U, 0x48d0c6c7U,
  0x4593e01eU, 0x4152fda9U, 0x5f15adacU, 0x5bd4b01bU, 0x569796c2U, 0x52568b75U,
  0x6a1936c8U, 0x6ed82b7fU, 0x639b0da6U, 0x675a1011U, 0x791d4014U, 0x7ddc5da3U,
  0x709f7b7aU, 0x745e66cdU, 0x9823b6e0U, 0x9ce2ab57U, 0x91a18d8eU, 0x95609039U,
  0x8b27c03cU, 0x8fe6dd8bU, 0x82a5fb52U, 0x8664e6e5U, 0xbe2b5b58U, 0xbaea46efU,
  0xb7a96036U, 0xb3687d81U, 0xad2f2d84U, 0xa9ee3033U, 0xa4ad16eaU, 0xa06c0b5dU,
  0xd4326d90U, 0xd0f37027U, 0xddb056feU, 0xd9714b49U, 0xc7361b4cU, 0xc3f706fbU,
  0xceb42022U, 0xca753d95U, 0xf23a8028U, 0xf6fb9d9fU, 0xfbb8bb46U, 0xff79a6f1U,
  0xe13ef6f4U, 0xe5ffeb43U, 0xe8bccd9aU, 0xec7dd02dU, 0x34867077U, 0x30476dc0U,
  0x3d044b19U, 0x39c556aeU, 0x278206abU, 0x23431b1cU, 0x2e003dc5U, 0x2ac12072U,
  0x128e9dcfU, 0x164f8078U, 0x1b0ca6a1U, 0x1fcdbb16U, 0x018aeb13U, 0x054bf6a4U,
  0x0808d07dU, 0x0cc9cdcaU, 0x7897ab07U, 0x7c56b6b0U, 0x71159069U, 0x75d48ddeU,
  0x6b93dddbU, 0x6f52c06cU, 0x6211e6b5U, 0x66d0fb02U, 0x5e9f46bfU, 0x5a5e5b08U,
  0x571d7dd1U, 0x53dc6066U, 0x4d9b3063U, 0x495a2dd4U, 0x44190b0dU, 0x40d816baU,
  0xaca5c697U, 0xa864db20U, 0xa527fdf9U, 0xa1e6e04eU, 0xbfa1b04bU, 0xbb60adfcU,
  0xb6238b25U, 0xb2e29692U, 0x8aad2b2fU, 0x8e6c3698U, 0x832f1041U, 0x87ee0df6U,
  0x99a95df3U, 0x9d684044U, 0x902b669dU, 0x94ea7b2aU, 0xe0b41de7U, 0xe4750050U,
  0xe9362689U, 0xedf73b3eU, 0xf3b06b3bU, 0xf771768cU, 0xfa325055U, 0xfef34de2U,
  0xc6bcf05fU, 0xc27dede8U, 0xcf3ecb31U, 0xcbffd686U, 0xd5b88683U, 0xd1799b34U,
  0xdc3abdedU, 0xd8fba05aU, 0x690ce0eeU, 0x6dcdfd59U, 0x608edb80U, 0x644fc637U,
  0x7a089632U, 0x7ec98b85U, 0x738aad5cU, 0x774bb0ebU, 0x4f040d56U, 0x4bc510e1U,
  0x46863638U, 0x42472b8fU, 0x5c007b8aU, 0x58c1663dU, 0x558240e4U, 0x51435d53U,
  0x251d3b9eU, 0x21dc2629U, 0x2c9f00f0U, 0x285e1d47U, 0x36194d42U, 0x32d850f5U,
  0x3f9b762cU, 0x3b5a6b9bU, 0x0315d626U, 0x07d4cb91U, 0x0a97ed48U, 0x0e56f0ffU,
  0x1011a0faU, 0x14d0bd4dU, 0x19939b94U, 0x1d528623U, 0xf12f560eU, 0xf5ee4bb9U,
  0xf8ad6d60U, 0xfc6c70d7U, 0xe22b20d2U, 0xe6ea3d65U, 0xeba91bbcU, 0xef68060bU,
  0xd727bbb6U, 0xd3e6a601U, 0xdea580d8U, 0xda649d6fU, 0xc423cd6aU, 0xc0e2d0ddU,
  0xcda1f604U, 0xc960ebb3U, 0xbd3e8d7eU, 0xb9ff90c9U, 0xb4bcb610U, 0xb07daba7U,
  0xae3afba2U, 0xaafbe615U, 0xa7b8c0ccU, 0xa379dd7bU, 0x9b3660c6U, 0x9ff77d71U,
  0x92b45ba8U, 0x9675461fU, 0x8832161aU, 0x8cf30badU, 0x81b02d74U, 0x857130c3U,
  0x5d8a9099U, 0x594b8d2eU, 0x5408abf7U, 0x50c9b640U, 0x4e8ee645U, 0x4a4ffbf2U,
  0x470cdd2bU, 0x43cdc09cU, 0x7b827d21U, 0x7f436096U, 0x7200464fU, 0x76c15bf8U,
  0x68860bfdU, 0x6c47164aU, 0x61043093U, 0x65c52d24U, 0x119b4be9U, 0x155a565eU,
  0x18197087U, 0x1cd86d30U, 0x029f3d35U, 0x065e2082U, 0x0b1d065bU, 0x0fdc1becU,
  0x3793a651U, 0x3352bbe6U, 0x3e119d3fU, 0x3ad08088U, 0x2497d08dU, 0x2056cd3aU,
  0x2d15ebe3U, 0x29d4f654U, 0xc5a92679U, 0xc1683bceU, 0xcc2b1d17U, 0xc8ea00a0U,
  0xd6ad50a5U, 0xd26c4d12U, 0xdf2f6bcbU, 0xdbee767cU, 0xe3a1cbc1U, 0xe760d676U,
  0xea23f0afU, 0xeee2ed18U, 0xf0a5bd1dU, 0xf464a0aaU, 0xf9278673U, 0xfde69bc4U,
  0x89b8fd09U, 0x8d79e0beU, 0x803ac667U, 0x84fbdbd0U, 0x9abc8bd5U, 0x9e7d9662U,
  0x933eb0bbU, 0x97ffad0cU, 0xafb010b1U, 0xab710d06U, 0xa6322bdfU, 0xa2f33668U,
  0xbcb4666dU, 0xb8757bdaU, 0xb5365d03U, 0xb1f740b4U
};
