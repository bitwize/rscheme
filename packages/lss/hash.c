#include <rscheme/pkgs/lss/lss.h>

static UINT_32 table[16] = {
  0x00000000U, 0x04c11db7U, 0x09823b6eU, 0x0d4326d9U, 0x130476dcU, 
  0x17c56b6bU, 0x1a864db2U, 0x1e475005U, 0x2608edb8U, 0x22c9f00fU, 
  0x2f8ad6d6U, 0x2b4bcb61U, 0x350c9b64U, 0x31cd86d3U, 0x3c8ea00aU, 
  0x384fbdbdU
};

#define crcBits (32)
#define nibsize (4)

#define shiftAmt (crcBits-nibsize)

#define crcHiBit (((UINT_32)1)<<(crcBits-1))
#define crcMask  (((crcHiBit-1)<<1)+1)

#define HASH_IN(nib) h = (table[ (h >> shiftAmt) ^ (nib) ] \
			   ^ (h << nibsize)) \
                          & crcMask

UINT_32 lssi_hash( UINT_32 num )
{
  UINT_32 h = 0;
  UINT_8 ch;

  /* this is 50 straight-line PPC instruction, 
     as compiled by `xlc -O2' */

  ch = num         & 0xF;     HASH_IN(ch);
  ch = (num >>  4) & 0xF;     HASH_IN(ch);
  ch = (num >>  8) & 0xF;     HASH_IN(ch);
  ch = (num >> 12) & 0xF;     HASH_IN(ch);
  ch = (num >> 16) & 0xF;     HASH_IN(ch);
  ch = (num >> 20) & 0xF;     HASH_IN(ch);
  ch = (num >> 24) & 0xF;     HASH_IN(ch);
  ch = (num >> 28) & 0xF;     HASH_IN(ch);
  return h;
}
