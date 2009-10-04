#ifndef _H_ZSTORE
#define _H_ZSTORE

#include "lss.h"

/* worst case is writing 2048 ptrs, ea. requiring a new page
   ref, and none of that compressing at all (5 bytes/word),
   leading to: 2048 data * 5 + 2048 refs * 5 * 2 words/ref = 30720 bytes
*/

#define CACHE_LINES         (16)
#define ASSOCIATIVITY        (4)

struct Compressor {
  UINT_8	*cntl_ptr;
  UINT_8	*short_ptr;
  UINT_32       *long_ptr;

  UINT_32       victim_0, victim_1;
  UINT_32       cache[CACHE_LINES][ASSOCIATIVITY];
  UINT_8	cntl_b[8300];
  UINT_8	short_b[8300];
  UINT_32       long_b[2100];
};

/*** Compression ***/

void init_compressor( struct Compressor *c );

void compress_word( struct Compressor *c, UINT_32 word );
void compress_obj( struct Compressor *c, obj item );

/* returns the compressed size */

UINT_32 write_compressed( struct Compressor *c, LSS *lss, UINT_32 rec );

void close_compressor( struct Compressor *c );

/*** Decompression ***/

struct Decompressor {
  struct Compressor cstate;
};

void init_decompressor( struct Decompressor *d, LSS *lss, UINT_32 rec );

UINT_32 decompress_word( struct Decompressor *d );
obj decompress_obj( struct Decompressor *d );

void close_decompressor( struct Decompressor *d );

#endif /* _H_ZSTORE */
