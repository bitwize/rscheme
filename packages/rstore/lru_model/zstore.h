#ifndef _H_ZSTORE
#define _H_ZSTORE

#include <rscheme/pkgs/lss/lss.h>

/* worst case is writing 2048 ptrs, ea. requiring a new page
   ref, and none of that compressing at all (5 bytes/word),
   leading to: 2048 data * 5 + 2048 refs * 5 * 2 words/ref = 30720 bytes
*/

#define CACHE_LINES         (16)
#define ASSOCIATIVITY        (4)

/* The max consumed by the an LRU model is determined as follows:
 *  1. Page size is 8192 bytes (= 2048 words)
 *  2. Worst case, a page will contain 2048 new page refs,
 *     for 1+(2*2048)=4097 words
 *  3. Total words to be encoded is max'd at 2048 + 4097 = 6145
 *  4. Each encoded word takes 1 CNTL
 *     and either 1 WORD or at most 2 SHORTs (in the NEW_16 case)
 *  5. Hence,
 *       MAX_CNTL = 6145
 *       MAX_SHORT = 2*6145 = 12290
 *       MAX_LONG = 6145 (ie, 24580 bytes)
 *     for a total output buffer of at least 43015 bytes
 */

#define LRUMODEL_MAX_WORDS  (6200)
#define LRUMODEL_MAX_CNTL   (6200)
#define LRUMODEL_MAX_SHORT  (12300)
#define LRUMODEL_BUFSIZE    (LRUMODEL_MAX_WORDS*sizeof(UINT_32) \
			      +(LRUMODEL_MAX_CNTL+LRUMODEL_MAX_SHORT)) \
                             /sizeof(UINT_32)

/*** Compression ***/

struct Compressor {
  UINT_8	*cntl_ptr;
  UINT_8	*short_ptr;
  UINT_32       *long_ptr;

  UINT_32       victim_0, victim_1;
  UINT_32       cache[CACHE_LINES][ASSOCIATIVITY];

  struct RStore  *store;   /* the store we are compressing for */

  /*  a word-aligned buffer -- 
   *   we should verify the max number of words by analyzing the
   *   algorithm.  It should be around 2x page size
   *
   *  the short_ptr & cntl_ptr point in the middle somewhere.
   *  we do a gather-write from this buffer, and read into
   *  it.
   */
  UINT_8	*cntl_base;
  UINT_8	*short_base;
  UINT_32       *long_base;

  UINT_8	*cntl_lim;  /* used for debugging, mostly */
  UINT_8	*short_lim;
  UINT_32       *long_lim;

  UINT_32       worda_b[LRUMODEL_BUFSIZE]; /* word aligned buffer */
};

void lru_init_compressor( struct Compressor *c );

/*** Decompression ***/

struct Decompressor {
  struct Compressor cstate;
};

#define SYM_LITERAL         (0)    /* 16 of them, 0-15 */
#define SYM_LITERALX4       (16)   /* 13 of them, 16, 20, ..., 64 */

#define SYM_VICTIM_0        (29)
#define SYM_VICTIM_1        (30)
#define SYM_NEAR_VICTIM_0   (31)
#define SYM_NEAR_VICTIM_1   (32)
#define SYM_NEW_32          (33)
#define SYM_NEW_16          (34)
#define SYM_NEW_4X2         (35)
#define SYM_NEW_8           (36)
#define SYM_NEAR            (37)
#define SYM_HIT             (SYM_NEAR+CACHE_LINES*ASSOCIATIVITY)

/* 256 - (SYM_NEAR + 2*(16 * 4)) ==> 91 spare symbols */

#define VICTIM_0_SHIFT(c,w)                    \
	  (c)->victim_0 = (c)->cache[h][3];    \
	  (c)->cache[h][3] = (c)->cache[h][2]; \
	  (c)->cache[h][2] = (c)->cache[h][1]; \
	  (c)->cache[h][1] = (c)->cache[h][0]; \
	  (c)->cache[h][0] = w;

#define VICTIM_1_SHIFT(c,w)                    \
	  (c)->victim_1 = (c)->victim_0;       \
	  VICTIM_0_SHIFT(c,w)


static inline int valid_8( UINT_32 w )
{
  INT_32 sw = w;

  return ((sw >= -128) && (sw <= 127));
}

static inline int valid_16( UINT_32 w )
{
  return (w <= 0xFFFF);
}

static inline UINT_8 encode_4x2( UINT_32 w )
{
  return ((w >> 12) & 0xF0) + ((w >> 3) & 0xF);
}

static inline UINT_32 decode_4x2( UINT_8 b )
{
  return ((b << 12) & 0xF0000) + ((b & 0xF) << 3) + 3;
}

static inline int valid_4x2( UINT_32 w )
{
  return (decode_4x2( encode_4x2( w ) ) == w);
}

#if 0 /* this mask/shift doesn't work with the hash() fn */
#define SIM_MASK (0xFFFFF807)
#define SIM_SHIFT (3)
#else
#define SIM_MASK (0xFFFFF00F)
#define SIM_SHIFT (4)
#endif

extern int lru_model_prefill[];

/**
 *   this hash function must be independent of any bits used
 *   in the approximation (similarity) function, and must
 *   map to 0 <= h < n
 */

static inline int hash( UINT_32 k )
{
  return ((k >> 16) + (k >> 12) + (k & 3)) & (CACHE_LINES-1);
}

#endif /* _H_ZSTORE */
