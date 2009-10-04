#ifndef _H_LSSV3
#define _H_LSSV3

#include "freemap.h"

#define ALLOC(t) ALLOCN(t,1)
#define ALLOCN(t,n) ((t *)malloc( sizeof(t) * (n) ))
#define REALLOCN(v,t,n) ((v) = (t *)realloc( (v), sizeof(t) * (n) ))

struct LSSRecordHeader {
  UINT_32   magic;
  UINT_32   recnum;         /* typ. use depends on record type */
  UINT_32   space_word;     /* size in granules + compression alg. # */
  UINT_32   length;         /* uncompressed size in bytes */
};

/*  A `space' word contains low four bits which specify the
 *  byte-level compression algorithm.  This compression is
 *  transparent to the client (apart from specifying which
 *  algorithm to use).
 *
 *  Up to 16 different algorithms may be in use in a given
 *  volume; algorithm `0' always refers to the "null"
 *  compressor, which is always available.
 *
 *  Note that we could store a reference count of the number
 *  of records using a particular algorithm in the commit
 *  record, if it became necessary to reclaim them.  However,
 *  that doesn't seem necessary yet...
 */

#define MAKE_SPACE_WORD(space_bytes,which_zip) ((space_bytes)+(which_zip))
#define GET_WHICH_ZIP(space_word)              ((int)((space_word) & 0xF))
#define GET_SPACE_BYTES(space_word)            ((space_word) & ~0xF)
#define MAX_ZIP_ALGORITHMS                     (16) /* includes 0=null */

struct LSSSegmentTrailer {
  UINT_32   magic;
  UINT_32   length;         /* including trailer, ie a multiple of IO_SIZE */
  UINT_32   trailer_space_w;/* 16 */
  UINT_32   trailer_len;    /* 0 */
};

#define HEADER_SIZE  (sizeof( struct LSSRecordHeader ))
#define TRAILER_SIZE  (sizeof( struct LSSSegmentTrailer ))

#if WORD_SIZE_BITS == 64
#define IO_BLOCK_SIZE   (2048)
#else
#define IO_BLOCK_SIZE   (1024)
#endif

#define DATASEG_MAGIC (0x44534547)  /* 'DSEG' */
#define DATAREC_MAGIC (0x44415441)  /* 'DATA' */
#define GAP_MAGIC     (0x47415020)  /* 'GAP ' */
#define VOLFN_MAGIC   (0x564f4c46)  /* 'VOLF' */
#define ZIPA_MAGIC    (0x5a495041)  /* 'ZIPA' */
#define COMMIT_MAGIC  (0x434f4d4d)  /* 'COMM' */
#define EOF_MAGIC     (0x2a454f46)  /* '*EOF' */
#define INDEX_MAGIC   (0x494e4458)  /* 'INDX' */
#define MINDEX_MAGIC  (0x4d494458)  /* 'MIDX' */

#define LSSX_VERSION  (3)           /* 0.7.1 was 2, 
				       unreleased lssx was also 3 */

#define MAX_OUT_BUFS   (6)   /* <= (MAX_IOVEC - 1) */
#define MAX_IN_BUFS   (10)   /* used to support multiple active reads */

#define MIN_OBUF_SIZE (64*1024)
/*#define MIN_OBUF_SIZE (2*1024) */
#define FIRST_OBUF_SIZE (MIN_OBUF_SIZE - 16) /* 16 makes space for DSEG */

struct IOBuf {
  UINT_8        *base, *ptr, *limit;
  UINT_32       num_accesses;           /* # currently open accesses */
  UINT_32       base_offset;            /* in bytes */
  UINT_32       vol_base;               /* volume # encoded as 0-locator */
  LSSAccess     *last_access;
};

/* up to 16 volumes */

#define VOLUME_BITS         (4)
#define MAX_VOLUMES         (1<<VOLUME_BITS)
#define OFFSET_MASK         ((1<<(32-VOLUME_BITS))-1)

/* each volume can have up to 2^(32-VOLUME_BITS+STORAGE_BITS)
   of data, ie, 4Gb
*/

/*  space words and record locators both use the spare bits implied
 *  by STORAGE_SHIFT_BITS.  The constructors are
 *  MAKE_SPACE_WORD() and MAKE_LOCATOR(), and the limits are
 *  max 16 volumes and max 16 compression algorithms.
 */

#define STORAGE_SHIFT_BITS  (4)
#define STORAGE_GRANULE     (1<<STORAGE_SHIFT_BITS)
#define ROUND_UP(n)         ((((n)-1)|(STORAGE_GRANULE-1))+1)

#define EXTRACT_VOLUME(at)    ((at) >> (32-VOLUME_BITS))
#define EXTRACT_OFFSET(at)    (((at) & OFFSET_MASK) << STORAGE_SHIFT_BITS)
#define MAKE_LOCATOR(vol,off) (((vol) << (32-VOLUME_BITS))+((off) >> STORAGE_SHIFT_BITS))
#define MAKE_BASED_LOCATOR(base,off)  ((base) + ((off)>>STORAGE_SHIFT_BITS))

struct LSSVolume {
  int                filedes;
  int                flags;     /* so far, only LSS_RDWR defined */
  UINT_32            cr_offset;
  UINT_64            serial_num;
  char              *file;
  struct IOBuf      *auxbuf;   /* auxillary output buffer for compaction */
};

/* space efficiency is to be ((n*GRANULE)-sizeof(32))/sizeof(32)
   even though this makes it more difficult to compute the hash
   value, and `n' large enough to amortize LSSRecordHeader 
   overhead.
*/

/*#define INDEX_LINE_SIZE  (3)  .. for testing */

/*  the `line_key' could be moved up a level,,
    thereby allowing line_key checking without faulting in the
    index line.  Something like:

    struct LSSIndexLinePtr {
      UINT_32  at;
      UINT_32  line_key;
    };
*/

#define INDEX_LINE_SIZE  (64)

struct LSSIndexEntries {
  UINT_32    entry[INDEX_LINE_SIZE];
};

struct LSSIndexLine {
  UINT_32                 line_key; /* stored in header's recnum */
  struct LSSIndexEntries  entries;
};

/* the max # of index lines allowed, given a capacity of n
*/

#define MAX_FILL(n) (((n)*192)/256)  /* 75% */

/*
 *  The MAX_DIFFS should be adjusted so that the size of
 *  a commit record is approximately the IO Block size
 *  (although a linear scan at index_insert time implies
 *  we don't want it to be outrageous.  On the other hand,
 *  that can be optimized if its too slow w/o changing
 *  the data structure, so let's max it out...)
 */

#if WORD_SIZE_BITS == 64
#define MAX_DIFFS  (120)
#else
#define MAX_DIFFS  (100)  /* about 32 words of gap space left... */
#endif

/*========================================================================

       LSSCommitRecord
      +----------------+
      |	   	       |	MIDX
      |index_offset  *-+-----> +-----+-------------+
      |index_table_cnt |       | key | line offset |	  INDX
      |		    4  |       |  37 |    12340 *--+---> +-------+
      |		       |       |  11 |    30240 *--+-->  |       |
      +----------------+       | 104 |     1200 *--+-> 	 |    	 |
       			       |  93 |   143010 *--+>  	 |    	 |
       			       +-----+-------------+   	 |    	 |
				       	       	      	 +-------+

  in-memory, the `index_offset' may be 0 to indicate that a flush
  of the MIDX is required.

  Other than that, the in-memory data structure looks like:

      LSS_V3
     +--------------+
     | 	            |          IndexEntry
     |	   index  *-+-------> +-key----at---ptr--+
     | index_cap  8 |	  [0] |	    	  0    	 |
     |	       	    |	  [1] |	    	  0    	 |
     |		    |	 h(37)|	 37   12340    *-+---> +-----+
     +--------------+	 h(11)|	 11   30240 NULL |     |     |
      		     	  [4] |	          0   	 |     :     :
       	       	  h(93)=h(104)| 104    1200 NULL |
      		          [6] |	 93  143010    *-+---> +-----+
      			  [7] |	   	  0      |     |     |
      			      +------------------+     :     :

  1. The NULL ptr indicates that an index line has not been loaded.
  2. A zero (0) for `at' with a NULL `ptr' indicates that the hash 
     table entry is not being used.
  3. A zero (0) for `at' with a non-NULL `ptr' indicates a new
     (dirty) index line.
  4. The collision between h(93) and h(104) is resolved by sequential
     search.

========================================================================*/

struct MasterIndexEntry {
  UINT_32   line_key;
  UINT_32   line_offset;     /* really a LOCATOR */
  UINT_32   vol_flags;
};

#define VOLFLAG_VOL(k)      (1<<(k))
#define VOLFLAG_DIRTY       (1<<16)     /* bits need re-computing */
#define VOLFLAG_HAS_FREE    (1<<17)     /* cache line has a free entry */

struct MasterIndexEntryNOVF {
  UINT_32   line_key;
  UINT_32   line_offset;
};

typedef struct IndexEntry {
  struct MasterIndexEntry  m;
  struct LSSIndexEntries  *mem_ptr;
} IndexEntry;

#define LSSV3_NOVF_MINOR_VERSION  (80)  /* non-volflag was v.80 */
#define LSSV3_MINOR_VERSION       (100)  /* write out at v.100 */
#define LSSV3_MINOR_WIGGLE        (9)   /* read up to minor v.109 */

#define NUM_BOOKMARKS (10)

struct LSSCommitRecord {
  UINT_32    magic;             /* 1st 4 words look like a record header */
  UINT_32    generation;
  UINT_32    cr_space_w;
  UINT_32    cr_length;         /* 0 */

  UINT_64    commit_time_ms;

  UINT_32    minor_version;     /* a potentially useful minor rev. check */
  UINT_32    prev_cr_at;
  UINT_32    self_cr_at;

  /**  information about the index lines   **/

  UINT_32    mix_cnt;        /* # entries in MIDX table */
  UINT_32    mix_offset;     /* offset of MIDX table (0==>write it out) */

  int        vh_fuel;           /* amount of fuel left for volume header */

  /**  information about the index diffs   **/

  UINT_32    num_diffs;
  struct {
    UINT_32    diff_recnum;
    UINT_32    diff_offset;
  } diffs[MAX_DIFFS];

  /*   bookmarks are links to _other_ commit records.
   *   These will are used for object-level GC by rs.db.rstore,
   *   and any LSS-level GC promises to preserve everything
   *   reachable via any of these bookmarks.
   */
    
  UINT_32      bookmarks[NUM_BOOKMARKS];
};

#define CR_SPACE ROUND_UP( sizeof( struct LSSCommitRecord ) )

typedef struct LSS_V3 {
  struct LSS         com;
  int                num_outbufs;
  unsigned           do_fsync : 1;         /* turn off for speed */
  UINT_8            *trailing_buf;
  struct LSSSegmentTrailer *trailer;        /* at the end of `trailing_buf' */

  IndexEntry           *mindex;
  UINT_32               mindex_cap;    /* size of hash table */

  /* taken from the volume header, used to determine
   * when to update the volume header
   */
  UINT_32               vh_last_cr_at;
  UINT_32               vh_last_cr_generation;

  int                num_vols;
  UINT_32            tip_base;  /* base locator */
  struct LSSVolume  *tip_vol;   /* may not be the last volume, in LSS_V3X */

  struct {
    unsigned         index;   /* the mindex[] entry we found */
    unsigned         mask;
    int              pass;
  } current_findr;

  struct LSSCommitRecord  cr;
  zip_algorithm     *zip_algs[MAX_ZIP_ALGORITHMS+1]; /* NULL at end of list */
  struct LSSVolume   vol[MAX_VOLUMES];
  struct IOBuf       outbuf[MAX_OUT_BUFS];
  struct IOBuf       inbuf[MAX_IN_BUFS];
  struct FreeMap     freemap;                        /* map of free records */
} LSS_V3;

/**
 *  How long to defer updating the volume header
 */

#define VOLUME_HEADER_FUEL    (500)
#define COMMIT_FUEL_COST       (50)
#define BLOCK_FUEL_COST         (1)

struct LSSVolumeHeader {

  UINT_32    vol_number;
  UINT_32    num_vols;
  UINT_32    vh_generation;
  UINT_32    last_cr_at;        /* where to start looking for last CR */
  UINT_32    last_cr_generation;
  UINT_64    vol_create_time_ms;/* creation time (Java-style, 64 bit ms) */

  UINT_32    zip_alg_at[MAX_ZIP_ALGORITHMS-1]; /* [0] is not stored */

  struct {
    UINT_32  vol_file_at;       /* locator to volume filename */
    UINT_32  cr_offset;         /* CR offset in that volume */
    UINT_64  vol_serial_num;    /* serial number */
    UINT_32  spare1;
    UINT_32  spare2;
  } vol_info[MAX_VOLUMES];
};

struct LSSAccess {
  size_t	       bytes;        /* uncompressed size */
  void	              *addr;
  zip_algorithm       *uses;
  struct IOBuf        *in_buf;
  size_t               space;        /* compressed size (approx) */
};

#endif /* _H_LSSV3 */
