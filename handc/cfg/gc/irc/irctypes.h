/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gc/irc/irctypes.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.21
 * File mod date:    2005-05-26 09:20:01
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#ifndef _H_IRCTYPES
#define _H_IRCTYPES

#ifdef RS_NEW_BUILD
#include <rscheme/config.h>
#else
#include <rscheme/platform.h>
#endif

#include <rscheme/gcconfig.h>
#include <rscheme/clientyp.h>

struct IRC_SizeClass;

typedef struct IRC_Header {
    struct IRC_Header	 *next;
    struct IRC_Header	 *prev;
    struct IRC_SizeClass *sizeClass;
    UINT_32		  flagBits;
} IRC_Header;

#define WATCHED  0
/* debug support for CR 729:
   #define WATCHED  ((IRC_Header *)0x6001212c00) 
*/

#if WATCHED
void IRC_check_freelist( IRC_Header *p );

#define WATCH_IT(p) (IRC_check_freelist(p), ((p) == WATCHED))
#else
#define WATCH_IT(p) 0
#endif

/*
 *  setting this to `1' causes every object on the free list
 *  to get a flagBits of 0xdeadbeef, at the cost of flip time
 *  (ie, it becomes proportional to number of dying objects)
 */

#define FREE_HAS_DEADBEEF  0


/*  header word bits (31 is MSB, 0 is LSB)

note: there may be some efficiency to be gained in IRC_blackenGenerationGrays
by making the age in the MS 4 bits, to effect a comparison w/o
a mask

        hi lo num descr
	31-24 (8) client byte -- 8 bits available for use by the
		  IRC client
	23-16 (8) mark count
        12-15 (4) (unused)
	 8-11 (4) age -- 0=>new; used to time promotion

	  7   (1) mapped object -- 1=>loaded from fasl/snap file
	              used to supress free() of large objects
	  6   (1) write protect -- 1=>this object is not writable
	  	  the IRC doesn't check this bit unless the write-barrier
		  trips
	  5   (1) will regray -- 1=>this object has already been regrayed
	  4   (1) in marked set -- 1=>this object is in the "marked set"
	  	  (useful to have a seperate bit, because if an object
		  is unmarked (mark count becomes 0) and remarked soon
		  thereafter, we won't have to do anything
	 3-1  (3) generation number
	  0   (1) color bit -- meaning of 0 & 1 alternate between
	  	  white & black/gray

  all bits are 0 unless otherwise specified


  7   6   5   4   3   2   1   0
+---+---+---+---+---+---+---+---+
|  C L I E N T      B Y T E     |   0
+---+---+---+---+---+---+---+---+
|  M A R K E D      C O U N T   |   1
+---+---+---+---+---+---+---+---+
|  u n u s e d  |    A  G  E    |   2
+---+---+---+---+---+---+---+---+
| P | W | R | M |  G  E  N  | C |   3    C=color M=marked R=regray 
+---+---+---+---+---+---+---+---+        W=writeprot P=mapped
                                   
*/

#define IRCH(p) (((IRC_Header *)p)-1)
                                 
#define IRC_MASK_COLOR      (1<<0)
#define IRC_MASK_GEN        (7<<1)
#define IRC_MASK_MARKEDQ    (1<<4) 
#define IRC_MASK_REGRAYQ    (1<<5) 
#define IRC_MASK_WRITEPROT  (1<<6) 
#define IRC_MASK_MAPPED     (1<<7) 
                                   
#define IRC_MASK_AGE        (15<<8)
#define IRC_MASK_MARKCOUNT  (255<<16)
#define IRC_MASK_CLIENTBYTE (255<<24)

#define IRCH_WRITEPROT(p) ((IRCH(p)->flagBits) & IRC_MASK_WRITEPROT)
#define IRCH_AGE(p)       ((IRCH(p)->flagBits) & IRC_MASK_AGE)
#define IRC_AGE_1         (1<<8)
#define IRC_AGE_MAX       (NUM_STEPS<<8)

#define IRC_writeBarrierCode(heap,lvalue,rvalue) \
		    ((heap)->writeBarrierTable[\
			((IRCH(lvalue)->flagBits & 0xF) << 4) \
			+(IRCH(rvalue)->flagBits & 0xF)])

#define IRC_PTR_BUCKET_SIZE (61)

struct IRC_PtrBucket {
  struct IRC_PtrBucket   *next;
  IRC_Header		**ptr;          /* current index into contents[] */
  IRC_Header		 *(contents[IRC_PTR_BUCKET_SIZE]);
};

struct IRC_PtrList {
    struct IRC_PtrBucket *first;
    struct IRC_PtrBucket *last;
};

void IRC_ptrListAdd( struct IRC_PtrList *list, IRC_Header *h );
void IRC_freePtrList( struct IRC_PtrList *ptrlist );

/*
 *     black         gray           white         free
 *       |             |              |             |
 *       +---+         +----+         +---+         +---+   +---+
 *       |   |-------->|    |-------->|   |-------->|   |-->|   |---+
 *       +---+         +----+         +---+         +---+   +---+   |
 *       ^                                                         /
 *        \-------------------------------------------------------'
 */

typedef struct IRC_SizeClass {
    IRC_Header		marker;		/* circular list seperator */
    IRC_Header 		*free;		/* free list */
    UINT_32		initFlagBits;	/* initial flag bits */
    IRC_Header 		*white;		/* allocated list (not seen yet) */
    IRC_Header 		*gray;		/* seen but not scanned */
    IRC_Header 		*black;		/* allocated list (seen already) */
    UINT_32		itemSize;	/* how big each item is */
    UINT_32		chunkSize;	/* how big a chunk is */
    int			isLargeObject;	/* large-object behavior? */
    struct IRC_Gen	*gen;		/* the generation we belong to */
    struct IRC_Heap	*heap;		/* the heap we live in */
} IRC_SizeClass;


struct IRC_Gen {
    unsigned		genNum;		/* generation number */
    int                 unmapped;       /* flag for marking it `unmapped' */
    struct IRC_Header   *scanning;      /* set while scanning an object */
    struct IRC_Heap	*heap;		/* the heap we live in */
    IRC_SizeClass	theSizeClasses[NUM_PHYSICAL_SIZE_CLASSES];
    struct IRC_PtrList	regrayObjects;
    struct IRC_PtrList	markedObjects;
    /* heap externals are places OUTSIDE this heap that pointers
       to INSIDE this heap may be stored.  ie, filled in by
       stores into a pstore
    */
    struct IRC_PtrList  extraHeapPointers;
    UINT_8		traversalWork[16];	/* where to do work */
    int			state;			/* current type of work */
    union {
	IRC_clientStableRootIterator		stable;
	IRC_clientQuasistableRootIterator	quasistable;
	IRC_clientUnstableRootIterator		unstable;
	struct IRC_PtrBucket                   *marked;
	struct IRC_PtrBucket                   *externals;
    } iterator;
    unsigned char	whiteColorCode;		/* the color code of 
    						   white objects */
    unsigned char	blackColorCode;		/* the color code of 
    						   black objects */
    struct IRC_Gen     *link;
    int                 tracking_level;          /* pstore tracking live objects, and how? */
    void              (*flip_hook)( void *info, int state );     /* arrange to be called when Gen 0 flips */
    void               *flip_hook_info;
    void              (*igp_hook)( void *info, 
                                   void *lvalue, UINT_32 offset,
                                   void *rvalue );
    void               *igp_hook_info;
};

struct IRC_Heap {
    void 		*clientInfo;
    IRC_SizeClass	*(sizeClassesIndex[NUM_LOGICAL_SIZE_CLASSES]);
    UINT_8		writeBarrierTable[256];
    void		*moreSpacePtr;
    UINT_32		spaceLeft;
    UINT_32             allocedInChunks;
    UINT_32             curAllocsInBig;
    void                (*alloc_chunk_meth)( struct IRC_Heap * );
    IRC_Header         *(*alloc_big_meth)( struct IRC_Heap *, UINT_32 size );
    void                (*free_big_meth)( struct IRC_Heap *, IRC_Header *ptr );
    struct IRC_Gen	theGenerations[NUM_GENERATIONS];
};

void irc_init_gen( IRC_Heap *owner, 
		   unsigned genNum,
		   struct IRC_Gen *gen,
		   IRC_SizeClass **logicalSCs );

IRC_Heap *irc_init_heap( IRC_Heap *h );
void irc_std_alloc_chunk( struct IRC_Heap *h );
IRC_Header *irc_std_alloc_big( struct IRC_Heap *h, UINT_32 size );

void irc_init_pstore_gen( struct IRC_Gen *gen );
void irc_close_pstore_gen( struct IRC_Gen *gen );

void irc_start_pstore_gens( void );     /* starting traversal */
void irc_flip_pstore_gens( void );     /* finished traversal */

void irc_pstore_gen_did_commit( struct IRC_Gen *gen );
void irc_config_pstore_tracking( int level );  /* used internally */
void irc_pstore_gen_set_tracking( struct IRC_Gen *gen, int track_level );
void irc_init_pstore_writebarrier( void *info,
                                   int (*trapfn)( void *info,
                                                  IRC_Heap *heap,
                                                  void *lvalue, 
                                                  UINT_32 offset, 
                                                  void *rvalue ) );

int irc_grayify( void *gcptr );

void RS_LVerbose( unsigned cat_id, unsigned msg_id, const char *fmt, ... );
int RS_LPGC( unsigned cat_id, unsigned msg_id, const char *fmt, ... );
#define RS_LPGC_ACTIVE() RS_LPGC(0,0,NULL)

#endif /* _H_IRCTYPES */
