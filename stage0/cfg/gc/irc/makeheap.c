/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gc/irc/makeheap.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.27
 * File mod date:    2005-05-26 09:19:28
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          IRC heap initializer
 *------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include "irc.h"

#ifndef atracef
FILE *atracef = NULL;
#endif

/*  for ease of debugging, manage our own heap on Alpha
 *  -- this is temporary during the Alpha port
 */

#if PLATFORM_ARCH_ALPHA
#define MMAP_OUR_HEAP     1
#define MMAP_OUR_HEAP_AT  (0x6000000000UL) /* MAX_USER_ADDR is really small */
#else
#define MMAP_OUR_HEAP     0
#endif

#if MMAP_OUR_HEAP
#include <unistd.h>
#include <sys/fcntl.h>
#include <sys/mman.h>

#define MMAP_FIRST_TIME     (16*1024*1024)
#define MMAP_AT_A_TIME      (4*1024*1024)
#define MMAP_REZ            (64*1024)

static void *mem_ptr = (void *)MMAP_OUR_HEAP_AT;
static void *mem_limit = (void *)MMAP_OUR_HEAP_AT;

static void *grab( unsigned long len )
{
  void *block = mem_ptr;

  mem_ptr = (char *)block + (1+(len | ((2*sizeof(double))-1)));

  if (mem_ptr > mem_limit)
    {
      size_t need = (char *)mem_ptr - (char *)mem_limit;
      size_t minmap;
      caddr_t rc;

      if (block == (void *)MMAP_OUR_HEAP_AT)
	minmap = MMAP_FIRST_TIME;
      else
	minmap = MMAP_AT_A_TIME;

      need = ((need - 1) | (MMAP_REZ - 1)) + 1;
      if (need < minmap)
	need = minmap;

      rc = mmap( (caddr_t)mem_limit,
		 need,
		 PROT_READ|PROT_WRITE|PROT_EXEC,
		 MAP_PRIVATE|MAP_FIXED|MAP_ANON,
		 0,
		 0 );
      assert( rc == (caddr_t)mem_limit );
      mem_limit += need;
      assert( mem_ptr <= mem_limit );
    }
  return block;
}

static void letgo( void *p )
{
}
#else
#define grab(n) malloc(n)
#define letgo(p) free(p)
#endif

static void out_of_memory( struct IRC_Heap *h, UINT_32 size )
{
  fprintf( stderr, 
	   "FATAL ERROR: allocation failed trying to grow heap by %#lx\n",
	   (unsigned long)size );
  fprintf( stderr, 
	   "  (had already grabbed %#lx + %#lx bytes from system)\n",
	   (unsigned long)h->allocedInChunks,
	   (unsigned long)h->curAllocsInBig );
  abort();
}


void irc_std_alloc_chunk( struct IRC_Heap *h )
{
  h->moreSpacePtr = grab( ALLOCATION_CHUNK_SIZE );
  if (!h->moreSpacePtr)
    out_of_memory( h, ALLOCATION_CHUNK_SIZE );
  h->allocedInChunks += ALLOCATION_CHUNK_SIZE;
  h->spaceLeft = ALLOCATION_CHUNK_SIZE;
}

void irc_std_free_big( struct IRC_Heap *h, IRC_Header *p )
{
  letgo( p );
}

IRC_Header *irc_std_alloc_big( struct IRC_Heap *h, UINT_32 size )
{
  IRC_Header *p;

  p = (IRC_Header *)grab( size + sizeof(IRC_Header) );
  if (!p)
    out_of_memory( h, size );
  h->curAllocsInBig += size + sizeof(IRC_Header);
  return p;
}

static void initSizeClass( struct IRC_Gen *gen, 
			    IRC_SizeClass *sc, 
			    int isLargeObjectQ,
			    unsigned limitBytes );

IRC_Heap *IRC_newHeap( void )
{
  return irc_init_heap( MALLOC(IRC_Heap) );
}

IRC_Heap *irc_init_heap( IRC_Heap *h )
{
UINT_8 *wbt;
unsigned gen, l, r;

/*
if (!atracef)
  atracef = stdout;
   atracef = fopen( "atrace0.log", "w" );  
*/

    h->clientInfo = NULL;
    h->moreSpacePtr = NULL;
    h->spaceLeft = 0;
    h->allocedInChunks = 0;
    h->curAllocsInBig = 0;

    for (gen=0; gen<NUM_GENERATIONS; gen++)
    {
	irc_init_gen( h, 
		  gen,
		  &h->theGenerations[gen],
		  gen ? NULL : h->sizeClassesIndex );
    }
    
    /* initialize the write-barrier table */
    wbt = h->writeBarrierTable;

    h->alloc_chunk_meth = irc_std_alloc_chunk;
    h->alloc_big_meth = irc_std_alloc_big;
    h->free_big_meth = irc_std_free_big;
    
    /* initially, all generations have 0=white, 1=black */
    
    for (l=0; l<14; l++)         /* l represents lvalue gen/color bits */
	for (r=0; r<16; r++)     /* r represents rvalue gen/color bits */
	{
	char c = WB_NONE;
	
	    if ((l/2) > (r/2))
	    {
		/* ie, young ptr in old obj */
		c = WB_GENERATION;
	    }
	    else if (((l/2) == (r/2)) && ((l & 1) && !(r & 1)))
	    {
		/* ie, white ptr in blk obj */
		c = WB_COLOR;
	    }
	    *wbt++ = c;
	}
    for (l=14; l<16; l++)
      for (r=0; r<16; r++)
	{
#if 0
	  *wbt++ = WB_GLOBAL;		/* no write is valid for gen 7 */
#endif
	  *wbt++ = WB_NONE;   /* ignore writes into pstore */
	}
    return h;
}

static void initPtrList( struct IRC_PtrList *ptrlist )
{
  ptrlist->first = NULL;
  ptrlist->last = NULL;
}

static struct IRC_Gen *first_pstore_gen;

void irc_pstore_gen_did_commit( struct IRC_Gen *gen )
{
  /* the pstore completed a commit operation, which means that 
   * everything in it is either a pointer to another object in
   * the store *or* a pointer to a pivot object.  In either case,
   * there are no more pointers such that the pstore contains
   * the only one.  That means we can empty out the list of
   * possible down pointers!
   */
  /* since we actually store in the rvalue's generation, and
     the write barrier is not active in cell (15,15), this list
     will always be empty.  We should be clearing the_arena's list
  */
extern IRC_Heap *gc_arena;

  assert( gen->extraHeapPointers.first == NULL );

  gen = &gc_arena->theGenerations[0];   /* XXX Experiment 2 */

  IRC_freePtrList( &gen->extraHeapPointers );
  initPtrList( &gen->extraHeapPointers );

  /*  Note that the GC could be in the middle of scanning the
   *  extraHeapPointers.  If we don't clear that out, it might
   *  scan garbage (either because the bucket gets reused
   *  or because an object containing an stored slot gets deallocated)
   */
  if (gen->state == GSTATE_XHEAP_SCAN) {
    gen->iterator.externals = NULL;
  }
}

void irc_pstore_gen_set_tracking( struct IRC_Gen *gen, int track_level )
{
  struct IRC_Gen *i;
  int found = 0;
  int num_tracking = 0;
  int max_tracking = 0;

  assert( gen->genNum == 7 );

  RS_LVerbose( 108, 6200, "Set tracking for gen %p to %d", gen, track_level );

  gen->tracking_level = track_level;

  for (i=first_pstore_gen; i; i=i->link) {
    if (i == gen) {
      found = 1;        /* want to assert() that we found it... */
    }
    if (i->tracking_level) {
      if (i->tracking_level > max_tracking) {
        max_tracking = i->tracking_level;
      }
      num_tracking++;
    }
  }
  assert( found );
 
  RS_LVerbose( 108, 6201, "%d generations have tracking enabled; max level=%d", 
               num_tracking, max_tracking );
  irc_config_pstore_tracking( max_tracking );
}


void irc_close_pstore_gen( struct IRC_Gen *gen )
{
  struct IRC_Gen *i, *p = NULL;

  for (i=first_pstore_gen; i; i=i->link) {
    if (i == gen) {
      
      IRC_freePtrList( &gen->extraHeapPointers );
      IRC_freePtrList( &gen->regrayObjects );
      IRC_freePtrList( &gen->markedObjects );

      /* unlink it from our list */
      if (p) {
        p->link = i->link;
      } else {
        first_pstore_gen = i->link;
      }
    }
    p = i;
  }
}

void irc_start_pstore_gens( void )
{
  struct IRC_Gen *i;

  for (i=first_pstore_gen; i; i=i->link) {
    if (i->flip_hook) {
      i->flip_hook( i->flip_hook_info, 0 );
    }
  }
}

void irc_flip_pstore_gens( void )
{
  struct IRC_Gen *i;

  for (i=first_pstore_gen; i; i=i->link) {

    if (i->flip_hook) {
      i->flip_hook( i->flip_hook_info, 1 );
    }
  }
}


void irc_init_pstore_gen( struct IRC_Gen *gen )
{
  gen->heap = NULL;
  gen->unmapped = 2;
  gen->genNum = 7;
  gen->scanning = NULL;
  gen->whiteColorCode = 0;
  gen->blackColorCode = 1;
  gen->state = GSTATE_IDLE;
  gen->tracking_level = 0;

  gen->flip_hook = NULL;
  gen->flip_hook_info = NULL;
  gen->igp_hook = NULL;
  gen->igp_hook_info = NULL;

  initPtrList( &gen->regrayObjects );
  initPtrList( &gen->markedObjects );
  initPtrList( &gen->extraHeapPointers );

  /*
   *  These are used to keep track of pointers *into* the
   *  persistent heap, so that we can kick off 
   */

  gen->link = first_pstore_gen;
  first_pstore_gen = gen;
}

void irc_init_gen( IRC_Heap *owner, 
		   unsigned genNum,
		   struct IRC_Gen *gen,
		   IRC_SizeClass **logicalSCs )
{
IRC_SizeClass *(temp[NUM_LOGICAL_SIZE_CLASSES]);
IRC_SizeClass **lsc, *psc;
int i;
UINT_32 lim;

    gen->heap = owner;
    gen->unmapped = 0;
    gen->scanning = NULL;
    gen->genNum = genNum;
    gen->whiteColorCode = 0;
    gen->blackColorCode = 1;
    gen->state = GSTATE_IDLE;
    gen->tracking_level = 0;
    gen->link = NULL;
    gen->flip_hook = NULL;
    gen->flip_hook_info = NULL;
    gen->igp_hook = NULL;
    gen->igp_hook_info = NULL;

    initPtrList( &gen->regrayObjects );
    initPtrList( &gen->markedObjects );
    initPtrList( &gen->extraHeapPointers );

    lsc = logicalSCs ? logicalSCs : temp;
	
    psc = gen->theSizeClasses;    /* physical size classes */

    /* fine size class [0] is aliased to size class [1] */
    *lsc++ = psc;

    lim = 0;
    for (i=0; i<NUM_FINE_SIZE_CLASSES-1; i++)
    {
	*lsc++ = psc;
	lim += FINE_SIZE_CLASS_RESOLUTION;
	initSizeClass( gen, psc, 0, lim );
	psc++;
    }
    for (i=0; i<NUM_COARSE_SIZE_CLASSES; i++)
    {
	*lsc++ = psc;
	lim += COARSE_SIZE_CLASS_RESOLUTION;
	initSizeClass( gen, psc, 0, lim );
	psc++;
    }
    *lsc++ = psc;
    initSizeClass( gen, psc, 1, 0xFFFFFFFF - sizeof(IRC_Header) );

    /* initialize the traversal work table */
    
    /* initially, only need to do work on encountering
       a white object in our own generation
    */
    for (i=0; i<16; i++)
	gen->traversalWork[i] = 0;
    gen->traversalWork[genNum*2] = 1;

    /* or when encountering a WHITE object in a pstore */
    gen->traversalWork[7*2+0] = 1;
}

static void initSizeClass( struct IRC_Gen *gen, 
		    IRC_SizeClass *sc, 
		    int isLargeObjectQ,
		    unsigned limitBytes )
{
IRC_Header *m;
unsigned num;

    sc->gen = gen;
    sc->heap = gen->heap;
    sc->itemSize = limitBytes + sizeof(IRC_Header);

    if (sc->itemSize > 256)
    {
	num = 2048 / sc->itemSize;
	if (num == 0)
	    num = 1;
    }
    else
	num = 8;

    sc->chunkSize = sc->itemSize * num;
    sc->isLargeObject = isLargeObjectQ;
    
    m = &sc->marker;
    sc->white = sc->black = sc->gray = sc->free = m;

    m->next = m->prev = m;
    /* set the sizeClass of the marker to be NULL
       so we can easily recognize it (and cause a SEGV
       in case we accidently try to use the marker as a
       real object!)
    */
    m->sizeClass = NULL;
    m->flagBits = 0xF;
    sc->initFlagBits = 0;
}

#include <stdarg.h>
#include <sys/time.h>
#include <string.h>

void RS_LVerbose( unsigned cat_id, unsigned msg_id, const char *fmt, ... )
{
  va_list va;
  static int vflag = -1;

  if (vflag == -1) {
    if (getenv( "RS_LVERBOSE" )) {
      vflag = 1;
    } else {
      vflag = 0;
    }
  }

  if (!vflag) {
    return;
  }

  printf( "%03u-%04uV ", cat_id, msg_id );
  va_start( va, fmt );
  vprintf( fmt, va );
  va_end( va );
  putchar( '\n' );
}

int RS_LPGC( unsigned cat_id, unsigned msg_id, const char *fmt, ... )
{
  static int phasef_init = 0;
  static FILE *phasef_log = NULL;
  struct timeval t;
  va_list ap;

  if (!phasef_init) {
    const char *fn = getenv( "RS_LPGC" );

    if (fn) {
      if (strcmp( fn, "stdout" ) == 0) {
        phasef_log = stdout;
      } else {
        phasef_log = fopen( fn, "a" );
        if (!phasef_log) {
          perror( fn );
        }
      }
    }
    phasef_init = 1;
  }

  if (!phasef_log) {
    return 0;
  }

  if (!fmt) {
    return 1;
  }
  gettimeofday( &t, NULL );
  fprintf( phasef_log, "%lu.%06lu ", 
           (unsigned long)t.tv_sec,
           (unsigned long)t.tv_usec );

  fprintf( phasef_log, "%03u-%04uT ", cat_id, msg_id );

  va_start( ap, fmt );
  vfprintf( phasef_log, fmt, ap );
  va_end( ap );

  fputc( '\n', phasef_log );
  fflush( phasef_log );
  return 1;
}
