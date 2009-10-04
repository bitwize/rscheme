/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gc/irc/traverse.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.26
 * File mod date:    2005-05-26 09:20:01
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          IRC incremental traverser
 *------------------------------------------------------------------------*/

#include "irc.h"
#include <stdio.h>
#include <rscheme/smemory.h>
#include <rscheme/scheme.h>

#ifdef INLINES
#include <rscheme/gcclient1.ci>
#include <rscheme/gcclient2.ci>
#else
#if INCLUDE_PSTORE_UNMAPPER
extern obj irc_current_scan;
#endif /* INCLUDE_PSTORE_UNMAPPER */
#endif
#include <rscheme/profile.h>

#ifdef DEBUG_TRAVERSAL
void plist( struct IRC_SizeClass *sc )
{
    printSizeClass(sc);
}

static void pneighbors( IRC_Header *item, char *msg )
{
unsigned f = item->flagBits;

    printf( "-------------- %s\n", msg );
    printf( "[%p] PREV:   next = %p\n", item->prev, item->prev->next );
    printf( "[%p] ", item );
    printf( " color=%d gen=%d age=%d marked=%c "
    	    "regrayed=%c wprot=%c marks=%d client=%02x\n",
	    (f & IRC_MASK_COLOR) >> 0,
	    (f & IRC_MASK_GEN) >> 1,
	    (f & IRC_MASK_AGE) >> 8,
	    (f & IRC_MASK_MARKEDQ) ? 'Y' : 'N',
	    (f & IRC_MASK_REGRAYQ) ? 'Y' : 'N',
	    (f & IRC_MASK_WRITEPROT) ? 'Y' : 'N',
	    (f & IRC_MASK_MARKCOUNT) >> 16,
	    (f & IRC_MASK_CLIENTBYTE) >> 24 );
    printf( "[%p] NEXT:   prev = %p\n", item->next, item->next->prev );
}
#endif /* DEBUG_TRAVERSAL */

#define BILL_PER_ROOT (10)

static void promote( struct IRC_SizeClass *sc,
		     struct IRC_Gen *gen,
		     IRC_Header *item )
{
IRC_Header *prv, *nxt;
struct IRC_SizeClass *nsc;

    /* reset it's sizeclass */

    nsc = &(gen+1)->theSizeClasses[ sc - gen->theSizeClasses ];
#ifdef DEBUG_TRAVERSAL
    printf( "    promote (sc #%d, nsc #%d)\n", 
    		sc - gen->theSizeClasses,
		nsc - (gen+1)->theSizeClasses );
#endif /* DEBUG_TRAVERSAL */

    
    /* unsnap it from it's old generation */

#ifdef DEBUG_TRAVERSAL
    pneighbors( item, "before unsnap" );
    printf( "old size class:\n" );
    plist( sc );
#endif /* DEBUG_TRAVERSAL */
    
    prv = item->prev;
    nxt = item->next;
    
    sc->gray = nxt;
    
    prv->next = nxt;
    nxt->prev = prv;
    
    if (sc->black == item)
	sc->black = nxt;
    /* snap it into it's new generation */
    
    nxt = nsc->gray;
    prv = nxt->prev;
    
    item->sizeClass = nsc; 
    item->next = nxt; nxt->prev = item;
    item->prev = prv; prv->next = item;
    
    if (nsc->black == nsc->gray)
    {
	nsc->black = item;
    }
    nsc->gray = item;
    
    /* mark it gray in the new generation */
    
    item->flagBits = (item->flagBits & ~IRC_MASK_GEN) 
		     + 2			/* increment generation # */
		     + (gen+1)->blackColorCode;	/* make black in new gen */

#ifdef DEBUG_TRAVERSAL
    pneighbors( item, "after snap" );
    printf( "old size class:\n" );
    plist( sc );
    printf( "new size class:\n" );
    plist( nsc );
#endif /* DEBUG_TRAVERSAL */
}

/*  traversal control  */

/* do some traversal in a generation, not to exceed
   workAmount (by much -- this isn't realtime)
   
   returns the amount of time remaining if we blackened
   everything we could, or 0 if we ran out of time
*/

static INT_32 IRC_blackenGenerationGrays( struct IRC_Gen *gen, 
					  INT_32 workAmount )
{
UINT_8 *travWork = gen->traversalWork;
INT_32 perObjectWork;
struct IRC_SizeClass *scLimit, *sc;
int anyGrays;
IRC_Header *g;

    scLimit = gen->theSizeClasses + NUM_PHYSICAL_SIZE_CLASSES;
    do {
	anyGrays = 0;
	for (sc=gen->theSizeClasses; sc<scLimit; sc++)
	{
	    perObjectWork = sc->itemSize;
	    /* note that we can't cache either gray or white,
	       because they may get modified when graying
	       something
	    */
	    while ((g = sc->gray) != sc->white)
	    {
		if ((g->flagBits & IRC_MASK_AGE) == IRC_AGE_MAX)
		{
		    if (gen->genNum < (NUM_GENERATIONS-1))
		    {
#ifdef DEBUG_TRAVERSAL
			printf( "promoting: %p\n", g );
#endif /* DEBUG_TRAVERSAL */
			/* XXX promotion disabled because IGPs aren't
			   implemented (so promoting an object loses
			   all of it's pointers! */
			/* promote( sc, gen, g ); */
			/* --- instead... --- */
			g->flagBits &= ~IRC_MASK_AGE;  /* reset age to 0 */
			/* fast unsnap from gray set */
			sc->gray = g->next;
		    }
		    else
		    {
			/* fast unsnap from gray set */
			sc->gray = g->next;
#ifdef DEBUG_TRAVERSAL
			printf( "blackening: %p (resetting age from %d)\n", 
				g, 
				(g->flagBits & IRC_MASK_AGE) >> 8 );
#endif /* DEBUG_TRAVERSAL */
			g->flagBits = g->flagBits & ~IRC_MASK_AGE;
		    }
		}
		else
		{
		    /* fast unsnap from gray set */
		    sc->gray = g->next;
#ifdef DEBUG_TRAVERSAL
		    printf( "blackening: %p (age = %d)\n", 
		    	    g, 
			    (g->flagBits & IRC_MASK_AGE) >> 8 );
#endif /* DEBUG_TRAVERSAL */
		    g->flagBits += IRC_AGE_1;
		}
		/* g is the object being blackened */
		
                gen->scanning = g;
		IRC_clientFindPointers( gen, g+1, travWork );
		gen->scanning = NULL;

		if ((workAmount -= perObjectWork) <= 0)
		    return 0;
		anyGrays = 1;
	    }
	}
    } while (anyGrays);
    return workAmount;
}

/* this is ONLY called when we've found a pointer
   to something which needs traversal work done for
   it (ie, IRC_TRAV() evaluated to non-zero).

   Note that if INCLUDE_PSTORE_UNMAPPER is defined,
   then that may be true during a `pstore-close'-induced
   traversal, in which case we are watching out for unmapped
   generations.  If this procedures returns non-zero, then
   the caller will replace the stored pointer with #unmapped.
*/

#if INCLUDE_PSTORE_UNMAPPER
obj found_unmapped;
#endif

int IRC_foundPointer( void *ircInfo, void *ptr )
{
  struct IRC_Gen *gen = ircInfo;
  IRC_Header *b = IRCH(ptr);
  struct IRC_SizeClass *sc;

  assert( IRC_TRAV(gen->traversalWork,ptr) );

#ifdef DEBUG_TRAVERSAL
  printf( "    found pointer: %p, IRC_TRAV = %d\n", 
          b, IRC_TRAV(gen->traversalWork,ptr) );
#endif /* DEBUG_TRAVERSAL */
  sc = b->sizeClass;

  if (sc->gen->genNum == 7) {
    /*  it's an object in a pstore; let them handle it */
    if (sc->gen->igp_hook) {
      sc->gen->igp_hook( sc->gen->igp_hook_info,
                         NULL, 0, 
                         ptr );
    }
    return 0;
  }

#if INCLUDE_PSTORE_UNMAPPER
  if (sc->gen->unmapped)
    {
      obj x = GCPTR_TO_PTR( ptr );

      if (sc->gen->unmapped == 2)
        return 0;
      
      printf( "** detected unmapped pointer from #[%s %x] to #[%s %x] in heap <%p> **\n", 
              symbol_text( class_name( object_class( irc_current_scan ) ) ),
              VAL(irc_current_scan),
              symbol_text( class_name( object_class( x ) ) ),
              VAL(x),
              sc->gen );
      found_unmapped = x;
      return 1;
    }
#endif /* INCLUDE_PSTORE_UNMAPPER */

  b->flagBits ^= IRC_MASK_COLOR;
    
  if (sc->white == b)
    {
      if (WATCH_IT(b))
	printf( "IRC_foundPointer: %p ; was first white\n", b );

      sc->white = b->next;
    }
  else
    {
      IRC_Header *prv, *nxt;
    
#ifdef DEBUG_TRAVERSAL
      pneighbors( b, "IRC_foundPointer: before unsnap" );
      printf( "whole size class:\n" );
      plist( sc );
#endif /* DEBUG_TRAVERSAL */
    
      /* unsnap */

      prv = b->prev;
      nxt = b->next;
	
      prv->next = nxt;
      nxt->prev = prv;

      /* snap */
	
      nxt = sc->gray;
      prv = nxt->prev;
	
      b->next = nxt; nxt->prev = b;
      b->prev = prv; prv->next = b;
	
      if (sc->black == sc->gray)
	{
	  if (WATCH_IT(b))
	    printf( "IRC_foundPointer: %p ; new first black\n", b );

          sc->black = b;
	}
      else
        {
          if (WATCH_IT(b))
            printf( "IRC_foundPointer: %p\n", b );
        }
      sc->gray = b;
	
#ifdef DEBUG_TRAVERSAL
      pneighbors( b, "IRC_foundPointer: after snap" );
      printf( "whole size class:\n" );
      plist( sc );
#endif /* DEBUG_TRAVERSAL */
    }
  return 0;
}

int irc_grayify( void *ptr )
{
  IRC_Header *h = IRCH(ptr);
  struct IRC_Gen *gen;

  gen = h->sizeClass->gen;

  if (IRC_TRAV(gen->traversalWork,ptr)) {
    RS_LVerbose( 108, 3990, "irc_grayify %p [%08x] in gen %p", ptr, h->flagBits, gen );
    IRC_foundPointer( gen, ptr );
    return 1;
  } else {
    RS_LVerbose( 108, 3991, "irc_grayify %p [%08x] in gen %p : nop", ptr, h->flagBits, gen );
    return 0;
  }
}

/* do a flip */

static void flipFlags( struct IRC_Gen *gen )
{
UINT_8 *wbt, temp;
int genNum = gen->genNum, i, l, r;
#define WBT(l,r) wbt[(l)*16+(r)]

    gen->whiteColorCode ^= IRC_MASK_COLOR;
    gen->blackColorCode ^= IRC_MASK_COLOR;
    for (i=0; i<NUM_PHYSICAL_SIZE_CLASSES; i++)
	gen->theSizeClasses[i].initFlagBits ^= IRC_MASK_COLOR;
    wbt = gen->heap->writeBarrierTable;
    
    for (l=0; l<16; l++)
    {
	temp = WBT(l,genNum*2);
	WBT(l,genNum*2) = WBT(l,genNum*2+1);
	WBT(l,genNum*2+1) = temp;
    }
    
    for (r=0; r<16; r++)
    {
	temp = WBT(genNum*2,r);
	WBT(genNum*2,r) = WBT(genNum*2+1,r);
	WBT(genNum*2+1,r) = temp;
    }
    
    /* update everybody's traversal work vector to reflect the change */
    for (i=0; i<NUM_GENERATIONS; i++)
    {
    UINT_8 *twt;
    
	twt = gen->heap->theGenerations[i].traversalWork;
	temp = twt[genNum*2];
	twt[genNum*2] = twt[genNum*2+1];
	twt[genNum*2+1] = temp;
    }
#undef WBT
}

static void reclaimNonLargeObjectGarbage( struct IRC_SizeClass *sc )
{
    assert( !sc->isLargeObject );
    assert( sc->gray == sc->white );

    if (rsprof_active)
      { IRC_Header *p;
	
	for (p=sc->white; p!=sc->free; p=p->next)
	  {
	    rsprof_obj_died( GCPTR_TO_PTR(p+1) );
	  }
      }

    if (atracef)
      { IRC_Header *p;
	
	for (p=sc->white; p!=sc->free; p=p->next)
	  {
	    fprintf( atracef, ".. FREE: %p\n", p+1 );
	    ((UINT_32 *)(p+1))[0] = 0x66600666;
	    ((UINT_32 *)(p+1))[1] = 0x55500555;
	  }
	fflush( atracef );
      }

    sc->free = sc->white;
    sc->gray = sc->white = sc->black;
}

static void reclaimLargeObjectGarbage( struct IRC_SizeClass *sc )
{
  IRC_Header *p, *prv, *nxt;
  void (*this_byebye)( IRC_Heap *, IRC_Header * );
  IRC_Heap *this_heap;
  
  this_heap = sc->heap;
  this_byebye = this_heap->free_big_meth;

    assert( sc->isLargeObject );
    assert( sc->gray == sc->white );
    assert( sc->free == &sc->marker );

    /* if the black list is empty (ie, black==white), then
     * set the black pointer to the marker
     */

    if (sc->white == sc->black)
      sc->black = &sc->marker;

    /* save a pointer to the white list */
    p = sc->white;

    /* detach the entire white list (white...marker)
     * from the block list
     */
       
    prv = p->prev;
    prv->next = &sc->marker;
    sc->marker.prev = prv;

    /*  zip through the white list, freeing the malloc()'d large objects */

    while (p != &sc->marker)
    {
        if (rsprof_active)
          rsprof_obj_died( GCPTR_TO_PTR(p+1) );
	nxt = p->next;
	if (!(p->flagBits & IRC_MASK_MAPPED))
	  this_byebye( this_heap, p );
	p = nxt;
    }

    /* update the gray and white pointers to make the white list
     * (and gray list) empty
     */
    sc->gray = sc->white = sc->black;
}

#if defined(DEBUG_TRAVERSAL) || FREE_HAS_DEADBEEF
void IRC_deathKnoll( struct IRC_SizeClass *sc )
{
  IRC_Header *p;
  
  for (p=sc->white; p != sc->free; p=p->next)
    {
#if defined(DEBUG_TRAVERSAL)
      printf( "death to: %p\n", p+1 );
#endif
    if (WATCH_IT(p))
      printf( "IRC_deathKnoll: %p\n", p );

#if FREE_HAS_DEADBEEF
      p->flagBits = 0xDEADBEEF;
      {
	UINT_32 i, n, *body = (UINT_32 *)(p+1);
	n = (sc->itemSize - sizeof(IRC_Header))/sizeof(UINT_32);
	if (!sc->isLargeObject)
	  for (i=0; i<n; i++)
	    body[i] = 0xFEEDFACE;
      }
#endif
    }
}
#endif /* DEBUG_TRAVERSAL */

static void generationFlip( struct IRC_Gen *gen )
{
  struct IRC_SizeClass *sc;
  int i;
  UINT_8 *wbt, *twt, *cell;
  
  if (atracef) 
    fprintf( atracef, "-- GENERATION FLIP %d\n", gen->genNum );

  /* printf(  "-- GENERATION FLIP %d\n", gen->genNum ); */

#if defined(DEBUG_TRAVERSAL) || FREE_HAS_DEADBEEF
    for (sc = gen->theSizeClasses, i=0; 
	 i<NUM_PHYSICAL_SIZE_CLASSES; 
	 i++, sc++)
      {
	IRC_deathKnoll( sc );
      }
#endif /* DEBUG_TRAVERSAL */

    for (sc = gen->theSizeClasses, i=0;
	 i<NUM_PHYSICAL_SIZE_CLASSES-1; 
	 i++, sc++)
      {
	reclaimNonLargeObjectGarbage( sc );
      }
    reclaimLargeObjectGarbage( sc );
    flipFlags( gen );
    
    /* release elder generation from Phase II */
    
    if (gen->genNum < NUM_GENERATIONS-1)
    {
    struct IRC_Gen *elder = gen+1;
    
	if (elder->state == GSTATE_PHASE2)
	    elder->state = GSTATE_TRAVERSE_5;
    }
    
    /* stop having younger generations do work for us */
    
    wbt = gen->heap->writeBarrierTable;
    
    for (i=0; i<gen->genNum; i++)
    {    
	/* remove our work from the write barrrier */
	cell = &wbt[ i*16 + gen->genNum*2 ];
	cell[0] = cell[1] = cell[16] = cell[17] = 0;
	
	/* remove our work from their traversal table */
	twt = gen->heap->theGenerations[i].traversalWork;
	cell = &twt[gen->genNum*2];
	cell[0] = cell[1] = 0;
    }
}

static void init_stable_scan( struct IRC_Gen *gen )
{
    IRC_clientStableRootIteratorInit( &gen->iterator.stable );
}

static void init_igp_scan( struct IRC_Gen *gen )
{
}

static void init_marked_scan( struct IRC_Gen *gen )
{
}

static void init_quasistable_scan( struct IRC_Gen *gen )
{
    IRC_clientQuasistableRootIteratorInit( &gen->iterator.quasistable );
}

static void init_unstable_scan( struct IRC_Gen *gen )
{
    IRC_clientUnstableRootIteratorInit( &gen->iterator.unstable );
}


static void startGenCollection( struct IRC_Gen *gen )
{
  assert( gen->state == GSTATE_IDLE );
  if (atracef) fprintf( atracef, "start gen collection: %d\n", gen->genNum );

  gen->state = GSTATE_STABLE_SCAN;
  init_stable_scan( gen );
}

static INT_32 run_stable_scan( struct IRC_Gen *gen, INT_32 time )
{
UINT_8 *twt;
void *p;

    twt = gen->traversalWork;
    while (time > 0)
    {
	p = IRC_clientStableRootIteratorNext( &gen->iterator.stable );
	if (!p)
	    return time;
	if (IRC_TRAV(twt,p)) 
	  {
	    if (atracef) fprintf( atracef, "   stable root = %p\n", p );
	    if (IRC_foundPointer( gen, p ))
              {
#if INCLUDE_PSTORE_UNMAPPER
                IRC_clientStableRootWasUnmapped( &gen->iterator.stable );
#endif /* INCLUDE_PSTORE_UNMAPPER */
              }
	  }
	time -= BILL_PER_ROOT;
    }
    return 0;
}

static INT_32 run_igp_scan( struct IRC_Gen *gen, INT_32 time )
{
    return time;
}


static INT_32 run_marked_scan( struct IRC_Gen *gen, INT_32 time )
{
  struct IRC_PtrBucket **state = &gen->iterator.marked;
  UINT_8 *twt;

  twt = gen->traversalWork;

  /* scan a single bucket, until we run out of time points */

  while (*state && (time > (BILL_PER_ROOT * IRC_PTR_BUCKET_SIZE)))
    {
      struct IRC_PtrBucket *b = *state;
      IRC_Header *h, **p, **lim;
      unsigned n;

      *state = b->next;
      p = b->contents;
      lim = b->ptr;
      time -= BILL_PER_ROOT * IRC_PTR_BUCKET_SIZE;

      n = 0;
      while (p < lim)
	{
	  h = *p++;

	  assert( h->flagBits & IRC_MASK_MARKEDQ );
	  if (h)
	    {
	      /* still marked? */
	      if (h->flagBits & IRC_MASK_MARKCOUNT)
		{
		  void *ptr;

		  n++;
		  ptr = h + 1;
		  if (IRC_TRAV(twt,ptr))
		    {
		      if (IRC_foundPointer( gen, ptr ))
                        {
                          /* object was unmapped -- removed from list */
                          p[-1] = NULL;
                        }
		    }
		}
	      else
		{
		  /* no.. remove from the list */
		  p[-1] = NULL;
		  h->flagBits &= ~IRC_MASK_MARKEDQ;
		}
	    }
	}
    }
  return time;
}

static void init_externals_scan( struct IRC_Gen *gen )
{
  gen->iterator.externals = gen->extraHeapPointers.first;
}

static INT_32 run_externals_scan( struct IRC_Gen *gen, INT_32 time )
{
  struct IRC_PtrBucket **state = &gen->iterator.externals;
  UINT_8 *twt;

  twt = gen->traversalWork;

  /* scan a single bucket, until we run out of time points */

  while (*state && (time > (BILL_PER_ROOT * IRC_PTR_BUCKET_SIZE)))
    {
      struct IRC_PtrBucket *b = *state;
      IRC_Header *h, **p, **lim;
      unsigned n;

      time -= BILL_PER_ROOT * IRC_PTR_BUCKET_SIZE;
      *state = b->next;
      p = b->contents;
      lim = b->ptr;

      /*printf( "- scan externals bucket %p qty %u\n", b, lim - p );*/

      n = 0;
      while (p < lim)
	{
	  gc_obj_addr objp;
	  pos_ptr_addr pp;

	  /* the ptrlist is really a list of pointers to possible pointers */

	  pp = (pos_ptr_addr) *p++;
	  if (!pp)
	    continue;

	  /*printf( " POSSIBLE DOWN-POINTER AT: %#08x (==> %#08x)\n", 
	     pp, *pp ); */

	  objp = cast_and_deref_ptr( pp );

	  if (objp)
	    {
	      /* see if it's really a pointer to an object
	       * in our generation
	       */
	      h = IRCH(objp);
	      
	      if (h->sizeClass->gen == gen)
		{
		  if (IRC_TRAV(twt,objp))
                    {
                      /*
                       *  Note:  we should never see a pointer within the
                       *  same generation that has also been unmapped
                       *  (because whole generations are mapped together)
                       */
                      IRC_foundPointer( gen, objp ); /* is never unmapped */
                    }
		}
	      else
		{
		  /* p[-1] = NULL; */
		}
	    }
	  else
	    {
	      /* p[-1] = NULL; */
	    }
	}
    }
  return time;
}

static INT_32 run_quasistable_scan( struct IRC_Gen *gen, INT_32 time )
{
UINT_8 *twt;
void *p;

    twt = gen->traversalWork;
    while (time > 0)
    {
	p = IRC_clientQuasistableRootIteratorNext( 
			&gen->iterator.quasistable );
	if (!p)
	    return time;
	if (IRC_TRAV(twt,p))
	  {
	    if (atracef) fprintf( atracef, "   quasistable root = %p\n", p );
	    if (IRC_foundPointer( gen, p ))
              {
#if INCLUDE_PSTORE_UNMAPPER
                IRC_clientQuasistableRootWasUnmapped( &gen->iterator.quasistable );
#endif /* INCLUDE_PSTORE_UNMAPPER */
              }
	  }
	time -= BILL_PER_ROOT;
    }
    return 0;
}

static INT_32 run_unstable_scan( struct IRC_Gen *gen, INT_32 time )
{
UINT_8 *twt;
void *p;

    twt = gen->traversalWork;
    while (time > 0)
    {
	p = IRC_clientUnstableRootIteratorNext( &gen->iterator.unstable );
	if (!p)
	    return time;
	if (IRC_TRAV(twt,p))
	  {
	    if (atracef) fprintf( atracef, "   unstable root = %p\n", p );
	    if (IRC_foundPointer( gen, p ))
              {
#if INCLUDE_PSTORE_UNMAPPER
                IRC_clientUnstableRootWasUnmapped( &gen->iterator.unstable );
#endif /* INCLUDE_PSTORE_UNMAPPER */
              }
	  }
	time -= BILL_PER_ROOT;
    }
    return 0;
}

static INT_32 do_traversal( struct IRC_Gen *gen, INT_32 time )
{
    return IRC_blackenGenerationGrays( gen, time );
}

/* blacken any regrayed objects */
    
static INT_32 blacken_regrayed( struct IRC_Gen *gen, INT_32 time )
{
  while (gen->regrayObjects.first)
    {
      struct IRC_PtrBucket *b;
      IRC_Header **pp;
      UINT_32 i = 0;

      b = gen->regrayObjects.first;
      if (atracef) {
	fprintf( atracef, "reblackening a bucket (%u in it)...\n",
		 (unsigned)(b->ptr - b->contents) );
	fflush( atracef );
      }
      if (!(gen->regrayObjects.first = b->next))
	{
	  gen->regrayObjects.last = NULL;
	}
      
      for (pp=b->contents; pp<b->ptr;)
	{
	  struct IRC_Header *g = *pp++;

	  if (WATCH_IT(g))
	    printf( "IRC: blacken_regrayed: %p\n", g );

	  /* since it's being removed from the regrayed list, clear the bit */
	  assert( g->flagBits & IRC_MASK_REGRAYQ );
	  g->flagBits &= ~IRC_MASK_REGRAYQ;

	  if (atracef) {
	    fprintf( atracef, "  [%lu] popping from write barrier: %p\n", 
		     i++, g+1 );
	    fflush( atracef );
	  }
          gen->scanning = g;
	  IRC_clientFindPointers( gen, g+1, gen->traversalWork );
          gen->scanning = NULL;

	  time -= g->sizeClass->itemSize;
	}
      free(b);
      if (time < 0)
	return 0;
    }
  if (atracef)
    fprintf( atracef, "no more regrayed objects...\n" );
  return time;
}


/* this process tries to terminate, which
   involves doing a substantial amount of work
   atomically
   (Q: would it be a useful optimization to totally
   skip the attempt if we don't have enough time to
   make a "reasonable effort".  We would be giving up
   time that we rightfully deserve, but it may improve
   performance)
*/

static INT_32 attempt_termination( struct IRC_Gen *gen, INT_32 time )
{
    /* give ourselves some time to work */
    
    if (time < 1600)
	time = 1600;
    
    /* atomically scan the roots */
    
    init_stable_scan( gen );
    time = run_stable_scan( gen, time );
    if (time <= 0) return 0;

    init_igp_scan( gen );
    time = run_igp_scan( gen, time );
    if (time <= 0) return 0;

    init_quasistable_scan( gen );
    time = run_quasistable_scan( gen, time );
    if (time <= 0) return 0;

    init_unstable_scan( gen );
    time = run_unstable_scan( gen, time );
    if (time <= 0) return 0;

    init_externals_scan( gen );
    run_externals_scan( gen, 10000000 );

    /* do traversal work */
    
    return do_traversal( gen, time );
}

/* used to track the generation being terminated */

static struct IRC_Gen *term_gen = NULL;

int is_object_dead( void *ptr )
{
  IRC_Header *b = IRCH(ptr);

  assert( term_gen );
  if ((b->flagBits & IRC_MASK_COLOR) == term_gen->whiteColorCode)
    return 1;
  else
    return 0;
}


static INT_32 run_gen( struct IRC_Gen *gen, INT_32 time )
{
int gstate = gen->state;

    while (1) 
    {
      if (atracef)
	fprintf( atracef, "generation[%d]: in state %d, time remaining: %ld\n",
		 gen->genNum, gstate, (long)time );
#ifdef DEBUG_TRAVERSAL
#endif /* DEBUG_TRAVERSAL */
	switch (gstate)
	{
	    case GSTATE_IDLE:

              /* note that calling irc_start_pstore_gens() might 
                 alter the write barrier and traversal actions.

                 Hence, we need to do it *before* actually scanning
                 anything!
              */
              irc_start_pstore_gens();

	        startGenCollection( gen );
		return time;
	    case GSTATE_PHASE2:
	    	return time;
	    case GSTATE_STABLE_SCAN:
		time = run_stable_scan( gen, time );
		if (!time) return 0;
		gstate = GSTATE_TRAVERSE_1;
		break;
	    case GSTATE_IGP_SCAN:
		time = run_igp_scan( gen, time );
		if (!time) return 0;
		gstate = GSTATE_TRAVERSE_2;
		break;
	    case GSTATE_QUASISTABLE_SCAN:
		time = run_quasistable_scan( gen, time );
		if (!time) return 0;
		gstate = GSTATE_TRAVERSE_3;
		break;
	    case GSTATE_UNSTABLE_SCAN:
		time = run_unstable_scan( gen, time );
		if (!time) return 0;
		gstate = GSTATE_TRAVERSE_4;
		break;

	      case GSTATE_XHEAP_SCAN:
		time = run_externals_scan( gen, time );
		if (!time) return 0;
		gstate = GSTATE_REBLACKEN;
            case GSTATE_REBLACKEN:
		if (gen->regrayObjects.first)
		  {
		    time = blacken_regrayed( gen, time );
		    if (!time) return 0;
		    gstate = GSTATE_TRAVERSE_5;
		    break;
		  }
		/* fall through into termination */
	    case GSTATE_TERMINATE:

		time = attempt_termination( gen, time );
		if (!time) 
		{
		    if (gen->genNum != 0)
		      gen->state = GSTATE_PHASE2;
		    else
		      gen->state = GSTATE_REBLACKEN;
		    return 0;
		}

		/* shouldn't be anything to regray, here */
		assert( !gen->regrayObjects.first );

		term_gen = gen;
		if (gc_cycle_finish_ok())
		  {
		    term_gen = NULL;
                    irc_flip_pstore_gens();
		    generationFlip( gen );
		    gstate = GSTATE_IDLE;
		  }
		else
		  {
		    term_gen = NULL;
		    return 0;
		  }
		break;
	    case GSTATE_TRAVERSE_1:
		time = do_traversal( gen, time );
		if (!time) return 0;
		init_igp_scan( gen );
		gstate = GSTATE_IGP_SCAN;
		break;
	    case GSTATE_TRAVERSE_2:
		time = do_traversal( gen, time );
		if (!time) return 0;
		init_quasistable_scan( gen );
		gstate = GSTATE_QUASISTABLE_SCAN;
		break;
	    case GSTATE_TRAVERSE_3:
		time = do_traversal( gen, time );
		if (!time) return 0;
		init_unstable_scan( gen );
		gstate = GSTATE_UNSTABLE_SCAN;
		break;
	    case GSTATE_TRAVERSE_4:
		time = do_traversal( gen, time );
		if (!time) return 0;
		if (gen->genNum != 0)
		    gstate = GSTATE_PHASE2;
		else
		  {
		    goto start_xheap;
		  }
		break;
	    case GSTATE_TRAVERSE_5:
		time = do_traversal( gen, time );
		if (!time) return 0;
		goto start_xheap;

	      start_xheap:
		gstate = GSTATE_XHEAP_SCAN;
		init_externals_scan( gen );
		break;
		
	    default:
	    	assert(0);
	}
	gen->state = gstate;
    }
}

UINT_32 IRC_safePoint( IRC_Heap *heap )
{
INT_32 t2, timer, didNothing;
struct IRC_Gen *gen;

    timer = INCREMENT_WORK;
    didNothing = 2 * NUM_GENERATIONS;
    for (gen=&heap->theGenerations[NUM_GENERATIONS]; 
    	 timer > 0 && gen>heap->theGenerations;)
    {
	gen--;
	t2 = run_gen( gen, timer );
	if (t2 != timer)
	{
	    if (--didNothing == 0)
		return INCREMENT_SPACING;
	}
	timer = t2;
    }
#if 0
IRC_fullCollect( heap, 1 );
#endif
    return INCREMENT_SPACING;
}


void IRC_fullCollect( IRC_Heap *heap, unsigned numGens )
{
struct IRC_Gen *gen;

if (atracef)      fprintf( atracef, "-- FULL COLLECT %u\n", numGens );
#ifdef DEBUG_TRAVERSAL
    printf( "************** full collect ***************\n" );
#endif /* DEBUG_TRAVERSAL */
    /* finish current collections */
    
    for (gen=heap->theGenerations; 
    	 gen<&heap->theGenerations[numGens];
	 gen++)
    {
	run_gen( gen, 2000000000 );
    }
    
    /* start a new collection */
    
    for (gen=heap->theGenerations; 
    	 gen<&heap->theGenerations[numGens];
	 gen++)
    {
	/* cheat by starting in a termination state.
	   the atomic scan will take care of root traversal
	   (but we need to flush the regrayed list, too)
	*/
	if (gen->genNum)
	{
#ifdef DEBUG_TRAVERSAL
	    printf( "generation[%u]: in state %d ==> phase II\n", 
		    gen->genNum, gen->state );
#endif /* DEBUG_TRAVERSAL */
	    gen->state = GSTATE_PHASE2;
	}
	else
	{
#ifdef DEBUG_TRAVERSAL
	    printf( "generation[%u]: in state %d ==> terminate\n", 
		    gen->genNum, gen->state );
#endif /* DEBUG_TRAVERSAL */
	    gen->state = GSTATE_REBLACKEN;
	}
    }
 
    /* finish it */
    
    for (gen=heap->theGenerations; 
    	 gen<&heap->theGenerations[numGens];
	 gen++)
    {
	run_gen( gen, 2000000000 );
    }
}


/*  object mapping  */

int IRC_forEachObject( IRC_Heap *heap, int (*fn)( void *info, void *heap_obj ), void *info )
{
  unsigned g, sc;
  IRC_Header *f, *p;
  IRC_SizeClass *szc;
  int rc;

  for (g=0; g<NUM_GENERATIONS; g++) {
    szc = heap->theGenerations[g].theSizeClasses;
    for (sc=0; sc<NUM_PHYSICAL_SIZE_CLASSES; sc++, szc++) {
      f = szc->free;
      for (p=szc->black; p != f; p=p->next) {
	rc = fn( info, p+1 );
	if (rc)
	  return rc;
      }
    }
  }
  return 0;
}

#if WATCHED
void IRC_check_freelist( IRC_Header *p )
{
#if FREE_HAS_DEADBEEF
  if (p->sizeClass->free != &p->sizeClass->marker)
    assert( p->sizeClass->free->flagBits == 0xDEADBEEF );
#endif
}
#endif
