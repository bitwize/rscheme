/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gc/irc/gcalloc.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.9
 * File mod date:    1998-12-05 16:11:58
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          IRC actual allocator
 *------------------------------------------------------------------------*/

#include "irc.h"
#ifndef INLINES
#include "gcserver1.ci"
#include "gcserver2.ci"
#endif

/*  first is a list of new blocks to be inserted
    
    let X be the object at the old marker's PREV
    
    it's last element's NEXT points to the marker
    the marker's PREV points to the last element
    
    it's first element's PREV points to X
    X's NEXT points to the first element
    
    and internally the elements are doubly linked

   /--- white
  v
  +-----+      +-----+
  |  X  |      |MARK |
  +-----+     /+-----+
    ^         |
   /          \
  |            ----------------\
  |                             v
  \ +-----+      +-----+      +-----+
   \|     |<---->|     |<---->|     |
    +-----+      +-----+      +-----+
    ^
    |
  first
    
  This function update's the SizeClass's 4 pointers
  to include the new list, with the first element at
  the end of the white list and the second element
  at the start of the free list (there need not be
  a second element -- it could be the marker)
  
  Returns the first element (the new tail of the white list)
*/

static IRC_Header *insertNewBlocks( IRC_SizeClass *sc, IRC_Header *first )
{
IRC_Header *m = &sc->marker;

    /* put the second element etc. onto the free list */

    sc->free = first->next;
    
    /* fix up the sublist pointers.
       Everything that used to point to the marker
       now needs to point to the new object */
    
    if (sc->white == m)
    {
	sc->white = first;
	if (sc->gray == m)
	{
	    sc->gray = first;
	    if (sc->black == m)
	    {
		sc->black = first;
	    }
	}
    }
    return first;
}

IRC_Header *IRC_growSizeClass( IRC_Heap *heap,
			       IRC_SizeClass *sc,
			       UINT_32 size )
{
UINT_32 itemSize = sc->itemSize;
UINT_32 chunkSize = sc->chunkSize;
IRC_Header *b, *nxt, *prv, *p;
IRC_Header *m = &sc->marker;

    assert( sc->free == m );

    /* special case large objects */

    if (sc->isLargeObject)
    {
      p = heap->alloc_big_meth( heap, size );
      prv = m->prev;

      p->next = m;
      p->prev = prv;
      
      prv->next = p;
      m->prev = p;
#if FREE_HAS_DEADBEEF
      p->flagBits = 0xDEADBEEF;
#endif
      return insertNewBlocks( sc, p );
    }
    else if (heap->spaceLeft < chunkSize)
    {
	if (heap->spaceLeft >= itemSize)
	{
	unsigned n;

	    /* fallback -- fill in the rest with as many as we can */
	    n = heap->spaceLeft / itemSize;
	    chunkSize = n * itemSize;
	}
	else
	{
	  heap->alloc_chunk_meth( heap );
	}
    }
    b = (IRC_Header *)heap->moreSpacePtr;
    heap->moreSpacePtr = (void *)((char *)heap->moreSpacePtr + chunkSize);
    heap->spaceLeft -= chunkSize;

    nxt = m;
    prv = nxt->prev;
    prv->next = b;
    b->prev = prv;
    p = (IRC_Header *)heap->moreSpacePtr;
    while (p > b)
    {
	p = (IRC_Header *)((char *)p - itemSize);
	p->next = nxt;
	p->sizeClass = sc; /* things in FREE set don't need sizeClass
			      -- unless you're debuggin' em */ 
#if FREE_HAS_DEADBEEF
	p->flagBits = 0xDEADBEEF;
#endif
	nxt->prev = p;
	nxt = p;
    }
    assert( p == b );
    return insertNewBlocks( sc, b );
}

#ifndef GC_MACROS
void *IRC_alloc( IRC_Heap *heap, UINT_32 size )
{
int i = LOGICAL_SIZE_CLASS_OF(size);
IRC_SizeClass *sc;
void *p;

    sc = heap->sizeClassesIndex[i];
    p = IRC_getBlock( heap, sc, size );
/*    printf( "alloc(%u) => %08x\n", size, p ); */
    return p;
}
#endif
