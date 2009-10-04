/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gc/irc/writebar.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.16
 * File mod date:    2005-09-16 11:07:52
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          IRC write barrier implementation
 *------------------------------------------------------------------------*/

#include "irc.h"
#include "alloc.h"

#ifdef INLINES
#include <rscheme/gcserver1.ci>
#include <rscheme/gcclient1.ci>
#endif

static void store_into_pstore( IRC_Heap *heap, 
			      void *lvalue, UINT_32 offset, 
			      void *rvalue );

/*
 *  The write barrier has been tripped.  
 *
 *  We are called *before* the write takes place, which means the
 *  value being clobbered is still present at lvalue[offset]
 */


int IRC_tripWriteBarrier( IRC_Heap *heap, 
		          void *lvalue, UINT_32 offset, 
		          void *rvalue )
{
  if (IRCH_WRITEPROT(lvalue)) {
    IRC_clientWriteViolation( heap, lvalue, offset );
    return 1;
  } else {
    char wbcode;
      
    wbcode = IRC_writeBarrierCode( heap, lvalue, rvalue );
    if (atracef) {
      fprintf( atracef, 
               "write barrier trip (%d): lvalue = %p (offset +%lu), "
               "rvalue = %p\n", 
               wbcode,
               lvalue, (unsigned long)offset, rvalue );
      fflush( atracef );
    }
    switch (wbcode) {
    case WB_NONE:
      /* why were we tripped? */
      break;
    case WB_COLOR:
      {
        struct IRC_Header *l = IRCH(lvalue);

        /* (note that we can't set the color
           bit in the lvalue back to WHITE, because
           then during traversal, we would crawl
           over it again (snapping it out of the
           black list an into the gray list)
           (is that the only reason?)
        */
        /* write of a pointer to a white object
           into a black (or gray) object
        */
        /* regray the lvalue */
        /* but only if it's not already being regrayed */
        if (!(l->flagBits & IRC_MASK_REGRAYQ)) {
          if (atracef) {
            fprintf(atracef,
                    "  -- %p added to regray list\n", l);
            fflush(atracef);
          }
          IRC_ptrListAdd( &l->sizeClass->gen->regrayObjects, l );
          l->flagBits |= IRC_MASK_REGRAYQ;
        } else {
          if (atracef) {
            fprintf(atracef,
                    "  -- %p already in regrayed list\n", 
                    IRCH(lvalue));
            fflush(atracef);
          }
        }
      }
      return 1;
    case WB_GENERATION:
      {
        struct IRC_Header *r = IRCH(rvalue);
        
        /* write of a pointer to a younger object
           into an older object
        */
        /* put rvalue into the IGP list */
        RS_LVerbose( 108, 7320, "WB_GENERATION L=%p[%u] R=%p", lvalue, offset, rvalue );
        if (r->sizeClass->gen->igp_hook) {
          r->sizeClass->gen->igp_hook( r->sizeClass->gen->igp_hook_info,
                                       lvalue, offset, rvalue );
        }
      }
      return 1;
    case WB_GLOBAL:
      /* write into a global shared object */
      return 1;
    case WB_PERSISTENT:
      /* a store into a persistent object --
         record lptr as an "external possptr",
         but only if we're not overwriting a 
         pointer into the same generation */
      RS_LVerbose( 108, 7340, "WB_PERSISTENT L=%p[%u] R=%p", lvalue, offset, rvalue );
      store_into_pstore( heap, lvalue, offset, rvalue );
      return 1;
    }
  }
  return 0;
}

#ifndef GC_MACROS
int IRC_writeBarrier( IRC_Heap *heap, 
		      void *lvalue, UINT_32 offset, 
		      void *rvalue )
{
    if (IRC_writeBarrierCode( heap, lvalue, rvalue ))
	return IRC_tripWriteBarrier( heap, lvalue, offset, rvalue );
    return 0;
}
#endif

static void *IRC_pstoreWriteTrapInfo;
static int (*IRC_pstoreWriteTrapFn)( void *info,
                                     IRC_Heap *heap,
                                     void *lvalue, UINT_32 offset, 
                                     void *rvalue ) = NULL;

static void heap_set_wb( IRC_Heap *heap,
                         unsigned char lvalue_gen,
                         unsigned char lvalue_color,
                         unsigned char rvalue_gen,
                         unsigned char rvalue_color,
                         unsigned char wb_code )
{
  assert( (wb_code <= WB_PERSISTENT) );
  assert( (lvalue_color == 0) || (lvalue_color == 1) );
  assert( (lvalue_gen <= 7) );
  assert( (rvalue_color == 0) || (rvalue_color == 1) );
  assert( (rvalue_gen <= 7) );

  heap->writeBarrierTable[ (lvalue_gen * 2 + lvalue_color) * 16
                           + (rvalue_gen * 2 + rvalue_color) ] = wb_code;
}
                         
void irc_init_pstore_writebarrier( void *info,
                                   int (*trapfn)( void *info,
                                                  IRC_Heap *heap,
                                                  void *lvalue, 
                                                  UINT_32 offset, 
                                                  void *rvalue ) )
{
  unsigned char gen;
  
  /*  detect mutations that create pointers from persistent
   *  objects to transient objects
   */

  heap_set_wb( gc_arena, 7, 0, 0, 0, WB_PERSISTENT );
  heap_set_wb( gc_arena, 7, 0, 0, 1, WB_PERSISTENT );
  heap_set_wb( gc_arena, 7, 1, 0, 0, WB_PERSISTENT );
  heap_set_wb( gc_arena, 7, 1, 0, 1, WB_PERSISTENT );
  
  IRC_pstoreWriteTrapInfo = info;
  IRC_pstoreWriteTrapFn = trapfn;
}

void irc_config_pstore_tracking( int level )
{
  unsigned char gen;
  struct IRC_Gen *g;

  assert( NUM_GENERATIONS == 1 );
  gen = 0;

  g = &gc_arena->theGenerations[ gen ];

  /*
   *  Note that we also need to configure WB_GENERATION for
   *  any store operation both sides of which are
   *  WHITE GEN=7 objects.  Otherwise, the application
   *  could, after the commit, stuff the only pointer to
   *  a truly live persistent object into a persistent 
   *  
   *  We needn't worry if the lvalue is a WHITE GEN=0 object
   *  because the TSCAN phase will catch up with it.  If the lvalue
   *  is GRAY GEN=7, then persistent traversal will find it.
   */

  if (level >= 1) {
    heap_set_wb( gc_arena, 7,   0,                 7, 0, WB_GENERATION );
    heap_set_wb( gc_arena, 7,   1,                 7, 0, WB_GENERATION );
  } else {
    heap_set_wb( gc_arena, 7,   0,                 7, 0, WB_NONE );
    heap_set_wb( gc_arena, 7,   1,                 7, 0, WB_NONE );
  }

  if (level == 2) {
    heap_set_wb( gc_arena, gen, g->blackColorCode, 7, 0, WB_GENERATION );
  } else {
    heap_set_wb( gc_arena, gen, g->blackColorCode, 7, 0, WB_NONE );
  }
}

static void store_into_pstore( IRC_Heap *heap, 
			       void *lvalue, UINT_32 offset, 
			       void *rvalue )
{
  if (IRC_pstoreWriteTrapFn( IRC_pstoreWriteTrapInfo,
                             heap,
                             lvalue, offset,
                             rvalue )) {
    struct IRC_Gen *rgen = IRCH(rvalue)->sizeClass->gen;
    pos_ptr_addr pp;

    pp = (pos_ptr_addr) (offset + (char *)lvalue);
    IRC_ptrListAdd( &rgen->extraHeapPointers, (IRC_Header *)pp );
  }
}
