/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gcadapt/irc/gcclient.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.7
 * File mod date:    2003-10-14 13:29:15
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          IRC GC adapater (scheme -> adapter -> gc)
 *------------------------------------------------------------------------*/

#include <rscheme/clientyp.h>
#include <rscheme/traverse.h>

#include <rscheme/vinsns.h>
#include <rscheme/gcserver.h>
#include <rscheme/linktype.h>
#include <rscheme/runtime.h>
#include <rscheme/smemory.h>
#include <rscheme/gcconfig.h>
#include <stdio.h>

IRC_Heap *gc_arena;
INT_32 gc_alloc_time;

#ifndef INLINES
#include "gcclient1.ci"
#endif
#include "gcclient2.ci"

void IRC_clientWriteViolation( IRC_Heap *heap, void *item, UINT_32 offset )
{
  scheme_error( "~s: write protected", 1, OBJ(((UINT_32)item)+POINTER_TAG) );
}

INT_32 left = 0;

void init_gc( int argc, const char **argv )
{
  gc_arena = IRC_newHeap();
  gc_alloc_time = 500000;
}

int gc_for_each( int (*fn)( void *info, void *heap_obj ), void *info )
{
  return IRC_forEachObject( gc_arena, fn, info );
}

void gc_now( void )
{
   IRC_fullCollect( gc_arena, NUM_GENERATIONS );
}

/*
 *  Handle the non-inline cases
 */    

static void gc_scan_first_slot_weak( obj item, 
                                     void *ircInfo, 
                                     UINT_8 *travActions )
{
  obj *p, *limit;
  p = (obj *)PTR_TO_DATAPTR(item);

  limit = (obj *)((char *)p + SIZEOF_PTR(item));
  if (p < limit)
    {
      found_weak_slot( item, p++ );
    }
  while (p<limit)
    {
      next_obj( ircInfo, travActions, p++ );
    }
}

struct _gc_scan_ctx {
  void   *ircInfo;
  UINT_8 *travActions;
  unsigned count, ptrs;
};

/*
 *  Note that this scan protocol doesn't allow the
 *  GC process to implement the INCLUDE_READ_BARRIER
 *  and INCLUDE_PSTORE_UNMAPPER features.
 */

static void gc_found( void *info, obj item )
{
  struct _gc_scan_ctx *ctx = (struct _gc_scan_ctx *)info;

  ctx->count++;
  if (OBJ_ISA_PTR( item )) {
    ctx->ptrs++;
    next_ptr( ctx->ircInfo, ctx->travActions, item );
  }
}

void gc_find_pointers_hook( obj item,
                            void *ircInfo, 
                            UINT_8 *travActions, 
                            obj type )
{
  int heap_type = fx2int( type );
  
  switch (heap_type) {
  case 4:
    gc_scan_first_slot_weak( item, ircInfo, travActions );
    break;
  default:
    {
      struct _gc_scan_ctx ctx;

      assert( (heap_type >= 0) && (heap_type < MAX_HEAP_TYPES) );
      assert( rs_heap_type_info[ heap_type ] );

      /*
      printf( "scan #[<%s> %08x]\n", 
              rs_heap_type_info[ heap_type ]->name,
              (unsigned)VAL(item) );
      */
      ctx.ircInfo = ircInfo;
      ctx.travActions = travActions;
      ctx.count = 0;
      ctx.ptrs = 0;
      rs_heap_type_info[ heap_type ]->gc_scan( item, gc_found, &ctx );
      /*printf( "    %d slots, %d pointers\n", ctx.count, ctx.ptrs );*/
    }
    break;
  }
}

