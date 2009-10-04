/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/finalize.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.7
 * File mod date:    2003-10-22 12:52:25
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Support for finalization and weak pointers
 *------------------------------------------------------------------------*
 * Notes:
 *      This support is GC-independent, supplying the client (language)
 *      side definitions for `safe_for_object_is_dead' and the internal
 *      function mark_as_finalizable and found_weak_slot
 *------------------------------------------------------------------------*/

#include <stdlib.h>
#include <rscheme/intrs.h>
#include <rscheme/osglue.h>
#include <rscheme/gcserver.h>
#include <rscheme/smemory.h>
#include <rscheme/scheme.h>

struct obj_list {
  obj      *contents;
  unsigned  count;
  unsigned  capacity;
};

static struct obj_list fzable = { NULL, 0, 0 };
static struct obj_list wkslot = { NULL, 0, 0 };


void shrink_list( struct obj_list *lst )
{
}

void add_to_list( struct obj_list *lst, obj item )
{
  if (lst->count >= lst->capacity)
    {
      if (lst->count == 0)
	{
	  lst->capacity = 16;
	  lst->contents = (obj *)malloc( sizeof( obj ) * 16 );
	}
      else
	{
	  lst->capacity *= 2;
	  lst->contents = (obj *)realloc( lst->contents, 
					  sizeof( obj ) * lst->capacity );
	}
    }
  lst->contents[lst->count++] = item;
}

void mark_as_finalizable( obj item )
{
  add_to_list( &fzable, item );
}

static obj current_gc_cycle = ZERO;

obj get_gc_cycle_id( void )
{
  return current_gc_cycle;
}

/* returns YES if the GC can flip now.  If this function returns YES,
 * the GC _will_ flip (we send the GC_FLIP signal to the scheme system)
 */

rs_bool gc_cycle_finish_ok( void )
{
  unsigned i;
  obj fzing_list = NIL_OBJ;
  unsigned fzing_cnt = 0;

  for (i=0; i<fzable.count;)
    {
      if (is_object_dead( PTR_TO_GCPTR(fzable.contents[i])) )
	{
	  fzing_list = cons( fzable.contents[i], fzing_list );
	  fzable.contents[i] = fzable.contents[--fzable.count];
	  fzing_cnt++;
	}
      else
	i++;
    }
  if (!EQ(fzing_list,NIL_OBJ))
    {
      /* signal the scheme system */

      struct RSSIG_info sig;
      sig.signum = RSSIG_FINALIZE;
      sig.data.finalize.finalize_list = fzing_list;
      sig.data.finalize.count = fzing_cnt;

      os_enqueue_sig( &sig );
      return NO;
    }
  else
    {
      /* send the `gc flipped' signal */

      struct RSSIG_info sig;
      sig.signum = RSSIG_GC_FLIP;
      os_enqueue_sig( &sig );

      /* nil out weak pointers */

      if (wkslot.count < (wkslot.capacity / 2))
	shrink_list( &wkslot );

      for (i=0; i<wkslot.count; i++)
	{
	  obj *vp, v;

	  vp = (obj *)OBJ_TO_RAW_PTR(wkslot.contents[i]);
	  v = *vp;
	  if (OBJ_ISA_PTR(v) && is_object_dead(PTR_TO_GCPTR(v)))
	    {
	      *vp = FALSE_OBJ;
	    }
	}
      wkslot.count = 0;
      current_gc_cycle = ADD1( current_gc_cycle );
      return YES;
    }
}

void found_weak_slot( obj in, obj *slot )
{
  add_to_list( &wkslot, RAW_PTR_TO_OBJ(slot) );
}
