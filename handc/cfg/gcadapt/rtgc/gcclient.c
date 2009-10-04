/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gcadapt/rtgc/gcclient.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.12
 * File mod date:    1998-10-15 16:36:41
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          rtgc GC adapater (scheme -> adapter -> gc)
 *------------------------------------------------------------------------*/

#include <stdio.h>
#include <rscheme/vinsns.h>
#include <rscheme/gcserver.h>
#include <rscheme/linktype.h>
#include <rscheme/runtime.h>
#include <rscheme/smemory.h>
#include <rscheme/osglue.h>
#include <rscheme/intrs.h>

struct module_descr **stable_root_module_ptr;
unsigned stable_root_num;
unsigned quasistable_root_num;
struct unit_root_iterator uri;

#ifndef INLINES
#include "gcclient1.ci"
#include "gcclient2.ci"
#endif

void gc_now( void )
{
  gc_full_collect(); /* finish current cycle */
  gc_full_collect(); /* do a complete 'nother one */
}

int gc_for_each( int (*fn)( void *info, void *heap_obj ), void *info )
{
  gc_obj_addr p;
  int rc;

  reset_not_known_free_object_iterator();

  while ((p = next_not_known_free_object()))
    {
      rc = fn( info, p );
      if (rc)
	return rc;
    }
  return 0;
}

bool safe_for_is_object_dead( void )
{
  return gc_cycle_finish_ok() ? true : false;
}

void rs_unstable_root_reset( void )
{
#ifdef STACK_CACHE
    cache_iterator_reset();
#endif
}

gc_obj_addr rs_unstable_root_next( void )
{
#ifdef STACK_CACHE
gc_obj_addr cache_iterator_next( void );

    return cache_iterator_next();
#else
    return NULL;
#endif
}

