/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gcadapt/freelist/gcclient.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.6
 * File mod date:    1997-11-29 23:10:45
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          freelist GC adapater (scheme -> adapter -> gc)
 *------------------------------------------------------------------------*/

#include <rscheme/vinsns.h>
#include <rscheme/gcserver.h>
#include <rscheme/linktype.h>
#include <rscheme/runtime.h>
#include <rscheme/smemory.h>
#include <stdio.h>
#include <rscheme/gcstruct.h>

extern MEMSizeClass size_classes[NUM_SIZE_CLASSES];
extern MEMSizeClass other_size_class;

struct module_descr **stable_root_module_ptr;
unsigned stable_root_num;
unsigned quasistable_root_num;

#ifndef INLINES
#include "gcclient1.ci"
#include "gcclient2.ci"
#endif

#ifdef VALIDATE_BLOCKS
#ifdef gcserver_h	/* see if we are using the rtgc */

void validate_block( gc_obj_addr x )
{
}
#endif
#endif

#if 0  /* when the RTGC is fixed */
void gc_ready_to_terminate( unsigned gen )
{
    printf( "gc_ready_to_terminate(%u)\n", gen );
    gc_terminate_generation( gen );
}
#endif

static int gc_for_each_in_sc( int (*fn)( void *info, void *heap_obj ), 
			      void *info, 
			      MEMSizeClass *sc )
{
MEMHeader *block;
int rc;

    block = sc->alloced;
    while (block)
      {
	rc = fn( info, (void *)(block+1) );
	if (rc)
	  return rc;
	block = block->next;
      }
    return 0;
}

int gc_for_each( int (*fn)( void *info, void *heap_obj ), void *info )
{
int i, rc;

    for (i=0; i<NUM_SIZE_CLASSES; i++)
      {
	rc = gc_for_each_in_sc( fn, info, &size_classes[i] );
	if (rc)
	  return rc;
      }
    return gc_for_each_in_sc( fn, info, &other_size_class );
}

void gc_now( void )
{
   gc_full_collect();
}
