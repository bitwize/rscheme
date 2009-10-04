/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gc/freelist/gcxverse.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.4
 * File mod date:    1997-11-29 23:10:47
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#ifdef _H_GCXVERSE
#define _H_GCXVERSE

#ifdef INLINES
#include "gcxverse.ci"
#endif

LINK_TYPE void gc_next_object( pos_ptr_addr p, gc_obj_addr ptr_to_object );

#endif
