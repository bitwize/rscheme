/*-----------------------------------------------------------------*-C-*---
 * File:    handc/unstub/unstub.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.6
 * File mod date:    1997-11-29 23:10:43
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#include <rscheme/scheme.h>
#include <rscheme/linktype.h>
#include <rscheme/smemory.h>

jump_addr template_unstub( obj the_template )
{
  /* no stubbing, so it's already unstubbed */
  return OBJ_TO_JUMP_ADDR( gvec_ref( the_template, SLOT(0) ) );
}
