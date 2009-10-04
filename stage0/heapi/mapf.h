/*-----------------------------------------------------------------*-C-*---
 * File:    handc/heapi/mapf.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.4
 * File mod date:    1997-11-29 23:10:44
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#ifndef _H_MAPF
#define _H_MAPF

#include <rscheme/obj.h>

rs_bool mapf_open( const char *path );
void mapf_seek( UINT_32 offset );
void *mapf_read( UINT_32 bytes );
void mapf_close( void );

#endif /* _H_MAPF */

