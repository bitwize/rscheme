/*-----------------------------------------------------------------*-C-*---
 * File:    handc/heapi/heapi.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.6
 * File mod date:    1997-11-29 23:10:44
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#ifndef _H_HEAPI
#define _H_HEAPI

#include <rscheme/obj.h>
#include <stdio.h>

void save_image( FILE *f, obj entry, obj refs, obj opt );
obj load_image( FILE *f, obj link_table );

obj load_image_file( const char *path, obj link_table, obj opts, 
		     int *fmt_vers );
void save_image_file( const char *path, obj entry, obj refs, obj opt );

/* possible image file format versions */

#define FMTV_RSCHEME_0_5       (50)
#define FMTV_RSCHEME_0_6       (60)
#define FMTV_RSCHEME_0_6_BOOT  (61)

#endif /* _H_HEAPI */
