/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/dynlink.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.7
 * File mod date:    2003-08-20 13:33:26
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          interface to dynamic linking feature
 *------------------------------------------------------------------------*/

/*
 *  dynamic linking is an optional feature,
 *  but the functions are always present (they
 *  may be stubbed out)
 */

void init_dynamic_link( const char *argv0 );
void done_resolving( void *info );
void *dynamic_link_file( const char *path );
void *resolve_link_symbol( void *info, const char *sym );
const char *dynamic_link_errors( void );
