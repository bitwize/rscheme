/*-----------------------------------------------------------------*-C-*---
 * File:    handc/platform/sunos/dynlink.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.8
 * File mod date:    2003-08-20 13:33:26
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          SunOS dynamic linking interface
 *------------------------------------------------------------------------*/

#include <stdio.h>
#include <rscheme/platform.h>  /* find out if we HAVE_LIBDL */
#include "dynlink.h"

#ifdef HAVE_LIBDL
#include <dlfcn.h>
#endif

void *resolve_link_symbol( void *info, const char *sym )
{
#if HAVE_LIBDL
    return dlsym( info, sym );
#else
    return NULL;
#endif
}

void *dynamic_link_file( const char *path )
{
#if HAVE_LIBDL
    return dlopen( path, 1 );
#else
    return NULL;
#endif
}

const char *dynamic_link_errors( void )
{
#if HAVE_LIBDL
  return dlerror();
#else
  return NULL;
#endif
}

void done_resolving( void *info )
{
}

void init_dynamic_link( const char *argv0 )
{
}


