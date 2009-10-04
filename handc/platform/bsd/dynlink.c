/*-----------------------------------------------------------------*-C-*---
 * File:    handc/platform/bsd/dynlink.c
 *
 *          Contributed by HIROSHI OOTA <oota@POBoxes.com>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest info.
 *
 * File version:     1.3
 * File mod date:    2003-08-20 13:33:25
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          FreeBSD dynamic linking interface
 *------------------------------------------------------------------------*/

#include <stdio.h>
#include <dlfcn.h>

#ifndef	RTLD_NOW
#define	RTLD_NOW	DL_LAZY
#endif
void *resolve_link_symbol( void *info, const char *sym )
{
  return dlsym( info, sym );
}

void *dynamic_link_file( const char *path )
{
  return dlopen( path, RTLD_NOW );
}

const char *dynamic_link_errors( void )
{
  return dlerror();
}

void done_resolving( void *info )
{
}

void init_dynamic_link( const char *argv0 )
{
}


