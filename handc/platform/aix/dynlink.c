/*-----------------------------------------------------------------*-C-*---
 * File:    handc/platform/aix/dynlink.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.7
 * File mod date:    2003-10-13 13:01:59
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          AIX dynamic linking interface
 *------------------------------------------------------------------------*/

#include <stdio.h>
#include <dlfcn.h>

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


