/*-----------------------------------------------------------------*-C-*---
 * File:    handc/platform/irix/dynlink.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.5
 * File mod date:    2003-08-20 13:33:25
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          IRIX dynamic linking interface
 *------------------------------------------------------------------------*/

#include <stdio.h>
#include <dlfcn.h>
#include <rscheme/linktype.h>

void *resolve_link_symbol( void *dlhandle, const char *sym )
{
  void *fct;

  fct = dlsym(dlhandle, sym);
  
  if (fct == NULL) {
    /* couldn't find symbol */
    return NULL;
  } else
    return fct;
  
}

void *dynamic_link_file( const char *path )
{
  void *dlhandle;

  dlhandle = dlopen(path, RTLD_LAZY);

  if (dlhandle == NULL) {
    /* couldn't open DSO */
    return NULL;
  } else
    return dlhandle;

}

void done_resolving( void *dlhandle )
{
  dlclose(dlhandle);
}

void init_dynamic_link( const char *argv0 )
{
}

const char *dynamic_link_errors( void )
{
  return NULL;
}
