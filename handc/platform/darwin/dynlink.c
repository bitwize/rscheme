/*-----------------------------------------------------------------*-C-*---
 * File:    handc/platform/com/dynlink.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.1
 * File mod date:    2005-01-20 21:08:31
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          stub dynamic linking interface
 *------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <mach-o/dyld.h>
#include <alloca.h>
#include <rscheme/linktype.h>

typedef struct export_table *module_init_fn( void );

void *resolve_link_symbol( void *info, const char *name )
{
  char *temp;
  NSModule mp = (NSModule)info;
  void *addr;
  NSSymbol sym;

  temp = alloca( strlen( name ) + 2 );
  sprintf( temp, "_%s", name );

  sym = NSLookupSymbolInModule( mp, temp );
  if (!sym) {
    return NULL;
  }

  addr = NSAddressOfSymbol( sym );

  return addr;
}

void *dynamic_link_file( const char *path )
{
  NSObjectFileImageReturnCode irc;
  NSObjectFileImage mo;
  NSModule mp;

  irc = NSCreateObjectFileImageFromFile( path, &mo );

  if (irc != NSObjectFileImageSuccess) {
    return NULL;
  }

  mp = NSLinkModule( mo, "slave.so", NSLINKMODULE_OPTION_BINDNOW );

  return mp;
}

const char *dynamic_link_errors( void )
{
  return NULL;
}

void done_resolving( void *info )
{
}

void init_dynamic_link( const char *argv0 )
{
}
