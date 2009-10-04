/*-----------------------------------------------------------------*-C-*---
 * File:    handc/rshell/shell.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.10
 * File mod date:    1997-11-29 23:10:43
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Trivial "shell" application
 *------------------------------------------------------------------------*/

#include <stdlib.h>
#include <stdio.h>
#include <rscheme/api.h>
#include <rscheme/stdmodul.h>
#include <rscheme/rlseconf.h>

#ifdef PLATFORM_MAC_CODEWARRIOR
#include <stdio.h>
#include <sioux.h>
#endif

struct module_descr *(std_modules[]) = { STD_MODULES_DECL };

int main( int argc, const char **argv )
{
#ifdef PLATFORM_MAC_CODEWARRIOR
  /* configure the I/O window */
  SIOUXSettings.autocloseonquit = TRUE;
  SIOUXSettings.asktosaveonclose = TRUE;
#endif

  rs_install_dir = getenv( "RS_INSTALL_DIR" );
  if (!rs_install_dir)
    rs_install_dir = INSTALL_DIR;

#ifdef PLATFORM_MAC
  return rscheme_std_main( argc, argv, std_modules, "install/system.img" );
#else
  {
    char temp[1024];

    sprintf( temp, "%s/resource/system.img", rs_install_dir );
    return rscheme_std_main( argc, argv, std_modules, temp );
  }
#endif
}
