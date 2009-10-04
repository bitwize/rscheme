/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/rdln/std/rdln_std.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.6
 * File mod date:    1997-11-29 23:10:45
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          interface to stdio to serve role of readline library
 *------------------------------------------------------------------------*/

#include <rscheme/obj.h>
#include <rscheme/rdln.h>

rs_bool rdln_isa_tty( void )
{
  return YES;
}

rs_bool rdln_enabled( void )
{
  return NO;
}

void rdln_add_history( obj str )
{
}

obj read_console_line( obj completions, const char *prompt )
{
  return ZERO;
}
