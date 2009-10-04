/*-----------------------------------------------------------------*-C-*---
 * File:    handc/lgh/learn0.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.2
 * File mod date:    1997-11-29 23:10:51
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#include "lgh.h"

int main( int argc, const char **argv )
{
  lgh_startup(0);
  lgh_eval( "(display \"Hello, RScheme-LGH\\n\")" );
  return 0;
}
