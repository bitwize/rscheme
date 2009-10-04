/*-----------------------------------------------------------------*-C-*---
 * File:    handc/lgh/lgh.h
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

#ifndef _H_LGH
#define _H_LGH

#include <rscheme/scheme.h>

void lgh_eval( char *str );
void lgh_startup( int packages );

#endif /* _H_LGH */
