/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gc/irc/writebar.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.5
 * File mod date:    1997-11-29 23:10:46
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#define WB_NONE  (0)		/* write is safe */
#define WB_COLOR (1)    	/* could violate color invariant */
#define WB_GENERATION (2)	/* could violate generation invariant */
#define WB_GLOBAL (3)		/* writing into a global object */
#define WB_PERSISTENT (4)       /* writing into a persistent object */
