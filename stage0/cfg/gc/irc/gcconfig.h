/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gc/irc/gcconfig.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.4
 * File mod date:    1997-11-29 23:10:46
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#ifndef _H_CONFIG
#define _H_CONFIG

#include <rscheme/sizeclas.h>

#define NUM_GENERATIONS  (1)

#define NUM_STEPS        (1)

/* in the current logical->physical mapping,
   only [0,1] are squished together
*/

#define NUM_PHYSICAL_SIZE_CLASSES (NUM_LOGICAL_SIZE_CLASSES-1)

/* how fast to grab memory (per heap) */

#define ALLOCATION_CHUNK_SIZE (64*1024)

/* how often to do some work */

#define INCREMENT_SPACING  (20000)

/* how much work to do */

#define INCREMENT_WORK     (30000)

#endif /* _H_CONFIG */
