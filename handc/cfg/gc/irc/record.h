/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gc/irc/record.h
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

#define NUM_GENS	(3)
#define NUM_SIZES	(10)
#define NUM_COLORS	(4)

typedef struct _DataRecord {
    unsigned short count[NUM_GENS][NUM_SIZES][NUM_COLORS];
} DataRecord;
