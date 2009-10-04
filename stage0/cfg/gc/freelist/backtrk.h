/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gc/freelist/backtrk.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.4
 * File mod date:    1997-11-29 23:10:47
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#ifndef _H_BACKTRACKING
#define _H_BACKTRACKING

#include "platform.h"

typedef struct {
    UINT_32	gc_ptr;
    UINT_32	offset;
} ptr_location;

typedef ptr_location *ptr_locations_list;

#endif
