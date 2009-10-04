/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gc/freelist/gcstruct.h
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

#ifndef _H_GCSTRUCT
#define _H_GCSTRUCT

struct _MEMSizeClass;
struct _MEMHeader;

typedef struct _MEMSizeClass {
    struct _MEMHeader *free;
    struct _MEMHeader *alloced;
    unsigned		item_size;
    unsigned		alloc_num;
} MEMSizeClass;

typedef struct _MEMHeader {
    struct _MEMHeader	*next;
    unsigned		black_flag;
#ifdef DEBUG_BACKTRACK
    unsigned	        actual_size;
    unsigned	        extra_flag;
#endif /* DEBUG_BACKTRACK */
} MEMHeader;

#define BYTES_PER_SIZE_CLASS (16)
#define NUM_SIZE_CLASSES (32)


#endif /* _H_GCSTRUCT */
