/**********************************************
THIS FILE WAS AUTOMATICALLY COPIED FROM THE
RSCHEME SOURCE TREE, AND THE ORIGINAL MAY CHANGE.
HENCE, DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#line 1 "modules/regex/rxmach.h"
/*-----------------------------------------------------------------*-C-*---
 * File:    modules/regex/rxmach.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.6
 * File mod date:    2004-07-02 14:47:37
 * System build:     v0.7.3.4-b7u, 2007-05-30
 * Owned by module:  regex
 *
 * Purpose:          Define constants and declare functions for regex machine
 *------------------------------------------------------------------------*/

#ifndef _H_RXMACH
#define _H_RXMACH
				/* amount of data */
#define RXM_MATCH1       (1)	/* data = 1 byte */
#define RXM_MATCH_ANY    (2)    /* no data */
#define RXM_MATCH_SET    (3)    /* data = 32 bytes */

#define RXM_MATCHN       (4)	/* data = n+1 bytes */
#define RXM_ACCEPT       (5)    /* no data */
#define RXM_BRANCH       (6)    /* data = 2 bytes */
#define RXM_SAVE_PLACE   (7)    /* data = 1 byte register # */

#define RXM_MATCH_STAR   (9)    /* data = 2 byte offset to sub-machine */
#define RXM_MATCH_PLUS  (10)    /* data = 2 byte offset to sub-machine */

#define RXM_STR_END     (11)    /* no data */
#define RXM_STR_START   (12)    /* no data */

#define RXM_REJECT      (13)    /* no data */
#define RXM_JUMP        (14)    /* data = 2 byte offset */

#define RXM_INC         (15)    /* data = 1 byte count register # */
#define RXM_BOUNDLOOP   (16)    /* data = 1 byte count register #,
                                          2 byte offset to sub-machine */
#define RXM_SETBOUND    (17)    /* data = 1 byte count register #,
                                          2 byte min count,
                                          2 byte max count */
UINT_8 *run_match( UINT_8 *str, UINT_32 pc );

struct RXCounter {
  UINT_16   min, max, count;
};

extern struct RXCounter *rxmach_bound;

extern UINT_8 **rxmach_save_array;
extern UINT_8 *rxmach_machine;
extern UINT_8 *rxmach_start, *rxmach_limit;
extern UINT_8 *rxmach_on_eos;

#define RXERR_INT_BAD_OPCODE  (100)
#define RXERR_INT_BAD_KLEENE  (101)
#define RXERR_INT_INCOMPLETE_LET (102)

void rxmach_internal_error( int code );

#endif /* _H_RXMACH */
