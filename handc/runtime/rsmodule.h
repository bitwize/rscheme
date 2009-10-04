/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/rsmodule.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.4
 * File mod date:    1997-11-29 23:10:50
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          the "RScheme" module, standard runtime system
 *------------------------------------------------------------------------*/

/*
 *   Provide access to the RScheme Runtime Module
 *   and it's constituent parts
 *
 */

#ifndef _H_RSMODULE
#define _H_RSMODULE

#include <rscheme/linktype.h>

extern struct part_descr       rscheme_runtime_part;  /* 9501 */
extern struct part_descr       rscheme_hasht_part;    /* 9502 */

extern struct module_descr     module_rscheme;

#endif /* _H_RSMODULE */
