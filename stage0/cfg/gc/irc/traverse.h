/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gc/irc/traverse.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.8
 * File mod date:    2003-10-14 13:29:05
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#ifndef _H_TRAVERSE
#define _H_TRAVERSE

#include <rscheme/clientyp.h>

/* IRC traversal protocol */

#define IRC_TRAV(travActions,ptr) (travActions[IRCH(ptr)->flagBits & 0xF])

CIH_DECL void IRC_clientFindPointers( void *ircInfo, void *item, UINT_8 *travActions );
int IRC_foundPointer( void *ircInfo, void *ptr );

CIH_DECL void IRC_clientStableRootIteratorInit( IRC_clientStableRootIterator *i );
CIH_DECL void *IRC_clientStableRootIteratorNext( IRC_clientStableRootIterator *i );
CIH_DECL void IRC_clientStableRootWasUnmapped( IRC_clientStableRootIterator *i );

CIH_DECL void IRC_clientQuasistableRootIteratorInit( 
		IRC_clientQuasistableRootIterator *i );
CIH_DECL void *IRC_clientQuasistableRootIteratorNext( 
		IRC_clientQuasistableRootIterator *i );
CIH_DECL void IRC_clientQuasistableRootWasUnmapped( IRC_clientQuasistableRootIterator *i );

CIH_DECL void IRC_clientUnstableRootIteratorInit( IRC_clientUnstableRootIterator *i );
CIH_DECL void *IRC_clientUnstableRootIteratorNext( IRC_clientUnstableRootIterator *i );
CIH_DECL void IRC_clientUnstableRootWasUnmapped( IRC_clientUnstableRootIterator *i );

#endif /* _H_TRAVERSE */
