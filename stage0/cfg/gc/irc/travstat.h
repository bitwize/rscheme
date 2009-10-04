/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gc/irc/travstat.h
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

#define GSTATE_IDLE		(0)
#define GSTATE_STABLE_SCAN	(1)
#define GSTATE_TRAVERSE_1	(2)
#define GSTATE_IGP_SCAN		(3)
#define GSTATE_TRAVERSE_2	(4)
#define GSTATE_QUASISTABLE_SCAN	(5)
#define GSTATE_TRAVERSE_3	(6)
#define GSTATE_UNSTABLE_SCAN	(7)
#define GSTATE_TRAVERSE_4	(8)
#define GSTATE_PHASE2		(9)
#define GSTATE_TRAVERSE_5	(10)


#define GSTATE_TERMINATE	(11)

#define GSTATE_REBLACKEN        (12)
#define GSTATE_XHEAP_SCAN       (13)
#define GSTATE_TRAVERSE_6       (14)
