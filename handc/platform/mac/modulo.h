/*-----------------------------------------------------------------*-C-*---
 * File:    handc/platform/mac/modulo.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.3
 * File mod date:    1997-11-29 23:10:47
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#define REMDR(x,y) ((x)%(y))
#define MOD(x,y) (((x)%(y))+((x)<0?((y)<0?0:y):((y)<0?y:0)))
