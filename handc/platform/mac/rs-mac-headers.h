/*-----------------------------------------------------------------*-C-*---
 * File:    handc/platform/mac/rs-mac-headers.h
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

#include <string.h>
#include <ConditionalMacros.h>

#define JCG_FIX 1

/*
#define DEBUG_0 1
#define STEP_DUMP 1
#define DEBUG_BCI 1
#define TYPECHECK_EVAL_STACK 1
*/

#ifndef TRUE
#define FALSE 0
#define TRUE !FALSE
#endif
