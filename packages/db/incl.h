/*-----------------------------------------------------------------*-C-*---
 * File:	    packages/db/incl.h
 *
 *          Copyright (C)1998 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest info.
 *
 * File version:     1.3
 * File mod date:    2003-01-05 11:52:38
 * System build:     v0.7.3.4-b7u, 2007-05-30
 * Owned by module:  &module;
 *
 * Purpose:          &purpose;
 *------------------------------------------------------------------------*/

#include <rscheme/platform.h>
#include <sys/types.h>
#include <limits.h>

#if HAVE_DB3_DB_185_H
#include <db3/db_185.h>
#else
#if HAVE_DB_DB_H
#include <db/db.h>
#else
#include <db.h>
#endif
#endif

#include <fcntl.h>
#include <string.h>
