/*-----------------------------------------------------------------*-C-*---
 * File:    pg/gettuple.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rosette.com>
 *          as part of the RScheme project, licensed for free use
 *
 * Version: 1.4
 * Date:    2000-11-04 11:38:01
 * Build:   v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose: Tuple extractor interface
 *------------------------------------------------------------------------*/

#ifndef _H_PG95_GETTUPLE
#define _H_PG95_GETTUPLE
#include <libpq-fe.h>
#include <rscheme/vinsns.h>

/*
 *  Note -- in Postgres7, dates are represented as +/- day offsets
 *  from 2000-01-01.  This structure remains in case we need to
 *  ressurect it [cr.691]
 */

typedef struct {
  unsigned char  day;
  unsigned char  month;
  unsigned short year;
} rspg_date;

typedef struct {
  short  hr;
  short  min;
  float  sec;
} rspg_time;

obj rspg_extract_tuple( PGresult *result,
			int tuple_num,
			obj gen_class,
			obj plan,
			obj proto,
			obj t_class );

#endif /* _H_PG95_GETTUPLE */
