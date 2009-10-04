/*-----------------------------------------------------------------*-C-*---
 * File:    handc/lgh/lghmain.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.3
 * File mod date:    1997-11-29 23:10:51
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#include <rscheme/api.h>
#include <rscheme/osglue.h>
#include <rscheme/rlseconf.h>
#include <rscheme/scheme.h>
#include <rscheme/linktype.h>
#include <string.h>
#include <ctype.h>
#include <rscheme/api.h>
#include <rscheme/stdmodul.h>
#include <rscheme/rlseconf.h>

extern int bci_trace_flag;

struct module_descr *(std_modules[]) = { STD_MODULES_DECL };

obj eval_proc;

void lgh_startup( int packages )
{
  obj start, args, rc;
  const char *system_file = NULL;
  int i = 1;
  rs_bool verbose = YES;
  rs_bool is_script = NO;
  const char *(argv[2]);

  argv[0] = "rs";
  argv[1] = NULL;

  init_dynamic_link( argv[0] ); /* some systems need this */

  start = init_scheme( 1, argv, "sys.img", NO, std_modules );
  if (EQ(start,FALSE_OBJ))
    {
      fprintf( stderr, "initialization from sys.img failed\n" );
      exit(1);
    }

  rc = call_scheme( start, 3, NIL_OBJ, FALSE_OBJ, FALSE_OBJ );
  eval_proc = rc;
}

void lgh_eval( char *str )
{
  call_scheme( eval_proc, 1, make_string(str) );
}

