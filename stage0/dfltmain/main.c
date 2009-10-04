/*-----------------------------------------------------------------*-C-*---
 * File:    handc/dfltmain/main.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.11
 * File mod date:    1997-11-29 23:10:43
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Default 'main'-like functionality for RScheme shells
 *------------------------------------------------------------------------*/

#include <rscheme/api.h>
#include <rscheme/osglue.h>
#include <rscheme/rlseconf.h>
#include <rscheme/scheme.h>
#include <rscheme/linktype.h>
#include <string.h>
#include <ctype.h>

extern int bci_trace_flag;

/*
 *  returns a pointer to possibly static data, or returns NULL
 *  if the image can't be found
 */

const char *find_system_image( const char *arg0, const char *dflt );

int rscheme_std_main( int argc, const char **argv, 
		      struct module_descr **modules,
		      const char *default_image )
{
  obj start, args, rc;
  const char *system_file = NULL;
  int i = 1;
  rs_bool verbose = YES;
  rs_bool is_script = NO;

  init_dynamic_link( argv[0] ); /* some systems need this */

  for (i=1; i<argc && argv[i][0] == (char)'-'; i++)
    {
      if (strcmp(argv[i],"-image") == 0)
	{
	  if (i+1 >= argc)
	    goto miss_arg;
	  system_file = argv[++i];
	}
      else if (strcmp(argv[i],"--version") == 0)
	{
	  puts( RSCHEME_VERSION );
	  return 0;
	}
      else if (strcmp(argv[i],"--install") == 0)
	{
	  puts( rs_install_dir );
	  return 0;
	}
      else if (strcmp(argv[i],"-qimage") == 0)
	{
	  if (i+1 >= argc)
	    goto miss_arg;
	  system_file = argv[++i];
	  verbose = NO;
	}
      else if (strcmp(argv[i],"-bcitrace") == 0)
	{
	  if (bci_trace_flag >= 0)
	    bci_trace_flag = 1;
	  else
	    goto not_comp;
	}
      else if (strcmp(argv[i],"-stepdump") == 0)
	{
#ifdef STEP_DUMP
	  do_step_dump = 1;
#else
	  goto not_comp;
#endif
	}
      else if (strcmp(argv[i],"-q") == 0)
	{
	  verbose = NO;
	}
      else if (strcmp(argv[i],"-script") == 0)
	{
	  verbose = NO;
	  is_script = YES;
	}
#ifdef RECORD_CALL_CHAIN
      else if (strcmp(argv[i],"-abt") == 0)
	{
	  extern rs_bool do_record_call_chain;

	  do_record_call_chain = YES;
	}
#endif
      else
	break;
    }
  if (!system_file)
    system_file = find_system_image( argv[0], default_image );

  if (!system_file)
    {
      fprintf( stderr, "%s: could not find system image\n", argv[0] );
      return 1;			/* boot failed */
    }

  start = init_scheme( argc, argv, system_file, 
		      verbose,
		      modules );
  if (EQ(start,FALSE_OBJ))
    {
      fprintf( stderr, "%s: initialization from %s failed\n", 
	      argv[0], system_file );
      return 1;
    }

  args = NIL_OBJ;
  while (argc > i)
    args = cons( make_string( argv[--argc] ), args );

  rc = call_scheme( start, 3, 
		    args, 
		    rb_to_bo(verbose),
		    rb_to_bo(is_script) );

  if (truish(rc))
    return 0;
  return 1;

 miss_arg:
  fprintf( stderr, "missing arg to `%s'\n", argv[i] );
  return 2;
 not_comp:
  fprintf( stderr, "system not compiled to support `%s'\n", argv[i] );
  return 2;
}
