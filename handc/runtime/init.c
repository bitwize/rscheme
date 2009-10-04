/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/init.c
 *
 *          Copyright (C)1997,1998 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.22
 * File mod date:    2003-06-09 22:17:14
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          RScheme initialization code
 *------------------------------------------------------------------------*/

#include <stdio.h>
#include <signal.h>
#include <string.h>

#include <rscheme.h>
#include <rscheme/regs.h>
#include <rscheme/scheme.h>
#include <rscheme/osglue.h>
#include <rscheme/heapi.h>
#include <rscheme/rlseconf.h>
#include <rscheme/api.h>
#include "intrs.h"

/* the place where RScheme is installed.  Assigned in shell.c. */
char *rs_install_dir = NULL;

/*
 *  Go through the image we just loaded, find all
 *  the symbol objects, and use those symbols to initially populate
 *  the symbol table. 
 *  
 */

static void populate_symbol_table( void )
{
  obj tbl, allsym;
  UINT_32 i, n;

  tbl = symbol_table;
  allsym = all_instances( symbol_class );
  n = SIZEOF_PTR( allsym );

  for (i=0; i<n; i+=SLOT(1)) {
    obj h, k, v;

    v = gvec_ref( allsym, i );
    k = symbol_str( v );
    h = hash_string( k );
    
    hashtable_install( tbl, h, k, v );
  }
}

/* copy globals from the gvec given to globally known array */
static void
load_rscheme_globals(obj init_globals)
{
  unsigned i;
  /* the number of globals supplied by the boot image */
  unsigned n = SIZEOF_PTR(init_globals) / SLOT(1);

  if (n > NUM_RSCHEME_GLOBALS)
    {
      fprintf(stderr, "warning: %u initial globals supplied (max %u)\n",
	      n, NUM_RSCHEME_GLOBALS);
      n = NUM_RSCHEME_GLOBALS;
    }

  /* put globals in globally known array. Note that we skip copying
     SLOT(0) & SLOT(1). This is because rscheme_global[0] is "boot_image",
     which is to be a pointer to the actual image that was loaded, 
     and rscheme_global[1] is "boot_args", which will
     be initialized to the argv passed to init_scheme()  */
  for (i = 2; i < n; i++)
    rscheme_global[i] = gvec_read(init_globals, SLOT(i));

  /*
   *  If symbol_table (aka rscheme_global[2]) is empty,
   *  then go through the image we just loaded, find all
   *  the symbol objects, and initialize the symbol table. 
   *  
   *  We have to do this before calling rs_init_c_signals() or else
   *  there would be no way to make sure a symbol such as 'SIGINT that
   *  was in the image we loaded was eq? the same object constructed
   *  by rs_init_c_signals()
   *
   */
  if (hashtable_size( symbol_table ) == 0) {
    populate_symbol_table();
  }
}

static obj
make_string_list(int argc, const char *argv[])
{
  obj args = NIL_OBJ;
  while (argc > 0)
    args = cons(make_string(argv[--argc]), args);
  return args;
}

/* load boot image, assign globally known array of RScheme globals,
   compute a closure to be used as initial procedure, and return it */
obj
init_scheme(int argc, const char *argv[],
	    const char *boot_image_path,
	    rs_bool verbose, 
	    struct module_descr **module_tab)
{
  /* load registers from safe place */
  switch_hw_regs_into_scheme();
  init_regs();
  init_math();
  init_linkage(module_tab);
  init_runtim();
  
  /* initialize the GC as well as loading the initial image (two functions
     glommed together because somethimes they ARE the same operation,
     ie, the FASL loader maps the image which is both the initial heap
     and the initialized GC structure) */

  /* boot_image is #defined in scheme.h to an array element, 
     ie, rscheme_global[0] */
  boot_image = load_initial_heap(boot_image_path, verbose);
  /* since C doesn't have exceptions we return an error token.
     Caller must of course test it */
  if (EQ(boot_image,FALSE_OBJ))
    return FALSE_OBJ;
  
  /* so far, we don't actually have any type information; the
     well-known classes have been loaded, but we need to store
     them in the rscheme_globals[] array */
  load_rscheme_globals(gvec_read(boot_image, SLOT(0)));
  
  init_os();
  init_interrupts();
  init_math2();

  /* boot_args is #defined in scheme.h to be rscheme_global[1] */
  boot_args = make_string_list(argc, argv);

  install_dir = make_string(rs_install_dir ? rs_install_dir : "install");
  rs_init_c_signals();
  {
    obj start = gvec_read(boot_image, SLOT(2));
    /* caller might need regs, so switch them back */
    switch_hw_regs_back_to_os();
    return start;
  }
}
