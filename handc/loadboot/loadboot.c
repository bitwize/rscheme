/*-----------------------------------------------------------------*-C-*---
 * File:    handc/loadboot/loadboot.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.8
 * File mod date:    1998-01-13 11:38:09
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          load the boot image
 *------------------------------------------------------------------------*
 * Notes:
 *      the function here is designed to be replaced by alternate
 *      boot image loaders, like FASL
 *------------------------------------------------------------------------*/

#include <rscheme/smemory.h>
#include <rscheme/scheme.h>
#include <rscheme/heapi.h>

obj load_initial_heap( const char *path, rs_bool verbose )
{
char *gc_argv[3];
int vers;
obj r;

  gc_argv[0] = "rs";
  gc_argv[1] = verbose ? (char *)NULL : "-q";
  gc_argv[2] = NULL;
  init_gc( verbose ? 1 : 2, (const char **)gc_argv );
  
  /* make room for it... */
  
  gc_safe_point( 1024*1024 );
  r = load_image_file( path, FALSE_OBJ, FALSE_OBJ, &vers );

  if (EQ(r,FALSE_OBJ))
    return FALSE_OBJ;

  switch (vers)
    {
    case FMTV_RSCHEME_0_5:  /* assume it's bootable */
    case FMTV_RSCHEME_0_6_BOOT:
      return r;
    default:
      fprintf( stderr, "%s: image version %d -- not bootable\n",
	       path, vers );
      return FALSE_OBJ;
    }
}
