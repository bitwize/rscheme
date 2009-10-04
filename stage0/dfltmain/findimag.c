/*-----------------------------------------------------------------*-C-*---
 * File:    handc/dfltmain/findimag.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.4
 * File mod date:    1997-11-29 23:10:43
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          function to locate system image
 *------------------------------------------------------------------------*/

#include <rscheme/osglue.h>
#include <rscheme/rlseconf.h>
#include <rscheme/scheme.h>
#include <rscheme/api.h>

#include <string.h>
#include <ctype.h>
#ifdef __NeXT__
#include <mach/mach.h>
#endif

const char *find_system_image( const char *arg0, const char *dflt );

/* try to locate the system image if it
   wasn't specified on the command line
*/

const char *find_system_image( const char *arg0, const char *dflt )
{
static char temp[1024];
char *p;
#ifdef __NeXT__
int n;

   if (getsectdata( "IMAGE", "system", &n ))
     {
       return "sect:IMAGE:system";
     }
#endif

  /* if the default is specified and exists, use it
   *
   * this is used by applications that supply their own
   * image
   */

  if (dflt && os_file_exists_p(dflt))
     return dflt;

  /* look in ~/lib/rs/$RSCHEME_VERSION/system.img  */

  if (os_getenv("HOME"))
    {
      strcpy( temp, os_getenv( "HOME" ) );
      strcat( temp, "/lib/rs/" RSCHEME_VERSION "/system.img" );
      if (os_file_exists_p(temp))
	return temp;
    }

  /* look in [install]/resource/system.img */

  strcpy( temp, rs_install_dir );
  strcat( temp, "/resource/system.img" );
  if (os_file_exists_p(temp))
    return temp;

  /*  If argv[0] is of the form foo/bin/blech
      then look for foo/resource/blech.img
  */

  if ((p = strrchr(arg0,'/')))
    {
      char *p2;

      strncpy( temp, arg0, p - arg0 );
      temp[p - arg0] = 0;

      p2 = strrchr( temp, '/' );
      if (p2)
	p2[1] = 0;

      strcat( temp, "resource/" );
      strcat( temp, p+1 );
      strcat( temp, ".img" );
    }
  else
    strcpy( temp, "../resource/system.img" );

  if (os_file_exists_p(temp))
    return temp;

  return NULL;
}
