/*------------------------------------------------------------------*-C-*-*
 * File:	packages/syscalls/syscallx.c
 * Version:	1.8
 * Date:	15:52:29
 * Build:	v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:	bytecode extensions to access generic system calls
 *------------------------------------------------------------------------*/

#ifdef INCL_SCCSID
char *F52RS_syscallx_sccsid__()
{ return "%W% 15:52:29"; }
#endif /* INCL_SCCSID */

#include <rscheme/scheme.h>
#include <rscheme/smemory.h>
#include <rscheme/bcextend.h>
#include <rscheme/osglue.h>
#include "systemh.h"
#include "scmtime.h"

/* #if..#endif taken from Autoconf documentation (edition 2.8), p.34 */

#if HAVE_DIRENT_H
# include <dirent.h>
# define NAMELEN(dirent) strlen((dirent)->d_name)
#else
# define dirent direct
# define NAMELIN(dirent) (dirent)->d_namlen
# if HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# if HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# if HAVE_NDIR_H
#  include <ndir.h>
# endif
#endif

/*
   The Syscalls BCI Extension
*/

#include "sysio.ci"

/*  portable scandir support  */

obj rs_scandir( const char *path )
{
  DIR *dir;
  struct dirent *ent;
  obj lst = NIL_OBJ;

  dir = opendir( path );
  if (!dir)
    {
      scheme_error( "scandir: opendir(~s) failed (errno ~d)",
		    2, make_string(path), int2fx(errno) );
    }

  while ((ent = readdir(dir)))
    {
      size_t len = NAMELEN(ent);
      obj str;

      str = bvec_alloc( len + 1, string_class ); /* leaves NUL byte */
      memcpy( string_text(str), ent->d_name, len );
      lst = cons( str, lst );
    }

  if (closedir(dir) < 0)
    {
      scheme_error( "scandir: closedir(~s) failed (errno ~d)",
		    2, make_string(path), int2fx(errno) );
    }
  return lst;
}
