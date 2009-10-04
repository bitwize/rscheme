#include "unixm_p.h"
#include <rscheme/intrs.h>
#include <rscheme/smemory.h>
#include <rscheme/osglue.h>

int rscheme_file_mode_to_os( int mode )
{
  int i_mode;
  i_mode = 0;

  if ((mode & 3) == 0)
    {
      i_mode = O_RDONLY;
    }
  else if ((mode & 3) == 1)
    {
      i_mode = O_WRONLY;
    }
  else if ((mode & 3) == 2)
    {
      i_mode = O_RDWR;
    }
  else
    {
      scheme_error( "file-mode->os: mode ~d is invalid", 1, int2fx(mode) );
    }

  if (mode & 4)        i_mode |= O_APPEND;
  if (mode & 8)        i_mode |= O_CREAT;
  if (mode & (1<<4))   i_mode |= O_EXCL;
  if (mode & (1<<5))   i_mode |= O_TRUNC;

  return i_mode;
}
