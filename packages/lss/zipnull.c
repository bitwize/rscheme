#include <stdlib.h>
#include <rscheme/pkgs/lss/lsszips.h>

/* this is just a "gather" copy */

static void *null_zip( zip_algorithm *self, void *dest, zipbuf *src )
{
  while (src->ptr)
    {
      size_t n = src->limit - src->ptr;
      memcpy( dest, src->ptr, n );
      dest = (char *)dest + n;
      src++;
    }
  return dest;
}

/* this is just a "scatter" copy */

static void *null_unzip( zip_algorithm *self, zipbuf *dest, void *src )
{
  while (dest->ptr)
    {
      size_t n = dest->limit - dest->ptr;
      memcpy( dest->ptr, src, n );
      src = (char *)src + n;
      dest++;
    }
  return src;
}

/*  declare this algorithm to the configure script  */
/*| define-zip-algorithm null lss_null_zip |*/

zip_algorithm lss_null_zip = {
  "null",
  (void *)0,
  null_zip,
  null_unzip
};

/* utility method -- not really part of zipnull */

size_t zipbuf_len( zipbuf *vec )
{
  size_t n = 0;
  while (vec->ptr)
    {
      n += (char *)vec->limit - (char *)vec->ptr;
      vec++;
    }
  return n;
}

#include "ziplist.ci"

zip_algorithm *lss_find_zip_algorithm( const char *name )
{
  int i;
  for (i=0; master_table[i]; i++)
    {
      if (strcmp( master_table[i]->name, name ) == 0)
	{
	  return master_table[i];
	}
    }
  return NULL;
}
